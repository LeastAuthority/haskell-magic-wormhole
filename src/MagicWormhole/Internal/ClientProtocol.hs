{-# LANGUAGE TupleSections #-}
-- | The peer-to-peer aspects of the Magic Wormhole protocol.
--
-- Described as the "client" protocol in the Magic Wormhole documentation.
module MagicWormhole.Internal.ClientProtocol
  ( Error(..)
  , EncryptedConnection
  , withEncryptedConnection
  , sendMessage
  , receiveMessage
  ) where

import Protolude hiding (phase)

import Control.Concurrent.STM.TVar
  ( TVar
  , modifyTVar'
  , newTVar
  , readTVar
  )
import qualified Crypto.Spake2 as Spake2
import Data.String (String)

import qualified MagicWormhole.Internal.Messages as Messages
import qualified MagicWormhole.Internal.Pake as Pake
import qualified MagicWormhole.Internal.Peer as Peer
import qualified MagicWormhole.Internal.Sequential as Sequential
import qualified MagicWormhole.Internal.Versions as Versions


-- XXX: Lots of duplicated code sending JSON data. Either make a typeclass for
-- this sort of thing or at least sendJSON, receiveJSON.

-- TODO: I've been playing fast and loose with Text -> ByteString conversions
-- (grep for `toS`) for the details. The Python implementation occasionally
-- encodes as `ascii` rather than the expected `UTF-8`, so I need to be a bit
-- more careful.

-- | Establish an encrypted connection between peers.
--
-- Use this connection with 'withEncryptedConnection'.
establishEncryption :: Peer.Connection -> Spake2.Password -> IO (Either Error EncryptedConnection)
establishEncryption peer password = runExceptT $ do
  key <- ExceptT $ first ProtocolError <$> Pake.pakeExchange peer password
  void $ ExceptT $ first VersionsError <$> Versions.versionExchange peer key
  conn <- liftIO $ atomically $ newEncryptedConnection peer key
  pure conn

-- | Run an action that communicates with a Magic Wormhole peer through an
-- encrypted connection.
--
-- Does the "pake" and "version" exchanges necessary to negotiate an encrypted
-- connection and then runs the user-provided action. This action can then use
-- 'sendMessage' and 'receiveMessage' to send & receive messages from its peer.
withEncryptedConnection
  :: Peer.Connection  -- ^ Underlying to a peer. Get this with 'Rendezvous.open'.
  -> Spake2.Password  -- ^ The shared password that is the basis of the encryption. Construct with 'Spake2.makePassword'.
  -> (EncryptedConnection -> IO a)  -- ^ Action to perform with the encrypted connection.
  -> IO (Either Error a)  -- ^ The result of the action, or some sort of protocol-level error.
withEncryptedConnection peer password action = runExceptT $ do
  conn <- ExceptT $ establishEncryption peer password
  ExceptT $ first CryptoError <$> runEncryptedConnection conn (action conn)


-- | A Magic Wormhole peer-to-peer application session.
--
-- You get one of these after you have found a peer, successfully negotatiated
-- a shared key, and verified that negotiation by exchanging versions. (Note
-- that this does not include the "verifying" step mentioned in
-- magic-wormhole's documentation, which is about a human being verifying the
-- correctness of the code).
--
-- All messages in this session, sent & received, are encrypted using keys
-- derived from this shared key.
data EncryptedConnection
  = EncryptedConnection
  { connection :: Peer.Connection
  , sharedKey :: Peer.SessionKey
  , inbound :: Sequential.Sequential Int (Messages.Phase, Peer.PlainText)
  , outbound :: TVar Int
  }

-- | Construct a new encrypted connection.
newEncryptedConnection :: Peer.Connection -> Peer.SessionKey -> STM EncryptedConnection
newEncryptedConnection conn sessionKey = EncryptedConnection conn sessionKey <$> Sequential.sequenceBy getAppRank firstPhase <*> newTVar firstPhase
  where
    getAppRank (phase, _) =
      case phase of
        Messages.PakePhase -> panic "Did not expect PakePhase. Expected application phase."
        Messages.VersionPhase -> panic "Did not expect VersionPhase. Expected application phase."
        (Messages.ApplicationPhase n) -> n

    -- | The rank of the first phase we expect to send, and the first phase we
    -- expect to receive. It is critically important that this number is
    -- agreed on between peers, otherwise, a peer will wait forever for, say,
    -- message 0, which the other side has cheerily sent as message 1.
    firstPhase = 0

-- | Take a successfully negotiated peer connection and run an action that
-- sends and receives encrypted messages.
--
-- Establish an encrypted connection using 'establishEncryption'.
--
-- Use this to communicate with a Magic Wormhole peer.
--
-- Once you have the session, use 'sendMessage' to send encrypted messages to
-- the peer, and 'receiveMessage' to received decrypted messages.
runEncryptedConnection
  :: EncryptedConnection
  -> IO a
  -> IO (Either Peer.Error a)
runEncryptedConnection conn action = do
  result <- race readLoop action
  pure $ case result of
           Left (Left readErr) -> Left readErr
           Left (Right _) -> panic "Cannot happen"
           Right r -> Right r
  where
    readLoop = do
      msg <- atomically $ Peer.receiveEncrypted (connection conn) (sharedKey conn)
      case msg of
        Left err -> pure $ Left err
        Right msg' -> do
          atomically $ Sequential.insert (inbound conn) msg'
          readLoop

-- | Send an encrypted message to the peer.
--
-- Obtain an 'EncryptedConnection' with 'withEncryptedConnection'.
--
-- The message will be encrypted using a one-off key deriving from the shared
-- key.
sendMessage :: EncryptedConnection -> Peer.PlainText -> IO ()
sendMessage conn body = do
  i <- atomically bumpPhase
  Peer.sendEncrypted (connection conn) (sharedKey conn) (Messages.ApplicationPhase i) body
  where
    bumpPhase = do
      i <- readTVar (outbound conn)
      modifyTVar' (outbound conn) (+1)
      pure i

-- | Receive a decrypted message from the peer.
--
-- Obtain an 'EncryptedConnection' with 'withEncryptedConnection'.
receiveMessage :: EncryptedConnection -> STM Peer.PlainText
receiveMessage conn = snd <$> Sequential.next (inbound conn)


data Error
  = ParseError String
  | ProtocolError (Spake2.MessageError Text)
  | VersionsError Versions.Error
  | CryptoError Peer.Error
  deriving (Eq, Show)
