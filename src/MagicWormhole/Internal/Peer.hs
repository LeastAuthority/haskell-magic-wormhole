{-# OPTIONS_HADDOCK not-home #-}
-- | Interface for communicating with a Magic Wormhole peer.
--
-- Build on this to write an application that uses Magic Wormhole.
module MagicWormhole.Internal.Peer
  ( EncryptedConnection
  , withEncryptedConnection
  , sendMessage
  , receiveMessage
  , deriveKey
  ) where

import Protolude hiding (phase)

import Control.Concurrent.STM.TVar
  ( TVar
  , modifyTVar'
  , newTVar
  , readTVar
  )
import qualified Crypto.Saltine.Core.SecretBox as SecretBox
import qualified Crypto.Spake2 as Spake2

import qualified MagicWormhole.Internal.ClientProtocol as ClientProtocol
import qualified MagicWormhole.Internal.Messages as Messages
import qualified MagicWormhole.Internal.Pake as Pake
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
establishEncryption :: ClientProtocol.Connection -> Spake2.Password -> IO EncryptedConnection
establishEncryption peer password = do
  key <- Pake.pakeExchange peer password
  void $ Versions.versionExchange peer key
  liftIO $ atomically $ newEncryptedConnection peer key

-- | Run an action that communicates with a Magic Wormhole peer through an
-- encrypted connection.
--
-- Does the "pake" and "version" exchanges necessary to negotiate an encrypted
-- connection and then runs the user-provided action. This action can then use
-- 'sendMessage' and 'receiveMessage' to send & receive messages from its peer.
--
-- Can throw:
--
--   * 'ClientProtocol.PeerError', when we receive nonsensical data from the other peer
--   * 'Pake.PakeError', when SPAKE2 cryptography fails
--   * 'Versions.VersionsError', when we cannot agree on shared capabilities (this can sometimes imply SPAKE2 cryptography failure)
withEncryptedConnection
  :: ClientProtocol.Connection  -- ^ Underlying to a peer. Get this with 'Rendezvous.open'.
  -> Spake2.Password  -- ^ The shared password that is the basis of the encryption. Construct with 'Spake2.makePassword'.
  -> (EncryptedConnection -> IO a)  -- ^ Action to perform with the encrypted connection.
  -> IO a  -- ^ The result of the action
withEncryptedConnection peer password action = do
  conn <- establishEncryption peer password
  runEncryptedConnection conn (action conn)

-- | A Magic Wormhole peer-to-peer application session.
--
-- Construct one of these using 'withEncryptedConnection'.
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
  { connection :: ClientProtocol.Connection
  , sharedKey :: ClientProtocol.SessionKey
  , inbound :: Sequential.Sequential Int (Messages.Phase, ClientProtocol.PlainText)
  , outbound :: TVar Int
  }

-- | Construct a new encrypted connection.
newEncryptedConnection :: ClientProtocol.Connection -> ClientProtocol.SessionKey -> STM EncryptedConnection
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
-- Establish an encrypted connection using 'withEncryptedConnection'.
--
-- Use this to communicate with a Magic Wormhole peer.
--
-- Once you have the session, use 'sendMessage' to send encrypted messages to
-- the peer, and 'receiveMessage' to received decrypted messages.
runEncryptedConnection
  :: EncryptedConnection
  -> IO a
  -> IO a
runEncryptedConnection conn action = do
  result <- race readLoop action
  pure $ case result of
           Left _ -> panic "Cannot happen"
           Right r -> r
  where
    readLoop = forever $ do
      msg <- atomically $ ClientProtocol.receiveEncrypted (connection conn) (sharedKey conn)
      inserted <- atomically $ Sequential.insert (inbound conn) msg
      unless inserted $ throwIO (uncurry ClientProtocol.MessageOutOfOrder msg)

-- | Send an encrypted message to the peer.
--
-- Obtain an 'EncryptedConnection' with 'withEncryptedConnection'.
--
-- The message will be encrypted using a one-off key deriving from the shared
-- key.
sendMessage :: EncryptedConnection -> ClientProtocol.PlainText -> IO ()
sendMessage conn body = do
  i <- atomically bumpPhase
  ClientProtocol.sendEncrypted (connection conn) (sharedKey conn) (Messages.ApplicationPhase i) body
  where
    bumpPhase = do
      i <- readTVar (outbound conn)
      modifyTVar' (outbound conn) (+1)
      pure i

-- | Receive a decrypted message from the peer.
--
-- Obtain an 'EncryptedConnection' with 'withEncryptedConnection'.
receiveMessage :: EncryptedConnection -> STM ClientProtocol.PlainText
receiveMessage conn = snd <$> Sequential.next (inbound conn)


-- | Derive a new key for the given purpose
--
-- Construct a new key from the existing session key for the given purpose
deriveKey :: EncryptedConnection -> ClientProtocol.Purpose -> SecretBox.Key
deriveKey conn = ClientProtocol.deriveKey (sharedKey conn)
