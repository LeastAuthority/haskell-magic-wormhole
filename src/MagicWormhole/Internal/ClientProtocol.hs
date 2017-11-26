{-# LANGUAGE TupleSections #-}
-- | The peer-to-peer aspects of the Magic Wormhole protocol.
--
-- Described as the "client" protocol in the Magic Wormhole documentation.
module MagicWormhole.Internal.ClientProtocol
  ( versionExchange
  , Error
  , sendEncrypted
  , receiveEncrypted
  , PlainText
  -- * Talking to peers
  , EncryptedConnection
  , withEncryptedConnection
  , sendMessage
  , receiveMessage
  -- * Exported for testing
  , decrypt
  , encrypt
  , deriveKey
  , Versions(..)
  , phasePurpose
  ) where

import Protolude hiding (phase)

import Control.Concurrent.STM.TVar
  ( TVar
  , modifyTVar'
  , newTVar
  , readTVar
  )
import Crypto.Hash (SHA256(..), hashWith)
import qualified Crypto.KDF.HKDF as HKDF
import qualified Crypto.Saltine.Internal.ByteSizes as ByteSizes
import qualified Crypto.Saltine.Class as Saltine
import qualified Crypto.Saltine.Core.SecretBox as SecretBox
import qualified Crypto.Spake2 as Spake2
import Data.Aeson (FromJSON, ToJSON, (.=), object, Value(..), (.:))
import Data.Aeson.Types (typeMismatch)
import qualified Data.Aeson as Aeson
import qualified Data.ByteArray as ByteArray
import qualified Data.ByteString as ByteString
import Data.String (String)

import qualified MagicWormhole.Internal.Messages as Messages
import qualified MagicWormhole.Internal.Pake as Pake
import qualified MagicWormhole.Internal.Peer as Peer
import qualified MagicWormhole.Internal.Sequential as Sequential


-- XXX: Lots of duplicated code sending JSON data. Either make a typeclass for
-- this sort of thing or at least sendJSON, receiveJSON.

-- TODO: I've been playing fast and loose with Text -> ByteString conversions
-- (grep for `toS`) for the details. The Python implementation occasionally
-- encodes as `ascii` rather than the expected `UTF-8`, so I need to be a bit
-- more careful.

-- | Exchange version information with a Magic Wormhole peer.
--
-- Obtain the 'SessionKey' from 'pakeExchange'.
versionExchange :: Peer.Connection -> Pake.SessionKey -> IO (Either Error Versions)
versionExchange conn key = do
  (_, theirVersions) <- concurrently sendVersion (atomically receiveVersion)
  pure $ case theirVersions of
    Left err -> Left err
    Right theirs
      | theirs /= Versions -> Left VersionMismatch
      | otherwise -> Right Versions
  where
    sendVersion = sendEncrypted conn key Messages.VersionPhase (toS (Aeson.encode Versions))
    receiveVersion = runExceptT $ do
      (phase, plaintext) <- ExceptT $ receiveEncrypted conn key
      lift $ unless (phase == Messages.VersionPhase) retry
      ExceptT $ pure $ first ParseError (Aeson.eitherDecode (toS plaintext))

-- | Establish an encrypted connection between peers.
--
-- Use this connection with 'withEncryptedConnection'.
establishEncryption :: Peer.Connection -> Spake2.Password -> IO (Either Error EncryptedConnection)
establishEncryption peer password = runExceptT $ do
  key <- ExceptT $ first ProtocolError <$> Pake.pakeExchange peer password
  void $ ExceptT $ versionExchange peer key
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
  ExceptT $ runEncryptedConnection conn (action conn)

-- NOTE: Versions
-- ~~~~~~~~~~~~~~
--
-- Magic Wormhole Python implementation sends the following as the 'version' phase:
--     {"app_versions": {}}
--
-- The idea is that /some time in the future/ this will be used to indicate
-- capabilities of peers. At present, it is unused, save as a confirmation
-- that the SPAKE2 exchange worked.

data Versions = Versions deriving (Eq, Show)

instance ToJSON Versions where
  toJSON _ = object ["app_versions" .= object []]

instance FromJSON Versions where
  parseJSON (Object v) = do
    -- Make sure there's an object in the "app_versions" key and abort if not.
    (Object _versions) <- v .: "app_versions"
    pure Versions
  parseJSON unknown = typeMismatch "Versions" unknown


sendEncrypted :: Peer.Connection -> Pake.SessionKey -> Messages.Phase -> PlainText -> IO ()
sendEncrypted conn key phase plaintext = do
  ciphertext <- encrypt derivedKey plaintext
  Peer.send conn phase (Messages.Body ciphertext)
  where
    derivedKey = deriveKey key (phasePurpose (Peer.ourSide conn) phase)

receiveEncrypted :: Peer.Connection -> Pake.SessionKey -> STM (Either Error (Messages.Phase, PlainText))
receiveEncrypted conn key = do
  message <- Peer.receive conn
  let Messages.Body ciphertext = Messages.body message
  pure $ (Messages.phase message,) <$> decrypt (derivedKey message) ciphertext
  where
    derivedKey msg = deriveKey key (phasePurpose (Messages.side msg) (Messages.phase msg))


-- | The purpose of a message. 'deriveKey' combines this with the 'SessionKey'
-- to make a unique 'SecretBox.Key'. Do not re-use a 'Purpose' to send more
-- than one message.
type Purpose = ByteString

-- | Derive a one-off key from the SPAKE2 'SessionKey'. Use this key only once.
deriveKey :: Pake.SessionKey -> Purpose -> SecretBox.Key
deriveKey (Pake.SessionKey key) purpose =
  fromMaybe (panic "Could not encode to SecretBox key") $ -- Impossible. We guarantee it's the right size.
    Saltine.decode (HKDF.expand (HKDF.extract salt key :: HKDF.PRK SHA256) purpose keySize)
  where
    salt = "" :: ByteString
    keySize = ByteSizes.secretBoxKey

-- | Obtain a 'Purpose' for deriving a key to send a message that's part of a
-- peer-to-peer communication.
phasePurpose :: Messages.Side -> Messages.Phase -> Purpose
phasePurpose (Messages.Side side) phase = "wormhole:phase:" <> sideHashDigest <> phaseHashDigest
  where
    sideHashDigest = hashDigest (toS @Text @ByteString side)
    phaseHashDigest = hashDigest (toS (Messages.phaseName phase) :: ByteString)
    hashDigest thing = ByteArray.convert (hashWith SHA256 thing)

-- XXX: Different types for ciphertext and plaintext please!
type CipherText = ByteString
type PlainText = ByteString

-- | Encrypt a message using 'SecretBox'. Get the key from 'deriveKey'.
-- Decrypt with 'decrypt'.
encrypt :: SecretBox.Key -> PlainText -> IO CipherText
encrypt key message = do
  nonce <- SecretBox.newNonce
  let ciphertext = SecretBox.secretbox key nonce message
  pure $ Saltine.encode nonce <> ciphertext

-- | Decrypt a message using 'SecretBox'. Get the key from 'deriveKey'.
-- Encrypted using 'encrypt'.
decrypt :: SecretBox.Key -> CipherText -> Either Error PlainText
decrypt key ciphertext = do
  let (nonce', ciphertext') = ByteString.splitAt ByteSizes.secretBoxNonce ciphertext
  nonce <- note (InvalidNonce nonce') $ Saltine.decode nonce'
  note (CouldNotDecrypt ciphertext') $ SecretBox.secretboxOpen key nonce ciphertext'


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
  , sharedKey :: Pake.SessionKey
  , inbound :: Sequential.Sequential Int (Messages.Phase, PlainText)
  , outbound :: TVar Int
  }

-- | Construct a new encrypted connection.
newEncryptedConnection :: Peer.Connection -> Pake.SessionKey -> STM EncryptedConnection
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
  -> IO (Either Error a)
runEncryptedConnection conn action = do
  result <- race readLoop action
  pure $ case result of
           Left (Left readErr) -> Left readErr
           Left (Right _) -> panic "Cannot happen"
           Right r -> Right r
  where
    readLoop = do
      msg <- atomically $ receiveEncrypted (connection conn) (sharedKey conn)
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
sendMessage :: EncryptedConnection -> PlainText -> IO ()
sendMessage conn body = do
  i <- atomically bumpPhase
  sendEncrypted (connection conn) (sharedKey conn) (Messages.ApplicationPhase i) body
  where
    bumpPhase = do
      i <- readTVar (outbound conn)
      modifyTVar' (outbound conn) (+1)
      pure i

-- | Receive a decrypted message from the peer.
--
-- Obtain an 'EncryptedConnection' with 'withEncryptedConnection'.
receiveMessage :: EncryptedConnection -> STM PlainText
receiveMessage conn = snd <$> Sequential.next (inbound conn)


data Error
  = ParseError String
  | ProtocolError (Spake2.MessageError Text)
  | CouldNotDecrypt ByteString
  | VersionMismatch
  | InvalidNonce ByteString
  deriving (Eq, Show)
