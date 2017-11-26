{-# LANGUAGE TupleSections #-}
-- | The peer-to-peer aspects of the Magic Wormhole protocol.
--
-- Described as the "client" protocol in the Magic Wormhole documentation.
module MagicWormhole.Internal.ClientProtocol
  ( pakeExchange
  , versionExchange
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
  , SessionKey(..) -- XXX: Figure out how to avoid exporting the constructors.
  , Versions(..)
  , phasePurpose
  , spakeBytesToMessageBody
  , messageBodyToSpakeBytes
  ) where

import Protolude hiding (phase)

import Control.Concurrent.STM.TVar
  ( TVar
  , modifyTVar'
  , newTVar
  , readTVar
  )
import Control.Monad (fail)
import Crypto.Hash (SHA256(..), hashWith)
import qualified Crypto.KDF.HKDF as HKDF
import qualified Crypto.Saltine.Internal.ByteSizes as ByteSizes
import qualified Crypto.Saltine.Class as Saltine
import qualified Crypto.Saltine.Core.SecretBox as SecretBox
import qualified Crypto.Spake2 as Spake2
import Crypto.Spake2.Group (Group(arbitraryElement))
import Crypto.Spake2.Groups (Ed25519(..))
import Data.Aeson (FromJSON, ToJSON, (.=), object, Value(..), (.:))
import Data.Aeson.Types (typeMismatch)
import qualified Data.Aeson as Aeson
import qualified Data.ByteArray as ByteArray
import Data.ByteArray.Encoding (convertToBase, convertFromBase, Base(Base16))
import qualified Data.ByteString as ByteString
import Data.String (String)

import qualified MagicWormhole.Internal.Messages as Messages
import qualified MagicWormhole.Internal.Peer as Peer
import qualified MagicWormhole.Internal.Sequential as Sequential


-- | The version of the SPAKE2 protocol used by Magic Wormhole.
type Spake2Protocol = Spake2.Protocol Ed25519 SHA256

newtype Spake2Message = Spake2Message { spake2Bytes :: ByteString } deriving (Eq, Show)

instance ToJSON Spake2Message where
  toJSON (Spake2Message msg) = object [ "pake_v1" .= toS @ByteString @Text (convertToBase Base16 msg) ]

instance FromJSON Spake2Message where
  parseJSON (Object msg) = do
    hexKey <- toS @Text @ByteString <$> msg .: "pake_v1"
    case convertFromBase Base16 hexKey of
      Left err -> fail err
      Right key -> pure $ Spake2Message key
  parseJSON unknown = typeMismatch "Spake2Message" unknown

spakeBytesToMessageBody :: ByteString -> Messages.Body
spakeBytesToMessageBody = Messages.Body . toS . Aeson.encode . Spake2Message

messageBodyToSpakeBytes :: Messages.Body -> Either Text ByteString
messageBodyToSpakeBytes (Messages.Body bodyBytes) =
  bimap toS spake2Bytes . Aeson.eitherDecode . toS $ bodyBytes

-- | Construct a SPAKE2 protocol compatible with Magic Wormhole.
wormholeSpakeProtocol :: Messages.AppID -> Spake2Protocol
wormholeSpakeProtocol (Messages.AppID appID') =
  Spake2.makeSymmetricProtocol SHA256 Ed25519 blind sideID
  where
    blind = arbitraryElement Ed25519 ("symmetric" :: ByteString)
    sideID = Spake2.SideID (toS appID')


-- XXX: Lots of duplicated code sending JSON data. Either make a typeclass for
-- this sort of thing or at least sendJSON, receiveJSON.

-- TODO: I've been playing fast and loose with Text -> ByteString conversions
-- (grep for `toS`) for the details. The Python implementation occasionally
-- encodes as `ascii` rather than the expected `UTF-8`, so I need to be a bit
-- more careful.

-- | SPAKE2 key used for the duration of a Magic Wormhole peer-to-peer connection.
--
-- Individual messages will be encrypted using 'encrypt' ('decrypt'), which
-- must be given a key that's /generated/ from this one (see 'deriveKey' and
-- 'derivePhaseKey').
newtype SessionKey = SessionKey ByteString

-- | Exchange SPAKE2 keys with a Magic Wormhole peer.
pakeExchange :: Peer.Connection -> Spake2.Password -> IO (Either Error SessionKey)
pakeExchange conn password = do
  let protocol = wormholeSpakeProtocol (Peer.appID conn)
  bimap ProtocolError SessionKey <$> Spake2.spake2Exchange protocol password sendPakeMessage (atomically receivePakeMessage)
  where
    sendPakeMessage = Peer.send conn Messages.PakePhase . spakeBytesToMessageBody
    receivePakeMessage  = do
      -- XXX: This is kind of a fun approach, but it means that everyone else
      -- has to promise that they *don't* consume pake messages.
      msg <- Peer.receive conn
      unless (Messages.phase msg == Messages.PakePhase) retry
      pure $ messageBodyToSpakeBytes (Messages.body msg)

-- | Exchange version information with a Magic Wormhole peer.
--
-- Obtain the 'SessionKey' from 'pakeExchange'.
versionExchange :: Peer.Connection -> SessionKey -> IO (Either Error Versions)
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


sendEncrypted :: Peer.Connection -> SessionKey -> Messages.Phase -> PlainText -> IO ()
sendEncrypted conn key phase plaintext = do
  ciphertext <- encrypt derivedKey plaintext
  Peer.send conn phase (Messages.Body ciphertext)
  where
    derivedKey = deriveKey key (phasePurpose (Peer.ourSide conn) phase)

receiveEncrypted :: Peer.Connection -> SessionKey -> STM (Either Error (Messages.Phase, PlainText))
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
deriveKey :: SessionKey -> Purpose -> SecretBox.Key
deriveKey (SessionKey key) purpose =
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
  , sharedKey :: SessionKey
  , inbound :: Sequential.Sequential Int (Messages.Phase, PlainText)
  , outbound :: TVar Int
  }

-- | Take a successfully negotiated peer connection and run an action with an
-- active session, returning the result of the action.
--
-- Use this to communicate with a Magic Wormhole peer.
--
-- Once you have the session, use 'sendMessage' to send encrypted messages to
-- the peer, and 'receiveMessage' to received decrypted messages.
withEncryptedConnection
  :: Peer.Connection  -- ^ A connection to the other peer.
  -> SessionKey  -- ^ A successfully negotiated session key
  -> (EncryptedConnection -> IO a)  -- ^ The action to perform
  -> IO (Either Error a)  -- ^ The result of the action
withEncryptedConnection conn sessionKey action = do
  conn' <- atomically $ newEncryptedConnection conn sessionKey
  runEncryptedConnection conn' action

-- | Construct a new session.
newEncryptedConnection :: Peer.Connection -> SessionKey -> STM EncryptedConnection
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

-- | Run an action inside a session.
--
-- Ensures that we are continually receiving messages from the peer, and
-- buffering them so the application /using/ the session can receive them in
-- order.
runEncryptedConnection :: EncryptedConnection -> (EncryptedConnection -> IO a) -> IO (Either Error a)
runEncryptedConnection conn action = do
  result <- race readLoop (action conn)
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
