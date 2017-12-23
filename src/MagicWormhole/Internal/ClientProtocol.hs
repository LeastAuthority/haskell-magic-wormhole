{-# LANGUAGE TupleSections #-}
-- | Low-level details of talking to a Magic Wormhole peer.
--
-- For a user-facing interface, see "MagicWormhole.Internal.Peer".
module MagicWormhole.Internal.ClientProtocol
  ( Connection(..)
  , SessionKey(..)
  , Error(..)
  , sendEncrypted
  , receiveEncrypted
  , PlainText
  -- * Exported for testing
  , decrypt
  , encrypt
  , deriveKey
  , phasePurpose
  ) where

import Protolude hiding (phase)

import Crypto.Hash (SHA256(..), hashWith)
import qualified Crypto.KDF.HKDF as HKDF
import qualified Crypto.Saltine.Internal.ByteSizes as ByteSizes
import qualified Crypto.Saltine.Class as Saltine
import qualified Crypto.Saltine.Core.SecretBox as SecretBox
import qualified Data.ByteArray as ByteArray
import qualified Data.ByteString as ByteString

import qualified MagicWormhole.Internal.Messages as Messages

-- | A connection to a peer via the Rendezvous server.
--
-- Normally construct this with 'Rendezvous.open'.
data Connection
  = Connection
  { -- | The application ID for this connection.
    appID :: Messages.AppID
    -- | The identifier for this side of the connection.
  , ourSide :: Messages.Side
    -- | Send a message to the other side.
  , send :: Messages.Phase -> Messages.Body -> IO ()
    -- | Receive a message from the other side.
  , receive :: STM Messages.MailboxMessage
  }

-- | SPAKE2 key used for the duration of a Magic Wormhole peer-to-peer connection.
--
-- Individual messages will be encrypted using 'encrypt' ('decrypt'), which
-- must be given a key that's /generated/ from this one (see 'deriveKey' and
-- 'derivePhaseKey').
newtype SessionKey = SessionKey ByteString

sendEncrypted :: Connection -> SessionKey -> Messages.Phase -> PlainText -> IO ()
sendEncrypted conn key phase plaintext = do
  ciphertext <- encrypt derivedKey plaintext
  send conn phase (Messages.Body ciphertext)
  where
    derivedKey = deriveKey key (phasePurpose (ourSide conn) phase)

receiveEncrypted :: Connection -> SessionKey -> STM (Either Error (Messages.Phase, PlainText))
receiveEncrypted conn key = do
  message <- receive conn
  let Messages.Body ciphertext = Messages.body message
  pure $ (Messages.phase message,) <$> decrypt (derivedKey message) ciphertext
  where
    derivedKey msg = deriveKey key (phasePurpose (Messages.side msg) (Messages.phase msg))

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

-- XXX: Different types for ciphertext and plaintext please!
type PlainText = ByteString
type CipherText = ByteString

-- | The purpose of a message. 'deriveKey' combines this with the 'SessionKey'
-- to make a unique 'SecretBox.Key'. Do not re-use a 'Purpose' to send more
-- than message.
type Purpose = ByteString

-- | Derive a one-off key from the SPAKE2 'SessionKey'. Use this key only once.
deriveKey :: SessionKey -> Purpose -> SecretBox.Key
deriveKey (SessionKey key) purpose =
  fromMaybe (panic "Could not encode to SecretBox key") $ -- Impossible. We guarntee it's the right size.
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

data Error
  = CouldNotDecrypt ByteString
  | InvalidNonce ByteString
  | MessageOutOfOrder Messages.Phase PlainText
  deriving (Eq, Show)
