{-# OPTIONS_HADDOCK not-home #-}
{-# LANGUAGE TupleSections #-}
-- |
-- Description : Low-level details for talking to a Magic Wormhole peer.
--
-- Low-level details for talking to a Magic Wormhole peer.
--
-- For a user-facing interface, see "MagicWormhole.Internal.Peer".
module MagicWormhole.Internal.ClientProtocol
  ( Connection(..)
  , SessionKey(..)
  , PeerError(..)
  , sendEncrypted
  , receiveEncrypted
  , PlainText(..)
  -- * Exported for testing
  , CipherText(..)
  , decrypt
  , encrypt
  , deriveKey
  , Purpose
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
-- Normally construct this with 'MagicWormhole.open'.
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
-- You can obtain a 'SessionKey' using 'MagicWormhole.Internal.Pake.pakeExchange'.
--
-- Individual messages will be encrypted using 'encrypt' ('decrypt'), which
-- must be given a key that's /generated/ from this one (see 'deriveKey').
newtype SessionKey = SessionKey ByteString

-- | Send an encrypted message to the peer.
sendEncrypted
  :: Connection -- ^ Connection to the peer
  -> SessionKey -- ^ The key established for this session
  -> Messages.Phase -- ^ Phase of the protocol this message represents
  -> PlainText -- ^ Content of the message
  -> IO ()
sendEncrypted conn key phase plaintext = do
  encryptedBody <- encryptMessage conn key phase plaintext
  send conn phase encryptedBody

-- | Pull a message from the peer and decrypt it. If the message fails to
-- decrypt, an exception will be thrown, aborting the transaction and leaving
-- the message on the queue.
receiveEncrypted
  :: Connection -- ^ Connection to the peer
  -> SessionKey -- ^ The key established for this session
  -> STM (Messages.Phase, PlainText)  -- ^ The phase and content of the message we received
receiveEncrypted conn key = do
  message <- receive conn
  either throwSTM pure $ decryptMessage key message

-- | Encrypt a mailbox message, deriving the key from the phase.
encryptMessage :: Connection -> SessionKey -> Messages.Phase -> PlainText -> IO Messages.Body
encryptMessage conn key phase plaintext = Messages.Body . unCipherText <$> encrypt derivedKey plaintext
  where
    derivedKey = deriveKey key (phasePurpose (ourSide conn) phase)

-- | Encrypt a message using 'SecretBox'. Get the key from 'deriveKey'.
-- Decrypt with 'decrypt'.
encrypt :: SecretBox.Key -> PlainText -> IO CipherText
encrypt key (PlainText message) = do
  nonce <- SecretBox.newNonce
  let ciphertext = SecretBox.secretbox key nonce message
  pure . CipherText $ Saltine.encode nonce <> ciphertext

-- | Decrypt a 'MailboxMessage' using 'SecretBox'. Derives the key from the phase.
decryptMessage :: SessionKey -> Messages.MailboxMessage -> Either PeerError (Messages.Phase, PlainText)
decryptMessage key message =
  let Messages.Body ciphertext = Messages.body message
  in (Messages.phase message,) <$> decrypt (derivedKey message) (CipherText ciphertext)
  where
    derivedKey msg = deriveKey key (phasePurpose (Messages.side msg) (Messages.phase msg))

-- | Decrypt a message using 'SecretBox'. Get the key from 'deriveKey'.
-- Encrypted using 'encrypt'.
decrypt :: SecretBox.Key -> CipherText -> Either PeerError PlainText
decrypt key (CipherText ciphertext) = do
  let (nonce', ciphertext') = ByteString.splitAt ByteSizes.secretBoxNonce ciphertext
  nonce <- note (InvalidNonce nonce') $ Saltine.decode nonce'
  note (CouldNotDecrypt ciphertext') $ PlainText <$> SecretBox.secretboxOpen key nonce ciphertext'

-- | Unencrypted text.
newtype PlainText = PlainText { unPlainText :: ByteString } deriving (Eq, Ord, Show)

-- | Encrypted text.
newtype CipherText = CipherText { unCipherText :: ByteString } deriving (Eq, Ord, Show)

-- | The purpose of a message. 'deriveKey' combines this with the 'SessionKey'
-- to make a unique 'SecretBox.Key'. Do not re-use a 'Purpose' to send more
-- than message.
type Purpose = ByteString

-- | Derive a one-off key from the SPAKE2 'SessionKey'. Use this key only once.
deriveKey
  :: SessionKey -- ^ Key established for this session
  -> Purpose -- ^ What this key is for. Normally created using 'phasePurpose'.
  -> SecretBox.Key  -- ^ A key to use once to send or receive a message
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

-- | Something that went wrong with the client protocol.
data PeerError
  -- | We received a message from the other side that we could not decrypt
  = CouldNotDecrypt ByteString
  -- | We could not determine the SecretBox nonce from the message we received
  | InvalidNonce ByteString
  -- | We received a message for a phase that we have already received a message for.
  | MessageOutOfOrder Messages.Phase PlainText
  deriving (Eq, Show, Typeable)

instance Exception PeerError
