-- | The peer-to-peer aspects of the Magic Wormhole protocol.
--
-- Described as the "client" protocol in the Magic Wormhole documentation.
module MagicWormhole.Internal.Peer
  ( pakeExchange
  , Error
  -- * Exported for testing
  , wormholeSpakeProtocol
  , decodeElement
  , encodeElement
  ) where

import Protolude hiding (phase)

import Control.Monad (fail)
import Crypto.Hash (SHA256(..))
import qualified Crypto.Spake2 as Spake2
import Crypto.Spake2.Group (Group(Element, arbitraryElement))
import Crypto.Spake2.Groups (Ed25519(..))
import Data.Aeson (FromJSON, ToJSON, (.=), object, Value(..), (.:))
import Data.Aeson.Types (typeMismatch)
import qualified Data.Aeson as Aeson
import Data.ByteArray.Encoding (convertToBase, convertFromBase, Base(Base16))
import Data.String (String)

import qualified MagicWormhole.Internal.Messages as Messages
import qualified MagicWormhole.Internal.Rendezvous as Rendezvous

-- | The version of the SPAKE2 protocol used by Magic Wormhole.
type Spake2Protocol = Spake2.Protocol Ed25519 SHA256

newtype Spake2Message = Spake2Message ByteString deriving (Eq, Show)

instance ToJSON Spake2Message where
  toJSON (Spake2Message msg) = object [ "pake_v1" .= toS @ByteString @Text (convertToBase Base16 msg) ]

instance FromJSON Spake2Message where
  parseJSON (Object msg) = do
    hexKey <- toS @Text @ByteString <$> msg .: "pake_v1"
    case convertFromBase Base16 hexKey of
      Left err -> fail err
      Right key -> pure $ Spake2Message key
  parseJSON unknown = typeMismatch "Spake2Message" unknown

type Spake2Element = Element Ed25519

decodeElement :: Spake2Protocol -> Messages.Body -> Either Error Spake2Element
decodeElement protocol (Messages.Body body) = do
  Spake2Message msg <- first ParseError (Aeson.eitherDecode (toS body))
  first ProtocolError $ Spake2.extractElement protocol msg

encodeElement :: Spake2Protocol -> Spake2Element -> Messages.Body
encodeElement protocol = Messages.Body . toS . Aeson.encode . Spake2Message . Spake2.elementToMessage protocol

-- | Construct a SPAKE2 protocol compatible with Magic Wormhole.
wormholeSpakeProtocol :: Messages.AppID -> Spake2Protocol
wormholeSpakeProtocol (Messages.AppID appID) =
  Spake2.makeSymmetricProtocol SHA256 Ed25519 blind sideID
  where
    blind = arbitraryElement Ed25519 ("S" :: ByteString)
    sideID = Spake2.SideID (toS appID)


-- data Peer
--   = Peer
--   { _peerSession :: Rendezvous.Session
--   }

-- | Exchange SPAKE2 keys with a Magic Wormhole peer.
pakeExchange :: Rendezvous.Session -> Spake2.Password -> IO (Either Error ByteString)
pakeExchange session password = do
  let protocol = wormholeSpakeProtocol (Rendezvous.sessionAppID session)
  exchange <- Spake2.startSpake2 protocol password
  let outbound = Spake2.computeOutboundMessage exchange
  (_, inbound) <- concurrently (sendPakeMessage protocol outbound) (atomically (receivePakeMessage protocol))
  case inbound of
    Left err -> pure (Left err)  -- Could not parse the SPAKE2 message
    Right inbound' ->
      let
        keyMaterial = Spake2.generateKeyMaterial exchange inbound'
      in pure (Right (Spake2.createSessionKey protocol inbound' outbound keyMaterial password))
  where
    sendPakeMessage protocol outbound =
      let body = encodeElement protocol outbound
      in Rendezvous.add session Messages.PakePhase body

    receivePakeMessage protocol = do
      -- XXX: This is kind of a fun approach, but it means that everyone else
      -- has to promise that they *don't* consume pake messages.
      msg <- Rendezvous.readFromMailbox session
      unless (Messages.phase msg == Messages.PakePhase) retry
      pure (decodeElement protocol (Messages.body msg))


data Error
  = ParseError String
  | ProtocolError Spake2.MessageError
  deriving (Eq, Show)
