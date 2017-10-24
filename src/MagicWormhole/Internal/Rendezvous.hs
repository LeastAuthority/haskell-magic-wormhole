-- | Interactions with a Magic Wormhole Rendezvous server.
--
-- Intended to be imported qualified, e.g.
-- ```
-- import qualified MagicWormhole.Internal.Rendezvous as Rendezvous
-- ```
module MagicWormhole.Internal.Rendezvous
  ( ClientMessage(..)
  , ServerMessage(..)
  , receive
  , rpc
  , Connection
  , runClient
  ) where

import Protolude

import Control.Monad (fail)
import Data.Aeson
  ( FromJSON(..)
  , ToJSON(..)
  , Value(Object)
  , (.:)
  , (.=)
  , eitherDecode
  , encode
  , object
  )
import Data.Aeson.Types (typeMismatch)
import Data.String (String)
import qualified Network.Socket as Socket
import qualified Network.WebSockets as WS

import MagicWormhole.Internal.WebSockets (WebSocketEndpoint(..))

-- | A message that can be sent to or received from the server.
--
-- Notes
-- * clients & servers MUST ignore unrecognized keys in otherwise-recognized
--   messages
--
-- Error messages
-- * welcome messages can include 'error'
--
-- Some open questions:
-- * general message stuff--how are we going to model this?
--   * outgoing messages include a randomly generated 'id' field, which is
--     returned by the server
--   * messages from the server include 'server_tx', a float timestamp recording
--     when the server received the message
--   * messages from the server that are direct responses include a 'server_rx'
--     timestamp--unclear what this means?
--   * do we want a separate Haskell type for each message type? e.g. PingMessage
--   * if we do that, how do associate request/response pairs? e.g. PingMessage &
--     PongMessage?
--   * do we want to (can we even?) structurally distinguish between messages that
--     make sense outside the scope of a binding (e.g. ping) and messages that only
--     make sense after a bind (e.g. allocate)
data ServerMessage
  = Welcome
  | Pong Int
  | Error { _errorMessage :: Text , _original :: ClientMessage }
  | Ack
  deriving (Eq, Show)

instance FromJSON ServerMessage where
  parseJSON (Object v) = do
    t <- v .: "type"
    case t of
      "welcome" -> pure Welcome
      "pong" -> Pong <$> v .: "pong"
      "ack" -> pure Ack
      "error" -> Error <$> v .: "error" <*> v .: "orig"
      _ -> fail $ "Unrecognized wormhole message type: " <> t
  parseJSON unknown = typeMismatch "Message" unknown

instance ToJSON ServerMessage where
  toJSON Welcome = object ["type" .= ("welcome" :: Text)]
  toJSON (Pong n) = object ["type" .= ("pong" :: Text), "pong" .= n]
  toJSON (Error errorMsg orig) =
    object [ "type" .= ("error" :: Text)
           , "error" .= toJSON errorMsg
           , "orig" .= toJSON orig
           ]
  toJSON Ack = object ["type" .= ("ack" :: Text)]

-- | A message sent from a rendezvous client to the server.
data ClientMessage
  = Ping Int
  deriving (Eq, Show)

instance FromJSON ClientMessage where
  parseJSON (Object v) = do
    t <- v .: "type"
    case t of
      "ping" -> Ping <$> v .: "ping"
      _ -> fail $ "Unrecognized rendezvous client message type: " <> t
  parseJSON unknown = typeMismatch "Message" unknown

instance ToJSON ClientMessage where
  toJSON (Ping n) = object ["type" .= ("ping" :: Text), "ping" .= n]


type ParseError = String

-- | Connection to a Rendezvous server.
newtype Connection = Conn { wsConn :: WS.Connection }

-- | Receive a wormhole message from a websocket. Blocks until a message is received.
-- Returns an error string if we cannot parse the message as a valid wormhole 'Message'.
-- Throws exceptions if the underlying connection is closed or there is some error at the
-- websocket level.
receive :: Connection -> IO (Either ParseError ServerMessage)
receive = map eitherDecode . WS.receiveData . wsConn

-- | Send a wormhole message to a websocket. Blocks until message is sent.
-- Throws exceptions if the underlying connection is closed or there is some error at the
-- websocket level.
send :: Connection -> ClientMessage -> IO ()
send (Conn conn) message = WS.sendBinaryData conn (encode message)

-- | Make a request to the rendezvous server.
rpc :: Connection -> ClientMessage -> IO (Either ParseError ServerMessage)
rpc conn req = do
  send conn req
  -- XXX: Broken, because messages might arrive out of order.
  Right _ack <- receive conn  -- XXX: Partial match
  receive conn

runClient :: WebSocketEndpoint -> (Connection -> IO ()) -> IO ()
runClient (WebSocketEndpoint host port path) app =
  Socket.withSocketsDo $ WS.runClient host port path (app . Conn)
