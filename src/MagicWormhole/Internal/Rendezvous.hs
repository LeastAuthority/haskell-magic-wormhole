{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
  -- * Specific RPCs
  , ping
  -- * Running a Rendezvous client
  , Connection
  , runClient
  -- * Other
  , AppID(..)
  , Side(..)
  ) where

import Protolude

import Control.Monad (fail)
import Data.Aeson
  ( FromJSON(..)
  , ToJSON(..)
  , Value(Object)
  , (.:)
  , (.:?)
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
  = Welcome { motd :: Maybe Text, errorMessage :: Maybe Text }
  | Pong Int
  | Error { _errorMessage :: Text , _original :: ClientMessage }
  | Ack
  deriving (Eq, Show)

instance FromJSON ServerMessage where
  parseJSON (Object v) = do
    t <- v .: "type"
    case t of
      "welcome" -> do
        welcome <- v .: "welcome"
        Welcome <$> welcome .:? "motd" <*> welcome .:? "error"
      "pong" -> Pong <$> v .: "pong"
      "ack" -> pure Ack
      "error" -> Error <$> v .: "error" <*> v .: "orig"
      _ -> fail $ "Unrecognized wormhole message type: " <> t
  parseJSON unknown = typeMismatch "Message" unknown

instance ToJSON ServerMessage where
  toJSON (Welcome motd' error') =
    object [ "type" .= toJSON ("welcome" :: Text)
           , "welcome" .= object (catMaybes [ ("motd" .=) <$> motd'
                                            , ("error" .=) <$> error'
                                            ])
           ]
  toJSON (Pong n) = object ["type" .= ("pong" :: Text), "pong" .= n]
  toJSON (Error errorMsg orig) =
    object [ "type" .= ("error" :: Text)
           , "error" .= errorMsg
           , "orig" .= orig
           ]
  toJSON Ack = object ["type" .= ("ack" :: Text)]

-- | A message sent from a rendezvous client to the server.
data ClientMessage
  = -- | Internal "ping". Response is 'Pong'. Used for testing.
    Ping Int
    -- | Set the application ID and the "side" for the duration of the connection.
  | Bind AppID Side
  deriving (Eq, Show)

-- | Short string to identify the application. Clients must use the same
-- application ID if they wish to communicate with each other.
--
-- Recommendation is to use "$DNSNAME/$APPNAME", e.g.
-- the Python `wormhole` command-line tool uses
-- "lothar.com/wormhole/text-or-file-xfer".
newtype AppID = AppID Text deriving (Eq, Show, FromJSON, ToJSON)

-- | Short string used to differentiate between echoes of our own messages and
-- real messages from other clients.
newtype Side = Side Text deriving (Eq, Show, FromJSON, ToJSON)

instance FromJSON ClientMessage where
  parseJSON (Object v) = do
    t <- v .: "type"
    case t of
      "ping" -> Ping <$> v .: "ping"
      "bind" -> Bind <$> v .: "appid" <*> v .: "side"
      _ -> fail $ "Unrecognized rendezvous client message type: " <> t
  parseJSON unknown = typeMismatch "Message" unknown

instance ToJSON ClientMessage where
  toJSON (Ping n) = object ["type" .= ("ping" :: Text), "ping" .= n]
  toJSON (Bind appID side) =
    object [ "type" .= ("bind" :: Text)
           , "appid" .= appID
           , "side" .= side
           ]

type ParseError = String

-- | Connection to a Rendezvous server.
newtype Connection = Conn { wsConn :: WS.Connection }

-- | Establish a Magic Wormhole connection.
--
-- Must be called as the first thing after opening a websocket connection to a
-- Rendezvous server.
--
-- Receives the "welcome" message. If the message contains an error, returns
-- that as Left, otherwise return a connection.
--
-- TODO: Also bind inside this.
--
-- TODO: Distinguish between parse error (server sent us non-welcome) and welcome error
-- (server sent us a 'welcome' with an error in it).
connect :: WS.Connection -> IO (Either Text Connection)
connect conn = do
  welcome <- eitherDecode <$> WS.receiveData conn
  pure $ case welcome of
    Left parseError -> Left $ toS parseError
    Right Welcome {errorMessage = Just errMsg} -> Left errMsg
    Right Welcome {errorMessage = Nothing} -> Right $ Conn conn
    Right unexpected -> Left $ "Unexpected message: " <> show unexpected

bind :: WS.Connection -> AppID -> Side -> IO ()
bind ws appID side = do
  WS.sendBinaryData ws (encode (Bind appID side))
  -- XXX: Broken, because messages might arrive out of order. How do we know
  -- next message is ack?
  response <- eitherDecode <$> WS.receiveData ws  -- XXX: Partial match
  case response of
    Right Ack -> pure ()
    -- XXX: Need to handle this
    _ -> notImplemented

-- | Receive a wormhole message from a websocket. Blocks until a message is received.
-- Returns an error string if we cannot parse the message as a valid wormhole 'Message'.
-- Throws exceptions if the underlying connection is closed or there is some error at the
-- websocket level.
receive :: Connection -> IO (Either ParseError ServerMessage)
receive = map eitherDecode . WS.receiveData . wsConn

-- | Make a request to the rendezvous server.
rpc :: Connection -> ClientMessage -> IO (Either ParseError ServerMessage)
rpc conn req = do
  WS.sendBinaryData (wsConn conn) (encode req)
  -- XXX: Broken, because messages might arrive out of order. How do we know
  -- next message is ack?
  Right _ack <- receive conn  -- XXX: Partial match
  -- XXX: Broken, for same reason. How do we know next message is response to request?
  receive conn

-- | Ping the server.
--
-- This is an in-band ping, used mostly for testing. It is not necessary to
-- keep the connection alive.
ping :: Connection -> Int -> IO (Either ParseError Int)
ping conn n = do
  response <- rpc conn (Ping n)
  -- XXX: Duplicated a lot with 'welcome'. Probably need a monad.
  pure $ case response of
    Left err -> Left err
    -- XXX: Should we indicate that pong response differs from ping?
    Right (Pong n') -> Right n'
    -- XXX: Distinguish error types here
    Right unexpected -> Left $ "Unexpected message: " <> show unexpected

runClient :: WebSocketEndpoint -> (Connection -> IO a) -> IO a
runClient (WebSocketEndpoint host port path) app =
  Socket.withSocketsDo . WS.runClient host port path $ \ws -> do
    conn' <- connect ws
    case conn' of
      Left _err -> notImplemented
      Right conn -> app conn

-- TODO
-- - use motd somehow
-- - bind -> m (), then wait for ack
-- - allocate -> m nameplace
-- - claim nameplate -> m mailbox
-- - open -> m ()
--
-- NOTES
-- - "message" messages received do not include mailbox information,
--   so we can only sensibly have one mailbox open
-- - "bind" doesn't have a response, so we don't know when server is
--   finished processing it. (jml thinks "ack" is sent immediately on receipt).
-- - might want to put some state on the 'Connection' type
-- - possibly create a separate "response" type?
