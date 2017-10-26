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
  , MessageID(..)
  , Side(..)
  , Phase(..)
  , Body(..)
  , Nameplate(..)
  , Mailbox(..)
  , Mood(..)
  ) where

import Protolude

import Control.Monad (fail)
import Data.Aeson
  ( FromJSON(..)
  , ToJSON(..)
  , Value(Object, String)
  , (.:)
  , (.:?)
  , (.=)
  , eitherDecode
  , encode
  , object
  )
import Data.Aeson.Types (Pair, typeMismatch)
import Data.ByteArray.Encoding (convertFromBase, convertToBase, Base(Base16))
import Data.Hashable (Hashable)
import Data.String (String)
import qualified Network.Socket as Socket
import qualified Network.WebSockets as WS
import Numeric (readHex, showHex)

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
  | Nameplates { nameplates :: [Nameplate] }
  | Allocated { nameplate :: Nameplate }
  | Claimed { mailbox :: Mailbox }
  | Released
  | Message { side :: Side, phase :: Phase, messageID :: MessageID, body :: Body }
  | Closed
  | Ack
  | Pong Int
  | Error { _errorMessage :: Text , _original :: ClientMessage }
  deriving (Eq, Show)

instance FromJSON ServerMessage where
  parseJSON (Object v) = do
    t <- v .: "type"
    case t of
      "welcome" -> do
        welcome <- v .: "welcome"
        Welcome <$> welcome .:? "motd" <*> welcome .:? "error"
      "nameplates" -> do
        ns <- v .: "nameplates"
        Nameplates <$> sequence [ Nameplate <$> n .: "id" | n <- ns ]
      "allocated" -> Allocated <$> v .: "nameplate"
      "claimed" -> Claimed <$> v .: "mailbox"
      "released" -> pure Released
      "message" -> Message <$> v .: "side" <*> v .: "phase" <*> v .: "id" <*> v .: "body"
      "closed" -> pure Closed
      "ack" -> pure Ack
      "pong" -> Pong <$> v .: "pong"
      "error" -> Error <$> v .: "error" <*> v .: "orig"
      _ -> fail $ "Unrecognized wormhole message type: " <> t
  parseJSON unknown = typeMismatch "Message" unknown

instance ToJSON ServerMessage where
  toJSON (Welcome motd' error') =
    objectWithType "welcome"
    [ "welcome" .= object (catMaybes [ ("motd" .=) <$> motd'
                                     , ("error" .=) <$> error'
                                     ])
    ]
  toJSON (Nameplates nameplates') =
    objectWithType "nameplates" ["nameplates" .= [ object ["id" .= n] | n <- nameplates' ] ]
  toJSON (Allocated nameplate') =
    objectWithType "allocated" [ "nameplate" .= nameplate' ]
  toJSON (Claimed mailbox') =
    objectWithType "claimed" [ "mailbox" .= mailbox' ]
  toJSON Released = objectWithType "released" []
  toJSON (Message side' phase' id body') =
    objectWithType "message"
    [ "phase" .= phase'
    , "side" .= side'
    , "body" .= body'
    , "id" .= id
    ]
  toJSON Closed = objectWithType "closed" []
  toJSON Ack = objectWithType "ack" []
  toJSON (Pong n) = objectWithType "pong" ["pong" .= n]
  toJSON (Error errorMsg orig) =
    objectWithType "error" [ "error" .= errorMsg
                           , "orig" .= orig
                           ]

-- | Create a JSON object with a "type" field.
objectWithType :: Text -> [Pair] -> Value
objectWithType typ pairs = object $ ("type" .= typ):pairs

-- | Identifier for a "nameplate".
--
-- TODO: Explain what a nameplate is and how it's used.
newtype Nameplate = Nameplate Text deriving (Eq, Show, ToJSON, FromJSON)

-- | XXX: Placeholders
newtype Phase = Phase Text deriving (Eq, Show, ToJSON, FromJSON)

-- | Identifier for a mailbox.
--
-- TODO: Explain what a mailbox is and how it's used.
newtype Mailbox = Mailbox Text deriving (Eq, Show, ToJSON, FromJSON)

-- | The body of a magic wormhole message.
--
-- This can be any arbitrary bytestring that is sent to or received from a
-- wormhole peer.
newtype Body = Body ByteString deriving (Eq, Show)

instance ToJSON Body where
  toJSON (Body bytes) = toJSON (toS @ByteString @Text (convertToBase Base16 bytes))

instance FromJSON Body where
  parseJSON (String s) = either fail (pure . Body) (convertFromBase Base16 (toS @Text @ByteString s))
  parseJSON x = typeMismatch "Body" x

-- | A message sent from a rendezvous client to the server.
data ClientMessage
  = -- | Set the application ID and the "side" for the duration of the connection.
    Bind AppID Side
    -- | Get a list of all allocated nameplates.
  | List
    -- | Ask the server to allocate a nameplate
  | Allocate
    -- | Claim a nameplate.
  | Claim Nameplate
    -- | Release a claimed nameplate.
    -- XXX: What are the semantics of not specifying this?
  | Release (Maybe Nameplate)
    -- | Open a mailbox.
  | Open Mailbox
    -- | Close a mailbox. Since only one mailbox can be open at a time, if
    -- mailbox isn't specified, then close the open mailbox.
  | Close (Maybe Mailbox) (Maybe Mood)
    -- | Internal "ping". Response is 'Pong'. Used for testing.
  | Ping Int
  deriving (Eq, Show)

instance FromJSON ClientMessage where
  parseJSON (Object v) = do
    t <- v .: "type"
    case t of
      "bind" -> Bind <$> v .: "appid" <*> v .: "side"
      "list" -> pure List
      "allocate" -> pure Allocate
      "claim" -> Claim <$> v .: "nameplate"
      "release" -> Release <$> v .:? "nameplate"
      "open" -> Open <$> v .: "mailbox"
      "close" -> Close <$> v .:? "mailbox" <*> v .:? "mood"
      "ping" -> Ping <$> v .: "ping"
      _ -> fail $ "Unrecognized rendezvous client message type: " <> t
  parseJSON unknown = typeMismatch "Message" unknown

instance ToJSON ClientMessage where
  toJSON (Bind appID side') =
    objectWithType "bind"  [ "appid" .= appID
                           , "side" .= side'
                           ]
  toJSON List = objectWithType "list" []
  toJSON Allocate = objectWithType "allocate" []
  toJSON (Claim nameplate') = objectWithType "claim" [ "nameplate" .= nameplate' ]
  toJSON (Release nameplate') =
    objectWithType "release" $ case nameplate' of
                                 Nothing -> []
                                 Just n -> ["nameplate" .= n]
  toJSON (Open mailbox') = objectWithType "open" [ "mailbox" .= mailbox' ]
  toJSON (Close mailbox' mood') =
    objectWithType "close" $ catMaybes [ ("mailbox" .=) <$> mailbox'
                                       , ("mood" .=) <$> mood'
                                       ]
  toJSON (Ping n) = objectWithType "ping" [ "ping" .= n]

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

data Mood = Happy | Lonely | Scary | Errory deriving (Eq, Show)

instance ToJSON Mood where
  toJSON Happy = "happy"
  toJSON Lonely = "lonely"
  toJSON Scary = "scary"
  toJSON Errory = "errory"

instance FromJSON Mood where
  parseJSON (String s) =
    case s of
      "happy" -> pure Happy
      "lonely" -> pure Lonely
      "scary" -> pure Scary
      "errory" -> pure Errory
      _ -> fail $ "Unrecognized mood: " <> toS s
  parseJSON unknown = typeMismatch "Mood" unknown

type ParseError = String

-- | Identifier sent with every client message that is included in the
-- matching server responses.
newtype MessageID = MessageID Int16 deriving (Eq, Show, Hashable)

instance ToJSON MessageID where
  toJSON (MessageID n) = toJSON $ showHex n ""

instance FromJSON MessageID where
  parseJSON (String s) =
    case readHex (toS s) of
      [(n, _)] -> pure (MessageID n)
      _ -> fail $ "Could not parse MessageID: " <> toS s
  parseJSON unknown = typeMismatch "MessageID" unknown

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
bind ws appID side' = do
  WS.sendBinaryData ws (encode (Bind appID side'))
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

runClient :: WebSocketEndpoint -> AppID -> Side -> (Connection -> IO a) -> IO a
runClient (WebSocketEndpoint host port path) appID side' app =
  Socket.withSocketsDo . WS.runClient host port path $ \ws -> do
    conn' <- connect ws
    case conn' of
      Left _err -> notImplemented -- XXX: Welcome failed
      Right conn -> do
        bind ws appID side'
        app conn

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
