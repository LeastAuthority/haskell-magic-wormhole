{-# LANGUAGE NamedFieldPuns #-}
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

import Control.Concurrent.STM
  ( TVar
  , newTVar
  , modifyTVar'
  , readTVar
  , writeTVar
  , TMVar
  , newEmptyTMVar
  , putTMVar
  , takeTMVar
  )
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
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HashMap
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
--
-- Use this to construct objects for client and server messages.
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

-- | The type of a response.
--
-- This is used pretty much only to match responses with requests.
--
-- TODO: Make this a sum type?
-- TODO: Duplication with ServerMessage?
type ResponseType = Text

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
data Connection
  = Conn
  { -- | Underlying websocket connection
    wsConn :: WS.Connection
  , -- | Responses that we are waiting for from the server.
    --
    -- If there is an entry in this map for a response type, that means
    -- we've made a request that expects the given response.
    --
    -- When the response is received, we populate the TMVar. Then the function
    -- that made the request unblocks, uses the value, and updates the map.
    --
    -- XXX: Could abstronaut this away, making ResponseType and ServerMessage
    -- both type variables, and instead implementing something that just
    -- matches requests with responses and shoves unmatched ones to a channel.
    pendingVar :: TVar (HashMap ResponseType (TMVar ServerMessage))
  }

newConnection :: WS.Connection -> STM Connection
newConnection ws = do
  pendingVar' <- newTVar mempty
  pure Conn { wsConn = ws, pendingVar = pendingVar' }

-- | Error caused by misusing the client.
data ClientError
  = -- | We tried to do an RPC while another RPC with the same response type
    -- was in flight. See warner/magic-wormhole#260 for details.
    AlreadySent ResponseType
    -- | Tried to send a non-RPC as if it were an RPC (i.e. expecting a response).
  | NotAnRPC ClientMessage
  deriving (Eq, Show)

expectResponse :: Connection -> ResponseType -> STM (Either ClientError (TMVar ServerMessage))
expectResponse Conn{pendingVar} responseType = do
  pending <- readTVar pendingVar
  case HashMap.lookup responseType pending of
    Nothing -> do
      box <- newEmptyTMVar
      writeTVar pendingVar (HashMap.insert responseType box pending)
      pure (Right box)
    Just _ -> pure (Left (AlreadySent responseType))

-- | Error due to weirdness from the server.
data ServerError
  = UnrecognizedResponse ServerMessage
  | ResponseWithoutRequest ServerMessage
  | ParseError String
  deriving (Eq, Show)

gotResponse :: Connection -> ResponseType -> ServerMessage -> STM (Maybe ServerError)
gotResponse Conn{pendingVar} responseType message = do
  pending <- readTVar pendingVar
  case HashMap.lookup responseType pending of
    Nothing -> pure (Just (ResponseWithoutRequest message))
    Just box -> do
      putTMVar box message  -- XXX: This will block (retry the transaction) if already populated. I don't think that's what we want.
      pure Nothing

-- XXX: This expectResponse / getResponseType stuff feels off to me.
-- I think we could do something better.
-- e.g.
-- - use an enum for ResponseType
-- - embed the response type in the ServerMessage value so we don't need to "specify" it twice
--   (once in the parser, once here)
-- - change the structure to have stricter types?
--   rather than a map of a type value to generic response box, have one box for each
--   request/response pair?

expectedResponse :: ClientMessage -> Maybe ResponseType
expectedResponse Bind{} = Nothing
expectedResponse List = Just "nameplates"
expectedResponse Allocate = Just "allocated"
expectedResponse Claim{} = Just "claimed"
expectedResponse Release{} = Just "released"
expectedResponse Open{} = Nothing
expectedResponse Close{} = Just "closed"
expectedResponse Ping{} = Just "pong"

getResponseType :: ServerMessage -> Maybe ResponseType
getResponseType Welcome{} = Nothing
getResponseType Nameplates{} = Just "nameplates"
getResponseType Allocated{} = Just "allocated"
getResponseType Claimed{} = Just "claimed"
getResponseType Released = Just "released"
getResponseType Message{} = Nothing
getResponseType Closed = Just "closed"
getResponseType Ack = Nothing
getResponseType Pong{} = Just "pong"
getResponseType Error{} = Nothing -- XXX: Alternatively, get the response type of the original message?

runClient :: WebSocketEndpoint -> AppID -> Side -> (Connection -> IO a) -> IO a
runClient (WebSocketEndpoint host port path) appID side' app =
  Socket.withSocketsDo . WS.runClient host port path $ \ws -> do
    conn' <- connect ws
    case conn' of
      Left _err -> notImplemented -- XXX: Welcome failed
      Right conn -> do
        bind conn appID side'
        withAsync (forever (void (readMessage conn))) (\_ -> app conn)

readMessage :: Connection -> IO (Maybe ServerError)
readMessage conn = do
  msg' <- eitherDecode <$> WS.receiveData (wsConn conn)
  case msg' of
    Left parseError -> pure (Just (ParseError parseError))
    Right msg ->
      case getResponseType msg of
        Nothing ->
          case msg of
            Ack -> pure Nothing  -- Skip Ack, because there's no point in handling it.
            Welcome{} -> notImplemented -- XXX: Not sure how to handle this?
            Error{} -> notImplemented -- XXX: Need a plan for handling errors
            _ -> panic $ "Impossible code. No response type for " <> show msg  -- XXX: Pretty sure we can design this away.
        Just responseType ->
          atomically $ gotResponse conn responseType msg

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
  case welcome of
    Left parseError -> pure . Left $ toS parseError
    Right Welcome {errorMessage = Just errMsg} -> pure . Left $ errMsg
    Right Welcome {errorMessage = Nothing} -> Right <$> atomically (newConnection conn)
    Right unexpected -> pure . Left $ "Unexpected message: " <> show unexpected

-- | Receive a wormhole message from a websocket. Blocks until a message is received.
-- Returns an error string if we cannot parse the message as a valid wormhole 'Message'.
-- Throws exceptions if the underlying connection is closed or there is some error at the
-- websocket level.
receive :: Connection -> IO (Either ParseError ServerMessage)
receive = map eitherDecode . WS.receiveData . wsConn

data RendezvousError = ClientError ClientError | ServerError ServerError deriving (Eq, Show)

-- | Send a message to the Rendezvous server that we don't expect a response for.
send :: Connection -> ClientMessage -> IO ()
send conn req = WS.sendBinaryData (wsConn conn) (encode req)

-- | Make a request to the rendezvous server.
rpc :: Connection -> ClientMessage -> IO (Either RendezvousError ServerMessage)
rpc conn req =
  case expectedResponse req of
    Nothing ->
      -- TODO: Pretty sure we can juggle things around at the type level
      -- to remove this check. (i.e. make a type for RPC requests).
      pure (Left (ClientError (NotAnRPC req)))
    Just responseType -> do
      box' <- atomically $ expectResponse conn responseType
      case box' of
        -- XXX: There's probably something clever we can do with the Left ->
        -- Left, Right -> Right symmetry here.
        Left clientError -> pure (Left (ClientError clientError))
        Right box -> do
          send conn req
          response <- atomically $ do
            -- XXX: Make this its own function
            response <- takeTMVar box
            modifyTVar' (pendingVar conn) (HashMap.delete responseType)
            pure response
          pure (Right response)

bind :: Connection -> AppID -> Side -> IO ()
bind conn appID side' = send conn (Bind appID side')

-- | Ping the server.
--
-- This is an in-band ping, used mostly for testing. It is not necessary to
-- keep the connection alive.
ping :: Connection -> Int -> IO (Either RendezvousError Int)
ping conn n = do
  response <- rpc conn (Ping n)
  -- XXX: Duplicated a lot with 'welcome'. Probably need a monad.
  pure $ case response of
    Left err -> Left err
    -- XXX: Should we indicate that pong response differs from ping?
    Right (Pong n') -> Right n'
    Right unexpected ->
      -- XXX: Panic is OK because this shouldn't be possible.
      -- We should be able to refactor though.
      panic $ "Unexpected message: " <> show unexpected


-- TODO
-- - use motd somehow
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
