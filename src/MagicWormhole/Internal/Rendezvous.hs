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
  = -- | Sent by the server on initial connection.
    Welcome
    { -- | A message to be displayed to users when they connect to the server
      motd :: Maybe Text
      -- | If present, the server does not want the client to proceed. Here's the reason why.
    , welcomeErrorMessage :: Maybe Text
    }
  | -- | Sent in response to "list"
    Nameplates
    {
      nameplates :: [Nameplate]
    }
  | -- | Sent in response to "allocate"
    Allocated
    {
      -- | The nameplate allocated to this connection (?).
      nameplate :: Nameplate
    }
  | -- | Sent in response to "claim"
    Claimed
    { -- | The mailbox claimed by this connection (?)
      mailbox :: Mailbox
    }
  | -- | Sent in response to "release"
    Released
  | -- | A message sent to the mailbox
    Message
    {
      -- | Which side sent the message. Might be our side.
      side :: Side
    , -- | Which phase of the client protocol we are in.
      phase :: Phase
      -- | An identifier for the message. Unused.
    , messageID :: MessageID
    , -- | The body of the message. To be interpreted by the client protocol.
      body :: Body
    }
  | -- | Sent in response to "close"
    Closed
  | -- | Sent immediately after every message. Unused.
    Ack
  | -- | Sent in response to "pong"
    Pong Int
  | -- | Sent by the server when it receives something from the client that it does not understand.
    Error
    { -- | Message explaining what the problem is
      errorMessage :: Text
      -- | The message that caused the problem.
    , original :: ClientMessage
    }
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

-- | TODO: Document phase once we understand what it is. It's a bit awkward,
-- because it appears to be an aspect of the client protocol, which I'd rather
-- the server protocol implementation not have to know about.
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
    -- TODO: Document the semantics of not specifying the nameplate to release.
  | Release (Maybe Nameplate)
    -- | Open a mailbox.
  | Open Mailbox
    -- | Send a message to an open mailbox. The message will be delivered to
    -- all connected clients that also have that mailbox open, including this
    -- one.
  | Add Phase Body
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
      "add" -> Add <$> v .: "phase" <*> v .: "body"
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
  toJSON (Add phase' body') = objectWithType "add"
    [ "phase" .= phase'
    , "body" .= body'
    ]
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

-- | How the client feels. Reported by the client to the server at the end of
-- a wormhole session.
data Mood
  = -- | The client had a great session with its peer.
    Happy
    -- | The client never saw its peer.
  | Lonely
    -- | The client saw a peer it could not trust.
  | Scary
    -- | The client encountered some problem.
  | Errory deriving (Eq, Show)

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

-- | The type of a response.
--
-- This is used pretty much only to match responses with requests.
--
-- XXX: Make this a sum type?
-- XXX: Duplication with ServerMessage?
type ResponseType = Text

-- XXX: This expectResponse / getResponseType stuff feels off to me.
-- I think we could do something better.
-- e.g.
-- - use an enum for ResponseType
-- - embed the response type in the ServerMessage value so we don't need to "specify" it twice
--   (once in the parser, once here)
-- - change the structure to have stricter types?
--   rather than a map of a type value to generic response box, have one box for each
--   request/response pair?

-- | Map 'ClientMessage' to a response. 'Nothing' means that we do not need a response.
expectedResponse :: ClientMessage -> Maybe ResponseType
expectedResponse Bind{} = Nothing
expectedResponse List = Just "nameplates"
expectedResponse Allocate = Just "allocated"
expectedResponse Claim{} = Just "claimed"
expectedResponse Release{} = Just "released"
expectedResponse Open{} = Nothing
expectedResponse Add{} = Nothing
expectedResponse Close{} = Just "closed"
expectedResponse Ping{} = Just "pong"

-- | Map 'ServerMessage' to a response. 'Nothing' means that it's not a response to anything.
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

-- | Connection to a Rendezvous server.
--
-- Note on "RPCs":
--
-- The Magic Wormhole Rendezvous protocol is full duplex. Clients can send
-- messages at any time and servers can send messages at any time.
--
-- Some of the messages sent by the client have a corresponding response. In
-- this module, we call those "RPCs", and the outgoing client messages
-- "requests".
--
-- To send an RPC with a 'Connection', we must:
-- - register that we expect a response with 'expectResponse'
-- - actually send the request
-- - wait for the response with 'waitResponse'
-- - continuously read messages from the server and call 'gotResponse' when we get them
--
-- To send messages that do not require a response, just use 'send'.
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
    -- TODO: Perhaps abstronaut this away, making ResponseType and
    -- ServerMessage both type variables, and instead implement something that
    -- just matches requests with responses and shoves unmatched ones to a
    -- channel.
    pendingVar :: TVar (HashMap ResponseType (TMVar ServerMessage))
  }

-- | Initialize a new Magic Wormhole Rendezvous connection.
--
-- Will generally want to use 'connect' instead.
newConnection :: WS.Connection -> STM Connection
newConnection ws = do
  pendingVar' <- newTVar mempty
  pure Conn { wsConn = ws, pendingVar = pendingVar' }

-- | Tell the connection that we expect a response of the given type.
--
-- Will fail with a 'ClientError' if we are already expecting a response of this type.
expectResponse :: Connection -> ResponseType -> STM (Either ClientError (TMVar ServerMessage))
expectResponse Conn{pendingVar} responseType = do
  pending <- readTVar pendingVar
  case HashMap.lookup responseType pending of
    Nothing -> do
      box <- newEmptyTMVar
      writeTVar pendingVar (HashMap.insert responseType box pending)
      pure (Right box)
    Just _ -> pure (Left (AlreadySent responseType))

waitForResponse :: Connection -> ResponseType -> TMVar ServerMessage -> STM ServerMessage
waitForResponse conn responseType box = do
  response <- takeTMVar box
  modifyTVar' (pendingVar conn) (HashMap.delete responseType)
  pure response

-- | Called when we have received a response from the server.
--
-- Tells anything waiting for the response that they can stop waiting now.
gotResponse :: Connection -> ResponseType -> ServerMessage -> STM (Maybe ServerError)
gotResponse Conn{pendingVar} responseType message = do
  pending <- readTVar pendingVar
  case HashMap.lookup responseType pending of
    Nothing -> pure (Just (ResponseWithoutRequest responseType message))
    Just box -> do
      -- TODO: This will block reading from the server (by retrying the
      -- transaction) if the box is already populated. I don't think we want
      -- that, but I'm not sure what behavior we do want.
      putTMVar box message
      pure Nothing

-- | Read a message from the server. If it's a response, make sure we handle it.
readMessage :: Connection -> IO (Maybe ServerError)
readMessage conn = do
  msg' <- receive conn
  case msg' of
    Left parseError -> pure (Just parseError)
    Right msg ->
      case getResponseType msg of
        Nothing ->
          case msg of
            Ack -> pure Nothing  -- Skip Ack, because there's no point in handling it.
            welcome@Welcome{} -> pure (Just (UnexpectedMessage welcome))
            Error{errorMessage, original} -> pure (Just (BadRequest errorMessage original))
            Message{} -> notImplemented
            _ -> panic $ "Impossible code. No response type for " <> show msg  -- XXX: Pretty sure we can design this away.
        Just responseType ->
          atomically $ gotResponse conn responseType msg

-- | Run a Magic Wormhole Rendezvous client.
runClient :: WebSocketEndpoint -> AppID -> Side -> (Connection -> IO a) -> IO a
runClient (WebSocketEndpoint host port path) appID side' app =
  Socket.withSocketsDo . WS.runClient host port path $ \ws -> do
    conn' <- connect ws
    case conn' of
      Left _err -> notImplemented -- XXX: Welcome failed
      Right conn -> do
        bind conn appID side'
        withAsync (forever (void (readMessage conn)))  -- XXX: Discards any server errors.
          (\_ -> app conn)

-- | Establish a Magic Wormhole connection.
--
-- Must be called as the first thing after opening a websocket connection to a
-- Rendezvous server.
--
-- Receives the "welcome" message. If the message contains an error, returns
-- that as Left, otherwise return a connection.
connect :: WS.Connection -> IO (Either ServerError Connection)
connect conn = do
  welcome <- eitherDecode <$> WS.receiveData conn
  case welcome of
    Left parseError -> pure . Left . ParseError $ parseError
    Right Welcome {welcomeErrorMessage = Just errMsg} -> pure . Left . Unwelcome $ errMsg
    Right Welcome {welcomeErrorMessage = Nothing} -> Right <$> atomically (newConnection conn)
    Right unexpected -> pure . Left . UnexpectedMessage $ unexpected

-- | Receive a wormhole message from a websocket. Blocks until a message is received.
-- Returns an error string if we cannot parse the message as a valid wormhole 'Message'.
-- Throws exceptions if the underlying connection is closed or there is some error at the
-- websocket level.
receive :: Connection -> IO (Either ServerError ServerMessage)
receive = map (bimap ParseError identity . eitherDecode) . WS.receiveData . wsConn

-- | Send a message to the Rendezvous server that we don't expect a response for.
send :: Connection -> ClientMessage -> IO ()
send conn req = WS.sendBinaryData (wsConn conn) (encode req)

-- | Make a request to the rendezvous server.
rpc :: Connection -> ClientMessage -> IO (Either RendezvousError ServerMessage)
rpc conn req =
  case expectedResponse req of
    Nothing ->
      -- XXX: Pretty sure we can juggle things around at the type level
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
          response <- atomically $ waitForResponse conn responseType box
          pure (Right response)

-- | Set the application ID and side for the rest of this connection.
--
-- The Rendezvous protocol doesn't have a response to 'bind', so there's no
-- way to tell if it has had its effect.
--
-- See https://github.com/warner/magic-wormhole/issues/261
bind :: Connection -> AppID -> Side -> IO ()
bind conn appID side' = send conn (Bind appID side')

-- | Ping the server.
--
-- This is an in-band ping, used mostly for testing. It is not necessary to
-- keep the connection alive.
ping :: Connection -> Int -> IO (Either RendezvousError Int)
ping conn n = do
  response <- rpc conn (Ping n)
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
-- | Any possible error from this module.
data RendezvousError
  = -- | An error due to misusing the client.
    ClientError ClientError
    -- | An error due to weird message from the server.
  | ServerError ServerError deriving (Eq, Show)

-- | Error due to weirdness from the server.
data ServerError
  = -- | Server sent us a response for something that we hadn't requested.
    ResponseWithoutRequest ResponseType ServerMessage
    -- | We couldn't understand the message from the server.
  | ParseError String
    -- | Clients are not welcome on the server right now.
  | Unwelcome Text
    -- | We were sent a message other than "Welcome" on connect.
  | UnexpectedMessage ServerMessage
    -- | Client sent a message that the server could not understand.
  | BadRequest Text ClientMessage
  deriving (Eq, Show)

-- | Error caused by misusing the client.
data ClientError
  = -- | We tried to do an RPC while another RPC with the same response type
    -- was in flight. See warner/magic-wormhole#260 for details.
    AlreadySent ResponseType
    -- | Tried to send a non-RPC as if it were an RPC (i.e. expecting a response).
  | NotAnRPC ClientMessage
  deriving (Eq, Show)
