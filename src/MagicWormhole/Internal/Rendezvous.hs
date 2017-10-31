{-# LANGUAGE NamedFieldPuns #-}
-- | Interactions with a Magic Wormhole Rendezvous server.
--
-- Intended to be imported qualified, e.g.
-- ```
-- import qualified MagicWormhole.Internal.Rendezvous as Rendezvous
-- ```
module MagicWormhole.Internal.Rendezvous
  (
    -- * Specific RPCs
    ping
  , list
  , allocate
  , claim
  , release
  , open
  , close
  , add
    -- * Running a Rendezvous client
  , Connection
  , runClient
  ) where

import Protolude hiding (list, phase)

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

import Data.Aeson (eitherDecode, encode)
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HashMap
import Data.String (String)
import qualified Network.Socket as Socket
import qualified Network.WebSockets as WS

import MagicWormhole.Internal.Messages
  ( ClientMessage(..)
  , ServerMessage(..)
  , AppID
  , Body
  , Mailbox
  , Mood
  , Nameplate
  , Phase
  , Side
  )
import MagicWormhole.Internal.WebSockets (WebSocketEndpoint(..))

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
readMessage :: HasCallStack => Connection -> IO (Maybe ServerError)
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
            err@Error{errorMessage, original} ->
              case expectedResponse original of
                Nothing -> pure (Just (ErrorForNonRequest errorMessage original))
                Just responseType ->
                  atomically $ gotResponse conn responseType err
            Message{} -> notImplemented  -- TODO: Implement message handling!
            _ -> panic $ "Impossible code. No response type for " <> show msg  -- XXX: Pretty sure we can design this away.
        Just responseType ->
          atomically $ gotResponse conn responseType msg

-- | Run a Magic Wormhole Rendezvous client.
--
-- Will fail with IO (Left ServerError) if the server declares we are unwelcome.
runClient :: HasCallStack => WebSocketEndpoint -> AppID -> Side -> (Connection -> IO a) -> IO (Either ServerError a)
runClient (WebSocketEndpoint host port path) appID side' app =
  Socket.withSocketsDo . WS.runClient host port path $ \ws -> do
    conn' <- connect ws
    case conn' of
      Left err -> pure (Left err)
      Right conn -> do
        bind conn appID side'
        -- TODO: Currently discards any ServerErrors that occur while reading.
        -- Find a way to usefully report these errors.
        Right <$> withAsync (forever (void (readMessage conn)))
          (\_ -> app conn)

-- | Establish a Magic Wormhole connection.
--
-- Must be called as the first thing after opening a websocket connection to a
-- Rendezvous server.
--
-- Receives the "welcome" message. If the message contains an error, returns
-- that as Left, otherwise return a connection.
connect :: HasCallStack => WS.Connection -> IO (Either ServerError Connection)
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
receive :: HasCallStack => Connection -> IO (Either ServerError ServerMessage)
receive = map (bimap ParseError identity . eitherDecode) . WS.receiveData . wsConn

-- | Send a message to the Rendezvous server that we don't expect a response for.
send :: HasCallStack => Connection -> ClientMessage -> IO ()
send conn req = WS.sendBinaryData (wsConn conn) (encode req)

-- | Make a request to the rendezvous server.
rpc :: HasCallStack => Connection -> ClientMessage -> IO (Either RendezvousError ServerMessage)
rpc conn req =
  case expectedResponse req of
    Nothing ->
      -- XXX: Pretty sure we can juggle things around at the type level
      -- to remove this check. (i.e. make a type for RPC requests).
      pure (Left (ClientError (NotAnRPC req)))
    Just responseType -> do
      box' <- atomically $ expectResponse conn responseType
      case box' of
        Left clientError -> pure (Left (ClientError clientError))
        Right box -> do
          send conn req
          response <- atomically $ waitForResponse conn responseType box
          pure $ case response of
                   Error reason original -> Left (ClientError (BadRequest reason original))
                   response' -> Right response'

-- | Set the application ID and side for the rest of this connection.
--
-- The Rendezvous protocol doesn't have a response to 'bind', so there's no
-- way to tell if it has had its effect.
--
-- See https://github.com/warner/magic-wormhole/issues/261
bind :: HasCallStack => Connection -> AppID -> Side -> IO ()
bind conn appID side' = send conn (Bind appID side')

-- | Ping the server.
--
-- This is an in-band ping, used mostly for testing. It is not necessary to
-- keep the connection alive.
ping :: HasCallStack => Connection -> Int -> IO (Either RendezvousError Int)
ping conn n = do
  response <- rpc conn (Ping n)
  pure $ case response of
    Left err -> Left err
    Right (Pong n') -> Right n'
    Right unexpected -> unexpectedMessage (Ping n) unexpected

-- | List the nameplates on the server.
list :: HasCallStack => Connection -> IO (Either RendezvousError [Nameplate])
list conn = do
  response <- rpc conn List
  pure $ case response of
    Left err -> Left err
    Right (Nameplates nameplates) -> Right nameplates
    Right unexpected -> unexpectedMessage List unexpected

-- | Allocate a nameplate on the server.
allocate :: HasCallStack => Connection -> IO (Either RendezvousError Nameplate)
allocate conn = do
  response <- rpc conn Allocate
  pure $ case response of
    Left err -> Left err
    Right (Allocated nameplate) -> Right nameplate
    Right unexpected -> unexpectedMessage Allocate unexpected

-- | Claim a nameplate on the server.
claim :: HasCallStack => Connection -> Nameplate -> IO (Either RendezvousError Mailbox)
claim conn nameplate = do
  response <- rpc conn (Claim nameplate)
  pure $ case response of
    Left err -> Left err
    Right (Claimed mailbox) -> Right mailbox
    Right unexpected -> unexpectedMessage (Claim nameplate) unexpected

-- | Release a nameplate on the server.
--
-- TODO: Document semantics around "optional" nameplate.
--
-- TODO: Make this impossible to call unless we have already claimed a
-- namespace.
release :: HasCallStack => Connection -> Maybe Nameplate -> IO (Either RendezvousError ())
release conn nameplate' = do
  response <- rpc conn (Release nameplate')
  pure $ case response of
    Left err -> Left err
    Right Released -> Right ()
    Right unexpected -> unexpectedMessage (Release nameplate') unexpected

-- | Open a mailbox on the server.
--
-- TODO: Are we sure that we don't have to wait for a response here?
--
-- TODO: Find out what happens if we call 'open' when we already have a mailbox open.
open :: HasCallStack => Connection -> Mailbox -> IO ()
open conn mailbox = send conn (Open mailbox)

-- | Close a mailbox on the server.
close :: HasCallStack => Connection -> Maybe Mailbox -> Maybe Mood -> IO (Either RendezvousError ())
close conn mailbox' mood' = do
  response <- rpc conn (Close mailbox' mood')
  pure $ case response of
    Left err -> Left err
    Right Closed -> Right ()
    Right unexpected -> unexpectedMessage (Close mailbox' mood') unexpected

-- | Send a message to the open mailbox.
--
-- XXX: Should we provide a version that blocks until the message comes back to us?
add :: HasCallStack => Connection -> Phase -> Body -> IO ()
add conn phase body = send conn (Add phase body)

-- | Called when an RPC receives a message as a response that does not match
-- the request.
--
-- As things are written, this should never happen, because 'gotResponse'
-- makes sure we only ever populate the response placeholder with something
-- that matches.
--
-- TODO: Try to make this unnecessary.
unexpectedMessage :: HasCallStack => ClientMessage -> ServerMessage -> a
unexpectedMessage request response = panic $ "Unexpected message: " <> show response <> ", in response to: " <> show request

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
    -- | We received an 'error' message for a message that's not expected to
    -- have a response.
  | ErrorForNonRequest Text ClientMessage
  deriving (Eq, Show)

-- | Error caused by misusing the client.
data ClientError
  = -- | We tried to do an RPC while another RPC with the same response type
    -- was in flight. See warner/magic-wormhole#260 for details.
    AlreadySent ResponseType
    -- | Tried to send a non-RPC as if it were an RPC (i.e. expecting a response).
  | NotAnRPC ClientMessage
    -- | We sent a message that the server could not understand.
  | BadRequest Text ClientMessage
  deriving (Eq, Show)
