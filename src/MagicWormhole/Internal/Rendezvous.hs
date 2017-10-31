{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
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
import Data.Hashable (Hashable)
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HashMap
import Data.String (String)
import qualified Network.Socket as Socket
import qualified Network.WebSockets as WS

import qualified MagicWormhole.Internal.Messages as Messages
import MagicWormhole.Internal.WebSockets (WebSocketEndpoint(..))

-- | The type of a response.
--
-- This is used pretty much only to match responses with requests.
--
-- XXX: Make this a sum type?
-- XXX: Duplication with ServerMessage?
data ResponseType
  = NameplatesResponse
  | AllocatedResponse
  | ClaimedResponse
  | ReleasedResponse
  | ClosedResponse
  | PongResponse
  deriving (Eq, Show, Generic, Hashable)

-- XXX: This expectResponse / getResponseType stuff feels off to me.
-- I think we could do something better.
-- e.g.
-- - embed the response type in the ServerMessage value so we don't need to "specify" it twice
--   (once in the parser, once here)
-- - change the structure to have stricter types?
--   rather than a map of a type value to generic response box, have one box for each
--   request/response pair?

-- | Map 'ClientMessage' to a response. 'Nothing' means that we do not need a response.
expectedResponse :: Messages.ClientMessage -> Maybe ResponseType
expectedResponse Messages.Bind{} = Nothing
expectedResponse Messages.List = Just NameplatesResponse
expectedResponse Messages.Allocate = Just AllocatedResponse
expectedResponse Messages.Claim{} = Just ClaimedResponse
expectedResponse Messages.Release{} = Just ReleasedResponse
expectedResponse Messages.Open{} = Nothing
expectedResponse Messages.Add{} = Nothing
expectedResponse Messages.Close{} = Just ClosedResponse
expectedResponse Messages.Ping{} = Just PongResponse

-- | Map 'ServerMessage' to a response. 'Nothing' means that it's not a response to anything.
getResponseType :: Messages.ServerMessage -> Maybe ResponseType
getResponseType Messages.Welcome{} = Nothing
getResponseType Messages.Nameplates{} = Just NameplatesResponse
getResponseType Messages.Allocated{} = Just AllocatedResponse
getResponseType Messages.Claimed{} = Just ClaimedResponse
getResponseType Messages.Released = Just ReleasedResponse
getResponseType Messages.Message{} = Nothing
getResponseType Messages.Closed = Just ClosedResponse
getResponseType Messages.Ack = Nothing
getResponseType Messages.Pong{} = Just PongResponse
getResponseType Messages.Error{} = Nothing -- XXX: Alternatively, get the response type of the original message?

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
    pendingVar :: TVar (HashMap ResponseType (TMVar Messages.ServerMessage))
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
expectResponse :: Connection -> ResponseType -> STM (Either ClientError (TMVar Messages.ServerMessage))
expectResponse Conn{pendingVar} responseType = do
  pending <- readTVar pendingVar
  case HashMap.lookup responseType pending of
    Nothing -> do
      box <- newEmptyTMVar
      writeTVar pendingVar (HashMap.insert responseType box pending)
      pure (Right box)
    Just _ -> pure (Left (AlreadySent responseType))

waitForResponse :: Connection -> ResponseType -> TMVar Messages.ServerMessage -> STM Messages.ServerMessage
waitForResponse conn responseType box = do
  response <- takeTMVar box
  modifyTVar' (pendingVar conn) (HashMap.delete responseType)
  pure response

-- | Called when we have received a response from the server.
--
-- Tells anything waiting for the response that they can stop waiting now.
gotResponse :: Connection -> ResponseType -> Messages.ServerMessage -> STM (Maybe ServerError)
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
            Messages.Ack -> pure Nothing  -- Skip Ack, because there's no point in handling it.
            welcome@Messages.Welcome{} -> pure (Just (UnexpectedMessage welcome))
            err@Messages.Error{Messages.errorMessage, Messages.original} ->
              case expectedResponse original of
                Nothing -> pure (Just (ErrorForNonRequest errorMessage original))
                Just responseType ->
                  atomically $ gotResponse conn responseType err
            Messages.Message{} -> notImplemented  -- TODO: Implement message handling!
            _ -> panic $ "Impossible code. No response type for " <> show msg  -- XXX: Pretty sure we can design this away.
        Just responseType ->
          atomically $ gotResponse conn responseType msg

-- | Run a Magic Wormhole Rendezvous client.
--
-- Will fail with IO (Left ServerError) if the server declares we are unwelcome.
runClient :: HasCallStack => WebSocketEndpoint -> Messages.AppID -> Messages.Side -> (Connection -> IO a) -> IO (Either ServerError a)
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
    Right Messages.Welcome {Messages.welcomeErrorMessage = Just errMsg} -> pure . Left . Unwelcome $ errMsg
    Right Messages.Welcome {Messages.welcomeErrorMessage = Nothing} -> Right <$> atomically (newConnection conn)
    Right unexpected -> pure . Left . UnexpectedMessage $ unexpected

-- | Receive a wormhole message from a websocket. Blocks until a message is received.
-- Returns an error string if we cannot parse the message as a valid wormhole 'Message'.
-- Throws exceptions if the underlying connection is closed or there is some error at the
-- websocket level.
receive :: HasCallStack => Connection -> IO (Either ServerError Messages.ServerMessage)
receive = map (bimap ParseError identity . eitherDecode) . WS.receiveData . wsConn

-- | Send a message to the Rendezvous server that we don't expect a response for.
send :: HasCallStack => Connection -> Messages.ClientMessage -> IO ()
send conn req = WS.sendBinaryData (wsConn conn) (encode req)

-- | Make a request to the rendezvous server.
rpc :: HasCallStack => Connection -> Messages.ClientMessage -> IO (Either RendezvousError Messages.ServerMessage)
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
                   Messages.Error reason original -> Left (ClientError (BadRequest reason original))
                   response' -> Right response'

-- | Set the application ID and side for the rest of this connection.
--
-- The Rendezvous protocol doesn't have a response to 'bind', so there's no
-- way to tell if it has had its effect.
--
-- See https://github.com/warner/magic-wormhole/issues/261
bind :: HasCallStack => Connection -> Messages.AppID -> Messages.Side -> IO ()
bind conn appID side' = send conn (Messages.Bind appID side')

-- | Ping the server.
--
-- This is an in-band ping, used mostly for testing. It is not necessary to
-- keep the connection alive.
ping :: HasCallStack => Connection -> Int -> IO (Either RendezvousError Int)
ping conn n = do
  response <- rpc conn (Messages.Ping n)
  pure $ case response of
    Left err -> Left err
    Right (Messages.Pong n') -> Right n'
    Right unexpected -> unexpectedMessage (Messages.Ping n) unexpected

-- | List the nameplates on the server.
list :: HasCallStack => Connection -> IO (Either RendezvousError [Messages.Nameplate])
list conn = do
  response <- rpc conn Messages.List
  pure $ case response of
    Left err -> Left err
    Right (Messages.Nameplates nameplates) -> Right nameplates
    Right unexpected -> unexpectedMessage Messages.List unexpected

-- | Allocate a nameplate on the server.
allocate :: HasCallStack => Connection -> IO (Either RendezvousError Messages.Nameplate)
allocate conn = do
  response <- rpc conn Messages.Allocate
  pure $ case response of
    Left err -> Left err
    Right (Messages.Allocated nameplate) -> Right nameplate
    Right unexpected -> unexpectedMessage Messages.Allocate unexpected

-- | Claim a nameplate on the server.
claim :: HasCallStack => Connection -> Messages.Nameplate -> IO (Either RendezvousError Messages.Mailbox)
claim conn nameplate = do
  response <- rpc conn (Messages.Claim nameplate)
  pure $ case response of
    Left err -> Left err
    Right (Messages.Claimed mailbox) -> Right mailbox
    Right unexpected -> unexpectedMessage (Messages.Claim nameplate) unexpected

-- | Release a nameplate on the server.
--
-- TODO: Document semantics around "optional" nameplate.
--
-- TODO: Make this impossible to call unless we have already claimed a
-- namespace.
release :: HasCallStack => Connection -> Maybe Messages.Nameplate -> IO (Either RendezvousError ())
release conn nameplate' = do
  response <- rpc conn (Messages.Release nameplate')
  pure $ case response of
    Left err -> Left err
    Right Messages.Released -> Right ()
    Right unexpected -> unexpectedMessage (Messages.Release nameplate') unexpected

-- | Open a mailbox on the server.
--
-- TODO: Are we sure that we don't have to wait for a response here?
--
-- TODO: Find out what happens if we call 'open' when we already have a mailbox open.
open :: HasCallStack => Connection -> Messages.Mailbox -> IO ()
open conn mailbox = send conn (Messages.Open mailbox)

-- | Close a mailbox on the server.
close :: HasCallStack => Connection -> Maybe Messages.Mailbox -> Maybe Messages.Mood -> IO (Either RendezvousError ())
close conn mailbox' mood' = do
  response <- rpc conn (Messages.Close mailbox' mood')
  pure $ case response of
    Left err -> Left err
    Right Messages.Closed -> Right ()
    Right unexpected -> unexpectedMessage (Messages.Close mailbox' mood') unexpected

-- | Send a message to the open mailbox.
--
-- XXX: Should we provide a version that blocks until the message comes back to us?
add :: HasCallStack => Connection -> Messages.Phase -> Messages.Body -> IO ()
add conn phase body = send conn (Messages.Add phase body)

-- | Called when an RPC receives a message as a response that does not match
-- the request.
--
-- As things are written, this should never happen, because 'gotResponse'
-- makes sure we only ever populate the response placeholder with something
-- that matches.
--
-- TODO: Try to make this unnecessary.
unexpectedMessage :: HasCallStack => Messages.ClientMessage -> Messages.ServerMessage -> a
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
    ResponseWithoutRequest ResponseType Messages.ServerMessage
    -- | We couldn't understand the message from the server.
  | ParseError String
    -- | Clients are not welcome on the server right now.
  | Unwelcome Text
    -- | We were sent a message other than "Welcome" on connect.
  | UnexpectedMessage Messages.ServerMessage
    -- | We received an 'error' message for a message that's not expected to
    -- have a response.
  | ErrorForNonRequest Text Messages.ClientMessage
  deriving (Eq, Show)

-- | Error caused by misusing the client.
data ClientError
  = -- | We tried to do an RPC while another RPC with the same response type
    -- was in flight. See warner/magic-wormhole#260 for details.
    AlreadySent ResponseType
    -- | Tried to send a non-RPC as if it were an RPC (i.e. expecting a response).
  | NotAnRPC Messages.ClientMessage
    -- | We sent a message that the server could not understand.
  | BadRequest Text Messages.ClientMessage
  deriving (Eq, Show)
