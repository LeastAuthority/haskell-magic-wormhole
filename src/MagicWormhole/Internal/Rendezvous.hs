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
    -- * Running a Rendezvous client
  , runClient
  , Session
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
  , tryPutTMVar
  , TQueue
  , newTQueue
  , readTQueue
  , writeTQueue
  )
import Data.Aeson (eitherDecode, encode)
import Data.Hashable (Hashable)
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HashMap
import Data.String (String)
import qualified Network.Socket as Socket
import qualified Network.WebSockets as WS

import qualified MagicWormhole.Internal.ClientProtocol as ClientProtocol
import qualified MagicWormhole.Internal.Messages as Messages
import MagicWormhole.Internal.WebSockets (WebSocketEndpoint(..))
import qualified MagicWormhole.Internal.WebSockets as WebSockets

-- | Abstract type representing a Magic Wormhole session.
--
-- - 'runClient' gets a session
-- - 'rpc' sends RPCs from inside 'runClient'
-- - 'send' sends non-RPC messages (e.g. `bind`)
-- - 'readFromMailbox' reads messages from the mailbox
data Session
  = Session
  { connection :: WS.Connection
  , sessionAppID :: Messages.AppID -- ^ The 'AppID' of this running session
  , sessionSide :: Messages.Side -- ^ The 'Side' of this running session
  , pendingVar :: TVar (HashMap ResponseType (TMVar Messages.ServerMessage))
  , messageChan :: TQueue Messages.MailboxMessage
  , motd :: TMVar (Maybe Text)
  }

-- | Create a new 'Session'.
new :: WS.Connection -- ^ Active WebSocket connection to a Rendezvous Server.
    -> Messages.AppID
    -> Messages.Side
    -> STM Session  -- ^ Opaque 'Session' object.
new connection appID side
  = Session connection appID side
  <$> newTVar mempty
  <*> newTQueue
  <*> newEmptyTMVar

-- | Send a message to a Magic Wormhole Rendezvous server.
send :: Session -- ^ An active session. Get this using 'runClient'.
     -> Messages.ClientMessage -- ^ Message to send to the server.
     -> IO ()
-- XXX: I think this needs to use `sendClose` when the message is Close.
send session msg = do
  WS.sendBinaryData (connection session) (encode msg)
  putStrLn @Text $ ">>> " <> show msg -- XXX: Debug

-- | Receive a message from a Magic Wormhole Rendezvous server.
-- Blocks until such a message exists.
receive :: Session -- ^ An active session. Get this using 'runClient'.
        -> IO Messages.ServerMessage -- ^ Next message from the server.
receive session = do
  -- XXX: I think we need to catch `CloseRequest` here and gracefully stop.
  msg <- WS.receiveData (connection session)
  either (throwIO . ParseError) pure (eitherDecode msg)

-- | Run a Magic Wormhole Rendezvous client. Use this to interact with a Magic Wormhole server.
--
-- Will throw a 'ServerError' if the server declares we are unwelcome.
runClient
  :: HasCallStack
  => WebSocketEndpoint -- ^ The websocket to connect to
  -> Messages.AppID -- ^ ID for your application (e.g. example.com/your-application)
  -> Messages.Side -- ^ Identifier for your side
  -> (Session -> IO a) -- ^ Action to perform inside the Magic Wormhole session
  -> IO a -- ^ The result of the action
runClient endpoint appID side app =
  Socket.withSocketsDo . WebSockets.runClient endpoint $ \ws -> do
    session <- atomically $ new ws appID side
    result <- race (readMessages session) (action session)
    case result of
      Left err -> panic $ "Cannot possibly happen: " <> err
      Right result' -> pure result'
  where
    action session = do
      bind session appID side
      app session

    -- | Read messages from the websocket forever, or until we fail to handle one.
    readMessages session = do
      -- We read the message from the channel and handle it (either by setting
      -- the RPC response or forwarding to the mailbox message queue) all in
      -- one transaction. This means that if an exception occurs, the message
      -- will remain in the channel.
      msg <- receive session
      result <- atomically $ gotMessage session msg
      case result of
        Just err -> throwIO err
        Nothing -> readMessages session

-- | Make a request to the rendezvous server.
--
-- Will throw a 'ClientError' if there's a problem.
rpc :: HasCallStack
    => Session -- ^ A Magic Wormhole session. Get one using 'runClient'.
    -> Messages.ClientMessage -- ^ The RPC to send. Will fail with an 'Error' if this is not a valid RPC.
    -> IO Messages.ServerMessage -- ^ The result of an RPC
rpc session req =
  case expectedResponse req of
    Nothing ->
      -- XXX: Pretty sure we can juggle things around at the type level
      -- to remove this check. (i.e. make a type for RPC requests).
      throwIO (NotAnRPC req)
    Just responseType -> do
      box <- atomically $ expectResponse session responseType
      send session req
      response <- atomically $ waitForResponse session responseType box
      case response of
        Messages.Error reason original -> throwIO (BadRequest reason original)
        response' -> pure response'

-- | Set the application ID and side for the rest of this connection.
--
-- The Rendezvous protocol doesn't have a response to 'bind', so there's no
-- way to tell if it has had its effect.
--
-- See https://github.com/warner/magic-wormhole/issues/261
bind :: HasCallStack => Session -> Messages.AppID -> Messages.Side -> IO ()
bind session appID side' = send session (Messages.Bind appID side')

-- | Ping the server.
--
-- This is an in-band ping, used mostly for testing. It is not necessary to
-- keep the connection alive.
ping :: HasCallStack => Session -> Int -> IO Int
ping session n = do
  response <- rpc session (Messages.Ping n)
  case response of
    Messages.Pong n' -> pure n'
    unexpected -> unexpectedMessage (Messages.Ping n) unexpected

-- | List the nameplates on the server.
list :: HasCallStack => Session -> IO [Messages.Nameplate]
list session = do
  response <- rpc session Messages.List
  case response of
    Messages.Nameplates nameplates -> pure nameplates
    unexpected -> unexpectedMessage Messages.List unexpected

-- | Allocate a nameplate on the server.
allocate :: HasCallStack => Session -> IO Messages.Nameplate
allocate session = do
  response <- rpc session Messages.Allocate
  case response of
    Messages.Allocated nameplate -> pure nameplate
    unexpected -> unexpectedMessage Messages.Allocate unexpected

-- | Claim a nameplate on the server.
claim :: HasCallStack => Session -> Messages.Nameplate -> IO Messages.Mailbox
claim session nameplate = do
  response <- rpc session (Messages.Claim nameplate)
  case response of
    Messages.Claimed mailbox -> pure mailbox
    unexpected -> unexpectedMessage (Messages.Claim nameplate) unexpected

-- | Release a nameplate on the server.
--
-- TODO: Document semantics around "optional" nameplate.
--
-- TODO: Make this impossible to call unless we have already claimed a
-- namespace.
release :: HasCallStack => Session -> Maybe Messages.Nameplate -> IO ()
release session nameplate' = do
  response <- rpc session (Messages.Release nameplate')
  case response of
    Messages.Released -> pure ()
    unexpected -> unexpectedMessage (Messages.Release nameplate') unexpected

-- | Open a mailbox on the server.
--
-- If there's already a mailbox open, the server will send an error message.
-- In the current implementation, that error will arise in a strange and
-- unexpected place.
--
-- See https://github.com/warner/magic-wormhole/issues/261#issuecomment-343192449
open :: HasCallStack => Session -> Messages.Mailbox -> IO ClientProtocol.Connection
open session mailbox = do
  send session (Messages.Open mailbox)
  pure ClientProtocol.Connection { ClientProtocol.appID = sessionAppID session
                                 , ClientProtocol.ourSide = sessionSide session
                                 , ClientProtocol.send = add session
                                 , ClientProtocol.receive = readFromMailbox session
                                 }

-- | Close a mailbox on the server.
close :: HasCallStack => Session -> Maybe Messages.Mailbox -> Maybe Messages.Mood -> IO ()
close session mailbox' mood' = do
  response <- rpc session (Messages.Close mailbox' mood')
  case response of
    Messages.Closed -> pure ()
    unexpected -> unexpectedMessage (Messages.Close mailbox' mood') unexpected

-- | Send a message to the open mailbox.
--
-- XXX: Should we provide a version that blocks until the message comes back to us?
add :: HasCallStack => Session -> Messages.Phase -> Messages.Body -> IO ()
add session phase body = send session (Messages.Add phase body)

-- | Read a message from someone else from an open mailbox
--
-- Will block until we receive a message from someone who isn't us.
-- Specifically, a message with a different 'side' to us.
--
-- Will discard all messages that are from us.
readFromMailbox :: HasCallStack => Session -> STM Messages.MailboxMessage
readFromMailbox session = do
  msg <- readFromMailbox' session
  if Messages.side msg == sessionSide session
    then readFromMailbox session
    else pure msg

-- | Read a message from an open mailbox.
--
-- Will block if there's no message, or if we're in no state to receive
-- messages (e.g. no mailbox open).
readFromMailbox' :: HasCallStack => Session -> STM Messages.MailboxMessage
readFromMailbox' session = readTQueue (messageChan session)

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

-- | Tell the connection that we expect a response of the given type.
--
-- Will throw a 'ClientError' if we are already expecting a response of this type.
expectResponse :: Session -> ResponseType -> STM (TMVar Messages.ServerMessage)
expectResponse session responseType = do
  pending <- readTVar (pendingVar session)
  case HashMap.lookup responseType pending of
    Nothing -> do
      box <- newEmptyTMVar
      writeTVar (pendingVar session) (HashMap.insert responseType box pending)
      pure box
    Just _ -> throwSTM (AlreadySent responseType)

waitForResponse :: Session -> ResponseType -> TMVar Messages.ServerMessage -> STM Messages.ServerMessage
waitForResponse session responseType box = do
  response <- takeTMVar box
  modifyTVar' (pendingVar session) (HashMap.delete responseType)
  pure response

-- | Called when we have received a response from the server.
--
-- Tells anything waiting for the response that they can stop waiting now.
gotResponse :: Session -> ResponseType -> Messages.ServerMessage -> STM (Maybe ServerError)
gotResponse session responseType message = do
  pending <- readTVar (pendingVar session)
  case HashMap.lookup responseType pending of
    Nothing -> pure (Just (ResponseWithoutRequest responseType message))
    Just box -> do
      -- TODO: This will block processing messages from the server (by
      -- retrying the transaction) if the box is already populated (i.e. if we
      -- are in the middle of processing another response of the same type--a
      -- rare circumstance). I don't think we want that, but I'm not sure what
      -- behavior we do want.
      putTMVar box message
      pure Nothing

-- | Called when we receive a message (possibly a response) from the server.
gotMessage :: Session -> Messages.ServerMessage -> STM (Maybe ServerError)
gotMessage session msg =
  case msg of
    Messages.Ack -> pure Nothing  -- Skip Ack, because there's no point in handling it.
    Messages.Welcome welcome -> handleWelcome welcome
    Messages.Error{Messages.errorMessage, Messages.original} ->
      case expectedResponse original of
        Nothing -> pure (Just (ErrorForNonRequest errorMessage original))
        Just responseType ->
          -- TODO: This not quite right, as messages that aren't requests
          -- (e.g. 'open') can generate errors.
          gotResponse session responseType msg
    Messages.Message mailboxMsg -> do
      writeTQueue (messageChan session) mailboxMsg
      pure Nothing
    Messages.Nameplates{} -> gotResponse session NameplatesResponse msg
    Messages.Allocated{} -> gotResponse session AllocatedResponse msg
    Messages.Claimed{} -> gotResponse session ClaimedResponse msg
    Messages.Released -> gotResponse session ReleasedResponse msg
    Messages.Closed -> gotResponse session ClosedResponse msg
    Messages.Pong{} -> gotResponse session PongResponse msg

  where
    handleWelcome welcome =
      case Messages.welcomeErrorMessage welcome of
        Just err -> pure (Just (Unwelcome err))
        Nothing -> do
          notYet <- tryPutTMVar (motd session) (Messages.motd welcome)
          if notYet
            then pure Nothing
            else pure (Just (UnexpectedMessage (Messages.Welcome welcome)))

-- | The type of a response.
--
-- This is used pretty much only to match responses with requests.
--
-- XXX: Duplication with ServerMessage?
data ResponseType
  = NameplatesResponse
  | AllocatedResponse
  | ClaimedResponse
  | ReleasedResponse
  | ClosedResponse
  | PongResponse
  deriving (Eq, Show, Generic, Hashable)

-- XXX: This expectResponse stuff feels off to me. I think we could do something better.
--
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

-- | Error due to weirdness from the server.
data ServerError
  = -- | Server sent us a response for something that we hadn't requested.
    ResponseWithoutRequest ResponseType Messages.ServerMessage
    -- | We were sent a message other than "Welcome" on connect, or a
    -- "Welcome" message at any other time.
  | UnexpectedMessage Messages.ServerMessage
    -- | We received an 'error' message for a message that's not expected to
    -- have a response.
  | ErrorForNonRequest Text Messages.ClientMessage
  -- | Clients are not welcome on the server right now.
  | Unwelcome Text
    -- | We couldn't understand the message from the server.
  | ParseError String
  deriving (Eq, Show, Typeable)

instance Exception ServerError

-- | Error caused by misusing the client.
data ClientError
  = -- | We tried to do an RPC while another RPC with the same response type
    -- was in flight. See warner/magic-wormhole#260 for details.
    AlreadySent ResponseType
    -- | Tried to send a non-RPC as if it were an RPC (i.e. expecting a response).
  | NotAnRPC Messages.ClientMessage
    -- | We sent a message that the server could not understand.
  | BadRequest Text Messages.ClientMessage
  deriving (Eq, Show, Typeable)

instance Exception ClientError
