{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
module MagicWormhole.Internal.Dispatch
  ( Session
  , Error
  , ServerError(..)
  , rpc
  , send
  , readFromMailbox
  , with
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
  , tryPutTMVar
  , TChan
  , readTChan
  , writeTChan
  , TQueue
  , newTQueue
  , readTQueue
  , writeTQueue
  )

import qualified MagicWormhole.Internal.Messages as Messages

import Data.Hashable (Hashable)
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HashMap
import Data.String (String)

-- | Abstract type representing a Magic Wormhole session.
--
-- - 'with' gets a session
-- - 'rpc' sends RPCs from inside 'with'
-- - 'send' sends non-RPC messages (e.g. `bind`)
-- - 'readFromMailbox' reads messages from the mailbox
data Session
  = Session
  { pendingVar :: TVar (HashMap ResponseType (TMVar Messages.ServerMessage))
  , inputChan :: TChan Messages.ServerMessage
  , outputChan :: TChan Messages.ClientMessage
  , messageChan :: TQueue Messages.MailboxMessage
  , motd :: TMVar (Maybe Text)
  } deriving (Eq)

-- | Create a new 'Session'.
--
-- Requires an input channel and an output channel. Assumes those channels are
-- connected to a Magic Wormhole Rendezvous server.
new :: TChan Messages.ServerMessage -- ^ Input channel that gets messages from the server
    -> TChan Messages.ClientMessage -- ^ Output channel that sends messages to the server
    -> STM Session  -- ^ Opaque 'Session' object.
new inputChan outputChan
  = Session
  <$> newTVar mempty
  <*> pure inputChan
  <*> pure outputChan
  <*> newTQueue
  <*> newEmptyTMVar

-- | Send a message to a Magic Wormhole Rendezvous server.
send :: Session -- ^ An active session. Get this using 'with'.
     -> Messages.ClientMessage -- ^ Message to send to the server.
     -> STM ()
send session = writeTChan (outputChan session)

-- | Receive a message from a Magic Wormhole Rendezvous server.
-- Blocks until such a message exists.
receive :: Session -- ^ An active session. Get this using 'with'.
        -> STM Messages.ServerMessage -- ^ Next message from the server.
receive session = readTChan (inputChan session)

-- | Get a message from mailbox. Will block if there's no message, or if we're
-- in no state to receive messages (e.g. no mailbox open).
readFromMailbox :: Session -> STM Messages.MailboxMessage
readFromMailbox session = readTQueue (messageChan session)

-- | Run an action inside a Magic Wormhole session. Use this to interact with a Magic Wormhole server.
with :: TChan Messages.ServerMessage -- ^ Channel that receives messages from the server. The session will read from this.
     -> TChan Messages.ClientMessage -- ^ Channel that sends messages to the server. The session will write to this.
     -> (Session -> IO a) -- ^ Action to perform while we are in a Magic Wormhole session. See 'send', 'rpc', and 'readFromMailbox'.
     -> IO (Either ServerError a) -- ^ Either the result of the action or a 'ServerError' if we encountered problems.
with inputChan outputChan action = do
  session <- atomically $ new inputChan outputChan
  race (readMessages session) (action session)
  where
    -- | Read messages from the input channel forever, or until we fail to handle one.
    readMessages session = do
      -- We read the message from the channel and handle it (either by setting
      -- the RPC response or forwarding to the mailbox message queue) all in
      -- one transaction. This means that if an exception occurs, the message
      -- will remain in the channel.
      result <- atomically $ do
        msg <- receive session
        gotMessage session msg
      case result of
        Just err -> do
          putStrLn @Text $ "[ERROR] " <> show err
          pure err
        Nothing -> readMessages session

rpc :: HasCallStack
    => Session -- ^ A Magic Wormhole session. Get one using 'with'.
    -> Messages.ClientMessage -- ^ The RPC to send. Will fail with an 'Error' if this is not a valid RPC.
    -> IO (Either Error Messages.ServerMessage) -- ^ Either the result of an RPC, or some sort of error happened.
rpc session req =
  case expectedResponse req of
    Nothing ->
      -- XXX: Pretty sure we can juggle things around at the type level
      -- to remove this check. (i.e. make a type for RPC requests).
      pure (Left (ClientError (NotAnRPC req)))
    Just responseType -> do
      box' <- atomically $ expectResponse session responseType
      case box' of
        Left clientError -> pure (Left (ClientError clientError))
        Right box -> do
          atomically $ send session req
          response <- atomically $ waitForResponse session responseType box
          pure $ case response of
                   Messages.Error reason original -> Left (ClientError (BadRequest reason original))
                   response' -> Right response'

-- | Tell the connection that we expect a response of the given type.
--
-- Will fail with a 'ClientError' if we are already expecting a response of this type.
expectResponse :: Session -> ResponseType -> STM (Either ClientError (TMVar Messages.ServerMessage))
expectResponse session responseType = do
  pending <- readTVar (pendingVar session)
  case HashMap.lookup responseType pending of
    Nothing -> do
      box <- newEmptyTMVar
      writeTVar (pendingVar session) (HashMap.insert responseType box pending)
      pure (Right box)
    Just _ -> pure (Left (AlreadySent responseType))

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
        Just responseType -> gotResponse session responseType msg
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

-- | Any possible dispatch error.
data Error
  = -- | An error due to misusing the client.
    ClientError ClientError
    -- | An error due to weird message from the server.
  | ServerError ServerError deriving (Eq, Show)
