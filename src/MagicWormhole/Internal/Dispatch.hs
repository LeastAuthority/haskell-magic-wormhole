{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
module MagicWormhole.Internal.Dispatch
  ( ConnectionState
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
  , newTChan
  , readTChan
  , writeTChan
  )

import qualified MagicWormhole.Internal.Messages as Messages

import Data.Hashable (Hashable)
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HashMap
import Data.String (String)

data ConnectionState
  = ConnectionState
  { pendingVar :: TVar (HashMap ResponseType (TMVar Messages.ServerMessage))
  , inputChan :: TChan Messages.ServerMessage
  , outputChan :: TChan Messages.ClientMessage
  , messageChan :: TChan Messages.MailboxMessage -- XXX: Maybe make a queue
  , motd :: TMVar (Maybe Text)
  } deriving (Eq)

new :: TChan Messages.ServerMessage -> TChan Messages.ClientMessage -> STM ConnectionState
new inputChan outputChan
  = ConnectionState
  <$> newTVar mempty
  <*> pure inputChan
  <*> pure outputChan
  <*> newTChan
  <*> newEmptyTMVar

send :: ConnectionState -> Messages.ClientMessage -> STM ()
send connState = writeTChan (outputChan connState)

receive :: ConnectionState -> STM Messages.ServerMessage
receive connState = readTChan (inputChan connState)

-- | Get a message from mailbox. Will block if there's no message, or if we're
-- in no state to receive messages (e.g. no mailbox open).
readFromMailbox :: ConnectionState -> STM Messages.MailboxMessage
readFromMailbox connState = readTChan (messageChan connState)

with :: TChan Messages.ServerMessage -> TChan Messages.ClientMessage -> (ConnectionState -> IO a) -> IO (Either ServerError a)
with inputChan outputChan action = do
  connState <- atomically $ new inputChan outputChan
  race (readMessages connState) (action connState)
  where
    -- | Read messages from the input channel forever, or until we fail to handle one.
    readMessages connState = do
      -- XXX: Maybe one transaction for these?
      msg <- atomically $ receive connState
      result <- atomically $ gotMessage connState msg
      case result of
        Just err -> do
          putStrLn @Text $ "[ERROR] " <> show err
          pure err
        Nothing -> readMessages connState

rpc :: HasCallStack => ConnectionState -> Messages.ClientMessage -> IO (Either Error Messages.ServerMessage)
rpc connState req =
  case expectedResponse req of
    Nothing ->
      -- XXX: Pretty sure we can juggle things around at the type level
      -- to remove this check. (i.e. make a type for RPC requests).
      pure (Left (ClientError (NotAnRPC req)))
    Just responseType -> do
      box' <- atomically $ expectResponse connState responseType
      case box' of
        Left clientError -> pure (Left (ClientError clientError))
        Right box -> do
          atomically $ send connState req
          response <- atomically $ waitForResponse connState responseType box
          pure $ case response of
                   Messages.Error reason original -> Left (ClientError (BadRequest reason original))
                   response' -> Right response'

-- | Tell the connection that we expect a response of the given type.
--
-- Will fail with a 'ClientError' if we are already expecting a response of this type.
expectResponse :: ConnectionState -> ResponseType -> STM (Either ClientError (TMVar Messages.ServerMessage))
expectResponse connState responseType = do
  pending <- readTVar (pendingVar connState)
  case HashMap.lookup responseType pending of
    Nothing -> do
      box <- newEmptyTMVar
      writeTVar (pendingVar connState) (HashMap.insert responseType box pending)
      pure (Right box)
    Just _ -> pure (Left (AlreadySent responseType))

waitForResponse :: ConnectionState -> ResponseType -> TMVar Messages.ServerMessage -> STM Messages.ServerMessage
waitForResponse connState responseType box = do
  response <- takeTMVar box
  modifyTVar' (pendingVar connState) (HashMap.delete responseType)
  pure response

-- | Called when we have received a response from the server.
--
-- Tells anything waiting for the response that they can stop waiting now.
gotResponse :: ConnectionState -> ResponseType -> Messages.ServerMessage -> STM (Maybe ServerError)
gotResponse connState responseType message = do
  pending <- readTVar (pendingVar connState)
  case HashMap.lookup responseType pending of
    Nothing -> pure (Just (ResponseWithoutRequest responseType message))
    Just box -> do
      -- TODO: This will block reading from the server (by retrying the
      -- transaction) if the box is already populated. I don't think we want
      -- that, but I'm not sure what behavior we do want.
      putTMVar box message
      pure Nothing

-- | Called when we receive a message (possibly a response) from the server.
gotMessage :: ConnectionState -> Messages.ServerMessage -> STM (Maybe ServerError)
gotMessage connState msg =
  case msg of
    Messages.Ack -> pure Nothing  -- Skip Ack, because there's no point in handling it.
    Messages.Welcome welcome -> handleWelcome welcome
    Messages.Error{Messages.errorMessage, Messages.original} ->
      case expectedResponse original of
        Nothing -> pure (Just (ErrorForNonRequest errorMessage original))
        Just responseType -> gotResponse connState responseType msg
    Messages.Message mailboxMsg -> do
      writeTChan (messageChan connState) mailboxMsg
      pure Nothing
    Messages.Nameplates{} -> gotResponse connState NameplatesResponse msg
    Messages.Allocated{} -> gotResponse connState AllocatedResponse msg
    Messages.Claimed{} -> gotResponse connState ClaimedResponse msg
    Messages.Released -> gotResponse connState ReleasedResponse msg
    Messages.Closed -> gotResponse connState ClosedResponse msg
    Messages.Pong{} -> gotResponse connState PongResponse msg

  where
    handleWelcome welcome =
      case Messages.welcomeErrorMessage welcome of
        Just err -> pure (Just (Unwelcome err))
        Nothing -> do
          notYet <- tryPutTMVar (motd connState) (Messages.motd welcome)
          if notYet
            then pure Nothing
            else pure (Just (UnexpectedMessage (Messages.Welcome welcome)))

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
