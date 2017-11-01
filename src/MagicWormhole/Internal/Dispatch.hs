{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
module MagicWormhole.Internal.Dispatch
  ( ConnectionState
  , newConnectionState
  , gotMessage
  , expectedResponse
  , expectResponse
  , waitForResponse
  , ServerError(UnexpectedMessage)
  , AlreadySentError
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

import qualified MagicWormhole.Internal.Messages as Messages

import Data.Hashable (Hashable)
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HashMap

newtype ConnectionState = ConnectionState { _pendingVar :: TVar (HashMap ResponseType (TMVar Messages.ServerMessage)) } deriving (Eq)

-- | Initialize a new Magic Wormhole Rendezvous connection.
--
-- Will generally want to use 'connect' instead.
newConnectionState :: STM ConnectionState
newConnectionState = ConnectionState <$> newTVar mempty

-- | Tell the connection that we expect a response of the given type.
--
-- Will fail with a 'ClientError' if we are already expecting a response of this type.
expectResponse :: ConnectionState -> ResponseType -> STM (Either AlreadySentError (TMVar Messages.ServerMessage))
expectResponse (ConnectionState pendingVar) responseType = do
  pending <- readTVar pendingVar
  case HashMap.lookup responseType pending of
    Nothing -> do
      box <- newEmptyTMVar
      writeTVar pendingVar (HashMap.insert responseType box pending)
      pure (Right box)
    Just _ -> pure (Left (AlreadySent responseType))

waitForResponse :: ConnectionState -> ResponseType -> TMVar Messages.ServerMessage -> STM Messages.ServerMessage
waitForResponse (ConnectionState pendingVar) responseType box = do
  response <- takeTMVar box
  modifyTVar' pendingVar (HashMap.delete responseType)
  pure response

-- | Called when we have received a response from the server.
--
-- Tells anything waiting for the response that they can stop waiting now.
gotResponse :: ConnectionState -> ResponseType -> Messages.ServerMessage -> STM (Maybe ServerError)
gotResponse (ConnectionState pendingVar) responseType message = do
  pending <- readTVar pendingVar
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
  case getResponseType msg of
    Nothing ->
      case msg of
        Messages.Ack -> pure Nothing  -- Skip Ack, because there's no point in handling it.
        welcome@Messages.Welcome{} -> pure (Just (UnexpectedMessage welcome))
        err@Messages.Error{Messages.errorMessage, Messages.original} ->
          case expectedResponse original of
            Nothing -> pure (Just (ErrorForNonRequest errorMessage original))
            Just responseType -> gotResponse connState responseType err
        Messages.Message{} -> notImplemented  -- TODO: Implement message handling!
        _ -> panic $ "Impossible code. No response type for " <> show msg  -- XXX: Pretty sure we can design this away.
    Just responseType -> gotResponse connState responseType msg

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
  deriving (Eq, Show)

-- | Error caused by misusing the client.
newtype AlreadySentError
  = -- | We tried to do an RPC while another RPC with the same response type
    -- was in flight. See warner/magic-wormhole#260 for details.
    AlreadySent ResponseType
  deriving (Eq, Show)
