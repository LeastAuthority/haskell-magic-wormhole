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
  , readFromMailbox
    -- * Running a Rendezvous client
  , runClient
  ) where

import Protolude hiding (list, phase)

import Control.Concurrent.STM
  ( TChan
  , newTChan
  , readTChan
  , writeTChan
  )
import Data.Aeson (eitherDecode, encode)
import qualified Network.Socket as Socket
import qualified Network.WebSockets as WS

import qualified MagicWormhole.Internal.Messages as Messages
import qualified MagicWormhole.Internal.Dispatch as Dispatch
import MagicWormhole.Internal.WebSockets (WebSocketEndpoint(..))

-- TODO: A big problem throughout this code is that exceptions will get raised
-- (by websocket code, when threads are killed due to 'race', etc.), and this
-- means all our error handling gets ignored.
--
-- a) properly understand what's going on
-- b) (probably) either catch things or switch our stuff to exceptions
-- c) try to figure out some testing strategy

-- | Run a Magic Wormhole Rendezvous client.
--
-- Will fail with IO (Left ServerError) if the server declares we are unwelcome.
runClient :: HasCallStack => WebSocketEndpoint -> Messages.AppID -> Messages.Side -> (Dispatch.Session -> IO a) -> IO (Either Dispatch.ServerError a)
runClient (WebSocketEndpoint host port path) appID side' app = do
  inputChan <- atomically newTChan
  outputChan <- atomically newTChan
  map join $ Socket.withSocketsDo . WS.runClient host port path $ \ws -> do
    result <- race (race (socketToChan ws inputChan) (chanToSocket ws outputChan))
                   (Dispatch.with inputChan outputChan action)
    pure $ case result of
             Left (Left readErr) -> Left readErr
             Left (Right writeErr) -> Left writeErr
             Right result' -> result'
  where
    action session = do
      bind session appID side'
      Right <$> app session

    socketToChan :: HasCallStack => WS.Connection -> TChan Messages.ServerMessage -> IO Dispatch.ServerError
    socketToChan ws chan = do
      -- XXX: I think we need to catch `CloseRequest` here and gracefully stop.
      bytes <- WS.receiveData ws
      case eitherDecode bytes of
        Left err -> do
          putStrLn @Text $ "[ERROR] " <> show err
          pure (Dispatch.ParseError err)
        Right msg -> do
          putStrLn @Text $ "<<< " <> show msg  -- XXX: Debug
          atomically $ writeTChan chan msg
          socketToChan ws chan

    chanToSocket :: HasCallStack => WS.Connection -> TChan Messages.ClientMessage -> IO b
    chanToSocket ws chan = forever $ do
      msg <- atomically $ readTChan chan
      -- XXX: I think this needs to use `sendClose` when the message is Close.
      WS.sendBinaryData ws (encode msg)
      putStrLn @Text $ ">>> " <> show msg -- XXX: Debug

-- | Make a request to the rendezvous server.
rpc :: HasCallStack => Dispatch.Session -> Messages.ClientMessage -> IO (Either Dispatch.Error Messages.ServerMessage)
rpc = Dispatch.rpc

-- | Set the application ID and side for the rest of this connection.
--
-- The Rendezvous protocol doesn't have a response to 'bind', so there's no
-- way to tell if it has had its effect.
--
-- See https://github.com/warner/magic-wormhole/issues/261
bind :: HasCallStack => Dispatch.Session -> Messages.AppID -> Messages.Side -> IO ()
bind session appID side' = atomically $ Dispatch.send session (Messages.Bind appID side')

-- | Ping the server.
--
-- This is an in-band ping, used mostly for testing. It is not necessary to
-- keep the connection alive.
ping :: HasCallStack => Dispatch.Session -> Int -> IO (Either Dispatch.Error Int)
ping session n = do
  response <- rpc session (Messages.Ping n)
  pure $ case response of
    Left err -> Left err
    Right (Messages.Pong n') -> Right n'
    Right unexpected -> unexpectedMessage (Messages.Ping n) unexpected

-- | List the nameplates on the server.
list :: HasCallStack => Dispatch.Session -> IO (Either Dispatch.Error [Messages.Nameplate])
list session = do
  response <- rpc session Messages.List
  pure $ case response of
    Left err -> Left err
    Right (Messages.Nameplates nameplates) -> Right nameplates
    Right unexpected -> unexpectedMessage Messages.List unexpected

-- | Allocate a nameplate on the server.
allocate :: HasCallStack => Dispatch.Session -> IO (Either Dispatch.Error Messages.Nameplate)
allocate session = do
  response <- rpc session Messages.Allocate
  pure $ case response of
    Left err -> Left err
    Right (Messages.Allocated nameplate) -> Right nameplate
    Right unexpected -> unexpectedMessage Messages.Allocate unexpected

-- | Claim a nameplate on the server.
claim :: HasCallStack => Dispatch.Session -> Messages.Nameplate -> IO (Either Dispatch.Error Messages.Mailbox)
claim session nameplate = do
  response <- rpc session (Messages.Claim nameplate)
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
release :: HasCallStack => Dispatch.Session -> Maybe Messages.Nameplate -> IO (Either Dispatch.Error ())
release session nameplate' = do
  response <- rpc session (Messages.Release nameplate')
  pure $ case response of
    Left err -> Left err
    Right Messages.Released -> Right ()
    Right unexpected -> unexpectedMessage (Messages.Release nameplate') unexpected

-- | Open a mailbox on the server.
--
-- If there's already a mailbox open, the server will send an error message.
-- In the current implementation, that error will arise in a strange and
-- unexpected place.
--
-- See https://github.com/warner/magic-wormhole/issues/261#issuecomment-343192449
open :: HasCallStack => Dispatch.Session -> Messages.Mailbox -> IO ()
open session mailbox = atomically $ Dispatch.send session (Messages.Open mailbox)

-- | Close a mailbox on the server.
close :: HasCallStack => Dispatch.Session -> Maybe Messages.Mailbox -> Maybe Messages.Mood -> IO (Either Dispatch.Error ())
close session mailbox' mood' = do
  response <- rpc session (Messages.Close mailbox' mood')
  pure $ case response of
    Left err -> Left err
    Right Messages.Closed -> Right ()
    Right unexpected -> unexpectedMessage (Messages.Close mailbox' mood') unexpected

-- | Send a message to the open mailbox.
--
-- XXX: Should we provide a version that blocks until the message comes back to us?
add :: HasCallStack => Dispatch.Session -> Messages.Phase -> Messages.Body -> IO ()
add session phase body = atomically $ Dispatch.send session (Messages.Add phase body)

-- | Read a message from an open mailbox.
readFromMailbox :: HasCallStack => Dispatch.Session -> IO Messages.MailboxMessage
readFromMailbox = atomically . Dispatch.readFromMailbox

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
