-- | Builds on top of the client protocol to provide ordered sending and
-- receiving of messages to a peer.
--
-- TODO: "ApplicationProtocol" is a poor name. This is really the client
-- protocol still, and what's in ClientProtocol is / are the first internal
-- steps? the negotiation?
module MagicWormhole.Internal.ApplicationProtocol
  ( Session
  , withSession
  , sendMessage
  , receiveMessage
  ) where

import Protolude hiding (phase)

import Control.Concurrent.STM.TVar
  ( TVar
  , modifyTVar'
  , newTVar
  , readTVar
  )

import qualified MagicWormhole.Internal.ClientProtocol as ClientProtocol
import qualified MagicWormhole.Internal.Messages as Messages
import qualified MagicWormhole.Internal.Peer as Peer
import qualified MagicWormhole.Internal.Sequential as Sequential

-- | A Magic Wormhole peer-to-peer application session.
--
-- You get one of these after you have found a peer, successfully negotatiated
-- a shared key, and verified that negotiation by exchanging versions. (Note
-- that this does not include the "verifying" step mentioned in
-- magic-wormhole's documentation, which is about a human being verifying the
-- correctness of the code).
--
-- All messages in this session, sent & received, are encrypted using keys
-- derived from this shared key.
data Session
  = Session
  { connection :: Peer.Connection
  , key :: ClientProtocol.SessionKey
  , inbound :: Sequential.Sequential Int (Messages.Phase, ClientProtocol.PlainText)
  , outbound :: TVar Int
  }

-- | Take a successfully negotiated peer connection and run an action with an
-- active session, returning the result of the action.
--
-- Use this to communicate with a Magic Wormhole peer.
--
-- Once you have the session, use 'sendMessage' to send encrypted messages to
-- the peer, and 'receiveMessage' to received decrypted messages.
withSession
  :: Peer.Connection  -- ^ A connection to the other peer.
  -> ClientProtocol.SessionKey  -- ^ A successfully negotiated session key
  -> (Session -> IO a)  -- ^ The action to perform
  -> IO (Either ClientProtocol.Error a)  -- ^ The result of the action
withSession conn sessionKey action = do
  session <- atomically $ newSession conn sessionKey
  runSession session action

-- | Construct a new session.
newSession :: Peer.Connection -> ClientProtocol.SessionKey -> STM Session
newSession conn sessionKey = Session conn sessionKey <$> Sequential.sequenceBy getAppRank firstPhase <*> newTVar firstPhase
  where
    getAppRank (phase, _) =
      case phase of
        Messages.PakePhase -> panic "Did not expect PakePhase. Expected application phase."
        Messages.VersionPhase -> panic "Did not expect VersionPhase. Expected application phase."
        (Messages.ApplicationPhase n) -> n

    -- | The rank of the first phase we expect to send, and the first phase we
    -- expect to receive. It is critically important that this number is
    -- agreed on between peers, otherwise, a peer will wait forever for, say,
    -- message 0, which the other side has cheerily sent as message 1.
    firstPhase = 0

-- | Run an action inside a session.
--
-- Ensures that we are continually receiving messages from the peer, and
-- buffering them so the application /using/ the session can receive them in
-- order.
runSession :: Session -> (Session -> IO a) -> IO (Either ClientProtocol.Error a)
runSession session action = do
  result <- race readLoop (action session)
  pure $ case result of
           Left (Left readErr) -> Left readErr
           Left (Right _) -> panic "Cannot happen"
           Right r -> Right r
  where
    readLoop = do
      msg <- atomically $ ClientProtocol.receiveEncrypted (connection session) (key session)
      case msg of
        Left err -> pure $ Left err
        Right msg' -> do
          atomically $ Sequential.insert (inbound session) msg'
          readLoop

-- | Send an encrypted message to the peer.
--
-- Obtain a session with 'withSession'.
--
-- The message will be encrypted using a one-off key deriving from the shared
-- key.
sendMessage :: Session -> ClientProtocol.PlainText -> IO ()
sendMessage session body = do
  i <- atomically bumpPhase
  ClientProtocol.sendEncrypted (connection session) (key session) (Messages.ApplicationPhase i) body
  where
    bumpPhase = do
      i <- readTVar (outbound session)
      modifyTVar' (outbound session) (+1)
      pure i

-- | Receive a decrypted message from the peer.
--
-- Obtain a 'Session' with 'withSession'.
receiveMessage :: Session -> STM ClientProtocol.PlainText
receiveMessage session = snd <$> Sequential.next (inbound session)
