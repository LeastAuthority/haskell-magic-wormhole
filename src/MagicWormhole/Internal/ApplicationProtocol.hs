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
import qualified Data.HashMap.Lazy as HashMap

import qualified MagicWormhole.Internal.ClientProtocol as ClientProtocol
import qualified MagicWormhole.Internal.Messages as Messages
import qualified MagicWormhole.Internal.Peer as Peer

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
  , inbound :: Sequential Int (Messages.Phase, ClientProtocol.PlainText)
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
newSession conn sessionKey = Session conn sessionKey <$> sequenceBy getAppRank 1 <*> newTVar 1
  where
    getAppRank (phase, _) =
      case phase of
        Messages.PakePhase -> panic "Did not expect PakePhase. Expected application phase."
        Messages.VersionPhase -> panic "Did not expect VersionPhase. Expected application phase."
        (Messages.ApplicationPhase n) -> n

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
          atomically $ insert (inbound session) msg'
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
receiveMessage session = snd <$> next (inbound session)

-- | A thing that gets numbered items and then buffers them so you can read
-- them in order without gaps.
--
-- "counter" is an 'Enum' that is used to determine the ordering of the
-- elements, and "item" is the type of the items themselves.
data Sequential counter item
  = Sequential
  { -- | The current value of the counter. The next item must have this rank.
    current :: TVar counter
  , -- | Where we store the items that have arrived out of order. A priority
    -- queue might be a better choice.
    buffer :: TVar (HashMap.HashMap counter item)
  , -- | How to rank items. Each item has a "rank", which is an 'Enum'
    -- (specifically, a "counter"). Items are yielded in order, without gaps,
    -- by 'next'.
    rank :: item -> counter
  }

-- | Create a 'Sequential' value for a series of item.
sequenceBy
  :: (Hashable counter, Eq counter)
  => (a -> counter) -- ^ How to rank items
  -> counter -- ^ The expected rank of the first item
  -> STM (Sequential counter a)
sequenceBy rank' initial = Sequential <$> newTVar initial <*> newTVar mempty <*> pure rank'

-- | Insert an item into the sequence. It may only be drawn from the
-- sequence with 'next' when its counter is next one.
insert :: (Enum counter, Hashable counter, Eq counter) => Sequential counter a -> a -> STM ()
insert sequential msg = modifyTVar' (buffer sequential) (HashMap.insert (rank sequential msg) msg)

-- | Get and remove the next item from the sequence. This will block until
-- there is an item with the exact rank we are expecting.
next
  :: (Enum counter, Eq counter, Hashable counter)
  => Sequential counter a  -- ^ How item are ordered in sequence
  -> STM a  -- ^ The next item
next thingy = do
  i <- readTVar (current thingy)
  q <- readTVar (buffer thingy)
  case HashMap.lookup i q of
    Nothing -> retry
    Just msg -> do
      modifyTVar' (current thingy) succ
      modifyTVar' (buffer thingy) (HashMap.delete i)
      pure msg
