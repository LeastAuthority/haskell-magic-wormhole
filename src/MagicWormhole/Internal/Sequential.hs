-- | Helpers for processing messages in sequence.
module MagicWormhole.Internal.Sequential
  ( Sequential
  , sequenceBy
  , insert
  , next
  ) where

import Protolude

import Control.Concurrent.STM.TVar
  ( TVar
  , modifyTVar'
  , newTVar
  , readTVar
  )
import qualified Data.HashMap.Lazy as HashMap

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
--
-- If the counter has already been past, don't insert it, since we'll never
-- reach it. Instead return 'False'.
insert
  :: (Ord counter, Enum counter, Hashable counter, Eq counter)
  => Sequential counter a
  -> a
  -> STM Bool
insert sequential msg = do
  cur <- readTVar (current sequential)
  let msgRank = rank sequential msg
  if msgRank < cur
    then pure False
    else do
    modifyTVar' (buffer sequential) (HashMap.insert (rank sequential msg) msg)
    pure True

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
