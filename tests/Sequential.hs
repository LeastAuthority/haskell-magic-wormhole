-- | Tests for sequential sort of messages.
module Sequential (tests) where

import Protolude

import Hedgehog (forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

import qualified MagicWormhole.Internal.Sequential as Sequential

tests :: IO TestTree
tests = pure $ testGroup "Sequential"
  [ testProperty "exhausts items in order" $ property $ do
      -- Sequential with an arbitrary start point that gets the rank from the
      -- first element of a tuple.
      start <- forAll (Gen.integral (Range.linear 0 100))
      sequential <- liftIO (atomically (Sequential.sequenceBy fst start))
      -- An arbitrary list of messages...
      messages <- forAll (Gen.list (Range.linear 0 100)  (Gen.text (Range.linear 0 100) Gen.unicode))
      -- ... ranked from our arbitrary start...
      let allMessages = zip [start..start+length messages] messages
      -- ... in an arbitrary order.
      shuffled <- forAll $ Gen.shuffle allMessages
      -- Insert them all.
      traverse_ (liftIO . atomically . Sequential.insert sequential) shuffled
      -- Pull them all out.
      items <- replicateM (length messages) (liftIO . atomically $ Sequential.next sequential)
      items === allMessages
  ]
