module Pake (tests) where

import Protolude hiding (phase)

import Hedgehog (forAll, property, tripping)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

import qualified MagicWormhole.Internal.Pake as Pake


tests :: IO TestTree
tests = pure $ testGroup "Pake"
  [ testProperty "SPAKE2 messages roundtrip" $ property $ do
      element <- forAll $ Gen.bytes (Range.singleton 32)
      tripping element Pake.spakeBytesToMessageBody Pake.messageBodyToSpakeBytes
  ]
