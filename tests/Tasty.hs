module Main (main) where

import Protolude

import Hedgehog ((===), forAll, property)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.Hedgehog (testProperty)

import qualified MagicWormhole

main :: IO ()
main = defaultMain tests
  where
    tests = testGroup "magic-wormhole"
      [ testProperty "commutativity" $ property $ do
          x <- forAll $ Gen.int (Range.linear (-1000) 1000)
          MagicWormhole.foo + x === x + MagicWormhole.foo
      ]
