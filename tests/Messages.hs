-- | Tests for serializing and deserializing messages.
module Messages (tests) where

import Protolude

import Data.Aeson (encode, eitherDecode)
import Hedgehog (forAll, property, tripping)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

import qualified Generator

tests :: IO TestTree
tests = pure $ testGroup "Messages"
  [ testProperty "client messages roundtrip" $ property $ do
      x <- forAll Generator.clientMessages
      tripping x encode eitherDecode
  , testProperty "server messages roundtrip" $ property $ do
      x <- forAll Generator.serverMessages
      tripping x encode eitherDecode
  ]
