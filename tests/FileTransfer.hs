-- | Tests for serializing and deserializing FileTransfer messages
module FileTransfer (tests) where

import Protolude

import Data.Aeson (encode, eitherDecode)
import Hedgehog (forAll, property, tripping)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

import qualified Generator

tests :: IO TestTree
tests = pure $ testGroup "FileTransfer"
  [ testProperty "offer messages roundtrip" $ property $ do
      x <- forAll Generator.offerMessages
      tripping x encode eitherDecode
  ]
