module Peer (tests) where

import Protolude

import Hedgehog (MonadGen(..), forAll, property, tripping)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

import qualified Crypto.Spake2.Group as Group
import Crypto.Spake2.Groups (Ed25519(Ed25519))
import qualified MagicWormhole.Internal.Peer as Peer

import qualified Messages as MessagesTest

tests :: IO TestTree
tests = pure $ testGroup "Peer"
  [ testProperty "SPAKE2 messages roundtrip" $ property $ do
      appID <- forAll MessagesTest.appIDs
      let protocol = Peer.wormholeSpakeProtocol appID
      element <- forAll elements
      tripping element (Peer.encodeElement protocol) (Peer.decodeElement protocol)
  ]
  where
    elements :: MonadGen m => m (Group.Element Ed25519)
    elements = Group.arbitraryElement Ed25519 <$> Gen.bytes (Range.linear 0 10)
