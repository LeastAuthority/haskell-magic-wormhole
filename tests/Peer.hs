-- | Tests for the "client protocol".
module Peer (tests) where

import Protolude hiding (phase)

import Hedgehog (forAll, property, tripping, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

import qualified MagicWormhole.Internal.Peer as Peer


tests :: IO TestTree
tests = pure $ testGroup "Peer"
  [ testProperty "SPAKE2 messages roundtrip" $ property $ do
      element <- forAll $ Gen.bytes (Range.singleton 32)
      tripping element Peer.spakeBytesToMessageBody Peer.messageBodyToSpakeBytes
  , testProperty "SecretBox encryption roundtrips" $ property $ do
      purpose <- forAll $ Gen.bytes (Range.linear 0 10)
      secret <- forAll $ Gen.bytes (Range.linear 0 10)
      let key = Peer.deriveKey (Peer.SessionKey secret) purpose
      plaintext <- forAll $ Gen.bytes (Range.linear 1 256)
      ciphertext <- liftIO $ Peer.encrypt key plaintext
      let decrypted = Peer.decrypt key ciphertext
      decrypted === Right plaintext
  ]
