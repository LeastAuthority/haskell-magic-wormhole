-- | Tests for the "client protocol".
module ClientProtocol (tests) where

import Protolude hiding (phase)

import Hedgehog (forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

import qualified MagicWormhole.Internal.ClientProtocol as ClientProtocol
import qualified MagicWormhole.Internal.Pake as Pake


tests :: IO TestTree
tests = pure $ testGroup "Peer"
  [ testProperty "SecretBox encryption roundtrips" $ property $ do
      purpose <- forAll $ Gen.bytes (Range.linear 0 10)
      secret <- forAll $ Gen.bytes (Range.linear 0 10)
      let key = ClientProtocol.deriveKey (Pake.SessionKey secret) purpose
      plaintext <- forAll $ Gen.bytes (Range.linear 1 256)
      ciphertext <- liftIO $ ClientProtocol.encrypt key plaintext
      let decrypted = ClientProtocol.decrypt key ciphertext
      decrypted === Right plaintext
  ]
