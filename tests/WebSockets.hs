module WebSockets (tests) where

import Protolude

import Test.Tasty (TestTree)
import Test.Tasty.Hspec (testSpec)
import Test.Hspec(describe, it)
import Test.Hspec.Expectations(shouldBe)

import MagicWormhole.Internal.WebSockets
  ( WebSocketEndpoint(..)
  , parseWebSocketEndpoint
  )

tests :: IO TestTree
tests = testSpec "WebSockets" $
  describe "parser" $ do
    it "parses normal URLs" $ do
      let example = "ws://foo:80/bar"
      parseWebSocketEndpoint example `shouldBe` Just (WebSocketEndpoint "foo" 80 "/bar")
    it "ignores scheme, query, and fragment" $ do
      let example = "http://foo:80/bar?s=what#qux"
      parseWebSocketEndpoint example `shouldBe` Just (WebSocketEndpoint "foo" 80 "/bar")
    it "fails without a port" $ do
      let example = "http://foo/bar"
      parseWebSocketEndpoint example `shouldBe` Nothing
