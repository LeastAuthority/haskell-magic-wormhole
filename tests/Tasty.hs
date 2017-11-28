module Main (main) where

import Protolude

import Test.Tasty (defaultMain, testGroup)

import qualified Integration
import qualified Messages
import qualified Peer
import qualified WebSockets

main :: IO ()
main = sequence tests >>= defaultMain . testGroup "MagicWormhole"
  where
    tests =
      [ Integration.tests
      , Messages.tests
      , Peer.tests
      , WebSockets.tests
      ]
