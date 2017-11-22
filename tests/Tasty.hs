module Main (main) where

import Protolude

import Test.Tasty (defaultMain, testGroup)

import qualified Messages
import qualified Peer
import qualified WebSockets

main :: IO ()
main = sequence tests >>= defaultMain . testGroup "MagicWormhole"
  where
    tests =
      [ Messages.tests
      , Peer.tests
      , WebSockets.tests
      ]
