module Main (main) where

import Protolude

import Test.Tasty (defaultMain, testGroup)

import qualified Messages
import qualified WebSockets

main :: IO ()
main = sequence tests >>= defaultMain . testGroup "MagicWormhole"
  where
    tests =
      [ Messages.tests
      , WebSockets.tests
      ]
