module Main (main) where

import Protolude

import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.Hedgehog (testProperty)

import qualified Rendezvous
import qualified WebSockets

main :: IO ()
main = sequence tests >>= defaultMain . testGroup "MagicWormhole"
  where
    tests =
      [ Rendezvous.tests
      , WebSockets.tests
      ]
