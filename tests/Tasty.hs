module Main (main) where

import Protolude

import Test.Tasty (defaultMain, testGroup)

import qualified ClientProtocol
import qualified Integration
import qualified Messages
import qualified Sequential
import qualified WebSockets
import qualified FileTransfer

main :: IO ()
main = sequence tests >>= defaultMain . testGroup "MagicWormhole"
  where
    tests =
      [ ClientProtocol.tests
      , Messages.tests
      , FileTransfer.tests
      , Sequential.tests
      , WebSockets.tests
        -- Put these at the end because they are slowest.
      , Integration.tests
      ]
