-- | Test our interoperability with Python Magic Wormhole.
--
-- Some notes:
-- * it is the developer's / CI server's responsibility to provide a Python installation and a magic-wormhole installation
-- * we currently make no attempt to handle different versions of magic-wormhole
-- * if Python is not present, these tests will pass
-- * if magic-wormhole is not present, these tests will pass
module Integration (tests) where

import Protolude hiding (stdin, stdout)

import qualified Crypto.Spake2 as Spake2
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as ByteString
import qualified System.IO as IO
import qualified System.Process as Process
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (testSpec, describe, it, shouldBe)

import qualified MagicWormhole.Internal.Messages as Messages
import qualified MagicWormhole.Internal.Peer as Peer

import qualified Paths_magic_wormhole

tests :: IO TestTree
tests = testSpec "Integration" $
  describe "SPAKE2 and version exchange" $
    it "Works with our hacked-together Python implementation" $ do
      -- Launch the Python process
      -- If it fails due to command-not-found, print a warning and succeed
      -- Send it params
      -- concurrently sendSpake receiveSpake
      -- concurrently sendVersion receiveVersion
      let appID = "jml.io/haskell-magic-wormhole-test"
      let ourSide = "treebeard" :: Text
      let otherSide = "saruman" :: Text
      let password = "mellon"
      let password' = Spake2.makePassword password
      scriptExe <- Paths_magic_wormhole.getDataFileName "tests/python/version_exchange.py"
      let testScript = (Process.proc "python"
                       [ scriptExe
                       , "--app-id=" <> toS appID
                       , "--side=" <> toS otherSide
                       , "--code=" <> toS password
                       ]) { Process.std_in = Process.CreatePipe
                         , Process.std_out = Process.CreatePipe
                         , Process.std_err = Process.Inherit
                         }
      Process.withCreateProcess testScript $
        \(Just stdin) (Just stdout) _stderr ph -> do
          IO.hSetBuffering stdin IO.LineBuffering
          IO.hSetBuffering stdout IO.LineBuffering
          IO.hSetBuffering stderr IO.LineBuffering
          let protocol = Peer.wormholeSpakeProtocol (Messages.AppID appID)
          theirPake <- readFromHandle stdout
          let (Right inbound) = Peer.decodeElement protocol (Messages.body theirPake)
          exchange <- Spake2.startSpake2 protocol password'
          let outbound = Spake2.computeOutboundMessage exchange
          let keyMaterial = Spake2.generateKeyMaterial exchange inbound
          let sessionKey = Peer.SessionKey (Spake2.createSessionKey protocol inbound outbound keyMaterial password')
          sendToHandle stdin Messages.MailboxMessage
            { Messages.phase = Messages.PakePhase
            , Messages.side = Messages.Side ourSide
            , Messages.body = Peer.encodeElement protocol outbound
            , Messages.messageID = Nothing
            }
          theirVersions <- readFromHandle stdout
          let ourKey = Peer.deriveKey sessionKey (Peer.phasePurpose (Messages.Side ourSide) Messages.VersionPhase)
          encrypted <- Peer.encrypt ourKey (toS (Aeson.encode Peer.Versions))
          sendToHandle stdin Messages.MailboxMessage
            { Messages.phase = Messages.VersionPhase
            , Messages.side = Messages.Side ourSide
            , Messages.body = Messages.Body encrypted
            , Messages.messageID = Nothing
            }
          Messages.phase theirVersions `shouldBe` Messages.VersionPhase
          let (Messages.Body ciphertext) = Messages.body theirVersions
          let theirKey = Peer.deriveKey sessionKey (Peer.phasePurpose (Messages.Side otherSide) Messages.VersionPhase)
          -- XXX: Assert successful exit
          void $ Process.waitForProcess ph
          let Right plaintext = Peer.decrypt theirKey ciphertext
          let Right versions = Aeson.eitherDecode (toS plaintext)
          versions `shouldBe` Peer.Versions


readFromHandle :: HasCallStack => Handle -> IO Messages.MailboxMessage
readFromHandle h = do
  line <- ByteString.hGetLine h
  case Aeson.eitherDecode (toS line) of
    Left err -> do
      hPutStrLn stderr $ "Could not decode line: " <> line
      panic (toS err)
    Right (Messages.Message result) -> pure result
    Right other -> do
      hPutStrLn @Text stderr $ "Decoded line to non-MailboxMessage: " <> show other
      panic (show other)

sendToHandle :: HasCallStack => Handle -> Messages.MailboxMessage -> IO ()
sendToHandle h msg = hPutStrLn h (Aeson.encode (Messages.Message msg))
