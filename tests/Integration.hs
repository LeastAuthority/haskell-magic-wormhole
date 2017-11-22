-- | Test our interoperability with Python Magic Wormhole.
--
-- Some notes:
-- * it is the developer's / CI server's responsibility to provide a Python installation and a magic-wormhole installation
-- * we currently make no attempt to handle different versions of magic-wormhole
-- * if Python is not present, these tests will pass
-- * if magic-wormhole is not present, these tests will pass
module Integration (tests) where

import Protolude hiding (stdin, stdout)

import qualified Data.Aeson as Aeson
import Data.ByteArray.Encoding (convertFromBase, convertToBase, Base(Base16))
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as Char8
import qualified System.IO as IO
import qualified System.Process as Process
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (testSpec, describe, it, shouldBe)

import qualified Crypto.Spake2 as Spake2
import qualified MagicWormhole.Internal.Messages as Messages
import qualified MagicWormhole.Internal.Peer as Peer

import qualified Paths_magic_wormhole

tests :: IO TestTree
tests = testSpec "Integration" $
  describe "SPAKE2 and version exchange" $ do
    it "Generates the same SPAKE2 session key" $ do
      let appID = "jml.io/haskell-magic-wormhole-test"
      let password = "mellon"
      let password' = Spake2.makePassword password
      scriptExe <- Paths_magic_wormhole.getDataFileName "tests/python/spake2_exchange.py"
      let testScript = (Process.proc "python"
                       [ scriptExe
                       , "--app-id=" <> toS appID
                       , "--code=" <> toS password
                       ]) { Process.std_in = Process.CreatePipe
                         , Process.std_out = Process.CreatePipe
                         , Process.std_err = Process.Inherit  -- So we get stack traces printed during test runs.
                         }
      Process.withCreateProcess testScript $
        \(Just stdin) (Just stdout) _stderr ph -> do
          -- The inter-process protocol is line-based.
          IO.hSetBuffering stdin IO.LineBuffering
          IO.hSetBuffering stdout IO.LineBuffering
          IO.hSetBuffering stderr IO.LineBuffering
          let protocol = Peer.wormholeSpakeProtocol (Messages.AppID appID)
          -- Receive their SPAKE2 material
          theirPakeHex <- ByteString.hGetLine stdout
          let Right theirPake = convertFromBase Base16 theirPakeHex
          let Right inbound = Spake2.extractElement protocol theirPake
          exchange <- Spake2.startSpake2 protocol password'
          -- Send our SPAKE2 material
          let outbound = Spake2.computeOutboundMessage exchange
          Char8.hPutStrLn stdin (convertToBase Base16 (Spake2.elementToMessage protocol outbound))
          -- Calculate the shared key
          let keyMaterial = Spake2.generateKeyMaterial exchange inbound
          let sessionKey = Spake2.createSessionKey protocol inbound outbound keyMaterial password'
          theirSpakeKey <- ByteString.hGetLine stdout
          -- Wait for the process to finish so we can get full stack trace in case of error.
          void $ Process.waitForProcess ph
          theirSpakeKey `shouldBe` convertToBase Base16 sessionKey

    it "Works with our hacked-together Python implementation" $ do
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
          -- The inter-process protocol is line-based.
          IO.hSetBuffering stdin IO.LineBuffering
          IO.hSetBuffering stdout IO.LineBuffering
          IO.hSetBuffering stderr IO.LineBuffering
          let protocol = Peer.wormholeSpakeProtocol (Messages.AppID appID)
          -- Receive their SPAKE2 material
          theirPake <- readFromHandle stdout
          let (Right inbound) = Peer.decodeElement protocol (Messages.body theirPake)
          -- Send our SPAKE2 material
          exchange <- Spake2.startSpake2 protocol password'
          let outbound = Spake2.computeOutboundMessage exchange
          sendToHandle stdin Messages.MailboxMessage
            { Messages.phase = Messages.PakePhase
            , Messages.side = Messages.Side ourSide
            , Messages.body = Peer.encodeElement protocol outbound
            , Messages.messageID = Nothing
            }
          -- Calculate the shared session key
          let keyMaterial = Spake2.generateKeyMaterial exchange inbound
          let sessionKey = Peer.SessionKey (Spake2.createSessionKey protocol inbound outbound keyMaterial password')
          -- Receive their versions message
          theirVersions <- readFromHandle stdout
          -- Send our versions message
          let ourKey = Peer.deriveKey sessionKey (Peer.phasePurpose (Messages.Side ourSide) Messages.VersionPhase)
          encrypted <- Peer.encrypt ourKey (toS (Aeson.encode Peer.Versions))
          sendToHandle stdin Messages.MailboxMessage
            { Messages.phase = Messages.VersionPhase
            , Messages.side = Messages.Side ourSide
            , Messages.body = Messages.Body encrypted
            , Messages.messageID = Nothing
            }
          Messages.phase theirVersions `shouldBe` Messages.VersionPhase
          -- Wait for the process to end so we get full stack trace, if any.
          -- XXX: Assert successful exit
          void $ Process.waitForProcess ph
          -- Decrypt their versions message.
          let (Messages.Body ciphertext) = Messages.body theirVersions
          let theirKey = Peer.deriveKey sessionKey (Peer.phasePurpose (Messages.Side otherSide) Messages.VersionPhase)
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
