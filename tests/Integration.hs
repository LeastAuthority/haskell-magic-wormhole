-- | Test our interoperability with Python Magic Wormhole.
--
-- Some notes:
-- * it is the developer's / CI server's responsibility to provide a Python installation and a magic-wormhole installation
-- * we currently make no attempt to handle different versions of magic-wormhole
-- * if Python is not present, these tests will pass
-- * if magic-wormhole is not present, these tests will pass
module Integration (tests) where

import Protolude hiding (phase, stdin, stdout)

import qualified Crypto.Saltine.Class as Saltine
import qualified Data.Aeson as Aeson
import Data.ByteArray.Encoding (convertFromBase, convertToBase, Base(Base16))
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as Char8
import Data.String (String)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
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
      interactWithPython "tests/python/spake2_exchange.py"
        [  "--app-id=" <> toS appID
        , "--code=" <> toS password
        ] $ \stdin stdout -> do
          let protocol = Peer.wormholeSpakeProtocol (Messages.AppID appID)
          Right sessionKey <- Spake2.spake2Exchange protocol password'
                              (Char8.hPutStrLn stdin . convertToBase Base16)
                              (convertFromBase Base16 <$> ByteString.hGetLine stdout)
          -- Calculate the shared key
          theirSpakeKey <- ByteString.hGetLine stdout
          theirSpakeKey `shouldBe` convertToBase Base16 sessionKey

    it "Derives the same phase keys" $ do
      fakeSpakeKey <- Gen.sample $ Gen.bytes (Range.singleton 32)
      let side = "treebeard"
      let phase = Messages.VersionPhase
      let ourPhaseKey = Peer.deriveKey
                        (Peer.SessionKey fakeSpakeKey)
                        (Peer.phasePurpose (Messages.Side side) phase)
      interactWithPython "tests/python/derive_phase_key.py"
        [ "--spake-key=" <> toS (convertToBase Base16 fakeSpakeKey :: ByteString)
        , "--side=" <> toS side
        , "--phase=" <> toS (Aeson.encode phase)
        ] $ \_stdin stdout -> do
          theirPhaseKey <- ByteString.hGetLine stdout
          theirPhaseKey `shouldBe` convertToBase Base16 (Saltine.encode ourPhaseKey)

    it "Works with our hacked-together Python implementation" $ do
      let appID = "jml.io/haskell-magic-wormhole-test"
      let ourSide = "treebeard" :: Text
      let otherSide = "saruman" :: Text
      let password = "mellon"
      let password' = Spake2.makePassword password
      interactWithPython "tests/python/version_exchange.py"
        [ "--app-id=" <> toS appID
        , "--side=" <> toS otherSide
        , "--code=" <> toS password
        ] $ \stdin stdout -> do
          let protocol = Peer.wormholeSpakeProtocol (Messages.AppID appID)
          Right sessionKey <- Peer.SessionKey <<$>> Spake2.spake2Exchange protocol password'
            (sendPakeBytes stdin ourSide) (receivePakeBytes stdout)
          -- Receive their versions message
          theirVersions <- readMailboxMessage stdout
          -- Send our versions message
          let ourKey = Peer.deriveKey sessionKey (Peer.phasePurpose (Messages.Side ourSide) Messages.VersionPhase)
          encrypted <- Peer.encrypt ourKey (toS (Aeson.encode Peer.Versions))
          sendMailboxMessage stdin Messages.MailboxMessage
            { Messages.phase = Messages.VersionPhase
            , Messages.side = Messages.Side ourSide
            , Messages.body = Messages.Body encrypted
            , Messages.messageID = Nothing
            }
          Messages.phase theirVersions `shouldBe` Messages.VersionPhase
          -- Decrypt their versions message.
          let (Messages.Body ciphertext) = Messages.body theirVersions
          let theirKey = Peer.deriveKey sessionKey (Peer.phasePurpose (Messages.Side otherSide) Messages.VersionPhase)
          let Right plaintext = Peer.decrypt theirKey ciphertext
          let Right versions = Aeson.eitherDecode (toS plaintext)
          versions `shouldBe` Peer.Versions


-- | Run a Python script and interact with it by sending stuff to its stdin
-- and reading from its stdout using a line-based protocol.
--
-- The Python process's stderr will inherit from this one, so we get Python
-- stack traces in our test runner output. The interaction won't terminate
-- until the Python process does, so that we can get full output from it,
-- especially in the case of errors.
interactWithPython
  :: FilePath -- ^ Name of the script to run
  -> [String] -- ^ Arguments to the script
  -> (Handle -> Handle -> IO a)  -- ^ Interaction with the script, params are stdin & stdout.
  -> IO a  -- ^ Result of the interaction.
interactWithPython name args action = do
  scriptExe <- Paths_magic_wormhole.getDataFileName name
  let testScript = (Process.proc "python" (scriptExe:args))
                   { Process.std_in = Process.CreatePipe
                   , Process.std_out = Process.CreatePipe
                   , Process.std_err = Process.Inherit
                   }
  Process.withCreateProcess testScript $
    \(Just stdin) (Just stdout) _stderr ph -> do
      IO.hSetBuffering stdin IO.LineBuffering
      IO.hSetBuffering stdout IO.LineBuffering
      IO.hSetBuffering stderr IO.LineBuffering
      action stdin stdout `finally` Process.waitForProcess ph


-- | Send SPAKE2 bytes as a mailbox message to a file handle.
sendPakeBytes :: Handle -> Text -> ByteString -> IO ()
sendPakeBytes stdin ourSide pakeBytes = do
  let body = Peer.spakeBytesToMessageBody pakeBytes
  sendMailboxMessage stdin Messages.MailboxMessage
    { Messages.phase = Messages.PakePhase
    , Messages.side = Messages.Side ourSide
    , Messages.body = body
    , Messages.messageID = Nothing
    }

-- | Receive SPAKE2 bytes as a mailbox message from a file handle.
receivePakeBytes :: Handle -> IO (Either Text ByteString)
receivePakeBytes stdout = do
  theirMessage <- readMailboxMessage stdout
  Messages.phase theirMessage `shouldBe` Messages.PakePhase
  pure . Peer.messageBodyToSpakeBytes . Messages.body $ theirMessage


readMailboxMessage :: HasCallStack => Handle -> IO Messages.MailboxMessage
readMailboxMessage h = do
  line <- ByteString.hGetLine h
  case Aeson.eitherDecode (toS line) of
    Left err -> do
      hPutStrLn stderr $ "Could not decode line: " <> line
      panic (toS err)
    Right (Messages.Message result) -> pure result
    Right other -> do
      hPutStrLn @Text stderr $ "Decoded line to non-MailboxMessage: " <> show other
      panic (show other)

sendMailboxMessage :: HasCallStack => Handle -> Messages.MailboxMessage -> IO ()
sendMailboxMessage h msg = hPutStrLn h (Aeson.encode (Messages.Message msg))
