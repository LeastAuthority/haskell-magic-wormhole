-- | Test our interoperability with Python Magic Wormhole.
--
-- Some notes:
-- * it is the developer's / CI server's responsibility to provide a Python installation and a magic-wormhole installation
-- * we currently make no attempt to handle different versions of magic-wormhole
-- * if Python is not present, these tests will pass
-- * if magic-wormhole is not present, these tests will pass
module Integration (tests) where

import Protolude hiding (phase, stdin, stdout, toS)
import Protolude.Conv (toS)

import Control.Concurrent.STM.TChan
  ( newTChan
  , readTChan
  , writeTChan
  )
import qualified Crypto.Saltine.Class as Saltine
import qualified Crypto.Saltine.Core.SecretBox as SecretBox
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
import Test.Tasty.Hspec (testSpec)
import Test.Hspec(describe, it)
import Test.Hspec.Expectations (shouldBe)

import qualified Crypto.Spake2 as Spake2
import qualified MagicWormhole.Internal.ClientProtocol as ClientProtocol
import qualified MagicWormhole.Internal.Messages as Messages
import qualified MagicWormhole.Internal.Pake as Pake
import qualified MagicWormhole.Internal.Versions as Versions

import qualified Paths_magic_wormhole

tests :: IO TestTree
tests = testSpec "Integration" $ do
  describe "SPAKE2 and version exchange" $ do
    it "Generates the same SPAKE2 session key as the python-spake2 library" $ do
      let appID = "jml.io/haskell-magic-wormhole-test"
      let password = "mellon"
      let password' = Spake2.makePassword password
      let ourSide = "treebeard"
      let theirSide = "saruman"
      interactWithPython "tests/python/spake2_exchange.py"
        [ "--app-id=" <> toS appID
        , "--code=" <> toS password
        , "--side=" <> theirSide
        ] $ \stdin stdout -> do
          ClientProtocol.SessionKey sessionKey <- withConnection (Messages.AppID appID) (Messages.Side ourSide) stdin stdout $ \conn ->
            Pake.pakeExchange conn password'
          -- Calculate the shared key
          theirSpakeKey <- ByteString.hGetLine stdout
          theirSpakeKey `shouldBe` convertToBase Base16 sessionKey

    it "Derives the same phase keys" $ do
      fakeSpakeKey <- Gen.sample $ Gen.bytes (Range.singleton 32)
      let side = "treebeard"
      let phase = Messages.VersionPhase
      let ourPhaseKey = ClientProtocol.deriveKey
                        (ClientProtocol.SessionKey fakeSpakeKey)
                        (ClientProtocol.phasePurpose (Messages.Side side) phase)
      interactWithPython "tests/python/derive_phase_key.py"
        [ "--spake-key=" <> toS (convertToBase Base16 fakeSpakeKey :: ByteString)
        , "--side=" <> toS side
        , "--phase=" <> toS (Messages.phaseName phase)
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
          versions <- withConnection (Messages.AppID appID) (Messages.Side ourSide) stdin stdout $ \conn -> do
            sessionKey <- Pake.pakeExchange conn password'
            Versions.versionExchange conn sessionKey
          versions `shouldBe` Versions.Versions

  describe "NaCL interoperability" $
    it "Swaps messages with Python" $ do
      key <- SecretBox.newKey
      nonce <- SecretBox.newNonce
      interactWithPython "tests/python/nacl_exchange.py"
        [ "--key=" <> toS (convertToBase Base16 (Saltine.encode key) :: ByteString)
        , "--nonce=" <> toS (convertToBase Base16 (Saltine.encode nonce) :: ByteString)
        ] $ \stdin stdout -> do
          let message = ClientProtocol.PlainText "Hello world!"
          ClientProtocol.CipherText encryptedByUs <- ClientProtocol.encrypt key message
          Char8.hPutStrLn stdin (convertToBase Base16 encryptedByUs :: ByteString)
          decryptedByPython <- ByteString.hGetLine stdout
          ClientProtocol.PlainText decryptedByPython `shouldBe` message
          encryptedByPython <- ByteString.hGetLine stdout
          let Right encryptedBytes = convertFromBase Base16 encryptedByPython
          let Right decryptedByUs = ClientProtocol.decrypt key (ClientProtocol.CipherText encryptedBytes)
          decryptedByUs `shouldBe` message


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

withConnection :: Messages.AppID -> Messages.Side -> Handle -> Handle -> (ClientProtocol.Connection -> IO a) -> IO a
withConnection appID ourSide stdin stdout action = do
  inChan <- atomically newTChan
  let connection = ClientProtocol.Connection
                   { ClientProtocol.appID = appID
                   , ClientProtocol.ourSide = ourSide
                   , ClientProtocol.send = send
                   , ClientProtocol.receive = readTChan inChan
                   }
  result <- race (receiveForever inChan) (action connection)
  case result of
    Left err -> panic ("Couldn't read messages: " <> show @Text err)
    Right r -> pure r
  where
    send phase body =
      sendMailboxMessage stdin Messages.MailboxMessage { Messages.phase = phase
                                                       , Messages.side = ourSide
                                                       , Messages.body = body
                                                       , Messages.messageID = Nothing
                                                       }
    receiveForever inChan = forever $ do
      msg <- readMailboxMessage stdout
      atomically $ writeTChan inChan msg

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
