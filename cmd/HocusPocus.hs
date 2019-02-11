-- | Command-line tool for opening and communicating through magic wormholes.
--
-- Intended to inter-operate with the `wormhole` command-line tool from
-- [magic-wormhole](https://github.com/warner/magic-wormhole).
module Main (main) where

import Protolude

import qualified Options.Applicative as Opt

import qualified Crypto.Spake2 as Spake2
import qualified Data.Aeson as Aeson
import qualified Data.Text as Text

import qualified MagicWormhole

data Options
  = Options
  { cmd :: Command
  , rendezvousEndpoint :: MagicWormhole.WebSocketEndpoint
  } deriving (Eq, Show)

optionsParser :: Opt.Parser Options
optionsParser
  = Options
  <$> commandParser
  <*> Opt.option
        (Opt.maybeReader MagicWormhole.parseWebSocketEndpoint)
        ( Opt.long "rendezvous-url" <>
          Opt.help "Endpoint for the Rendezvous server" <>
          Opt.value defaultEndpoint <>
          Opt.showDefault )
  where
    -- | Default URI for rendezvous server.
    --
    -- This is Brian Warner's personal server.
    defaultEndpoint = fromMaybe (panic "Invalid default URL") (MagicWormhole.parseWebSocketEndpoint "ws://relay.magic-wormhole.io:4000/v1")


data Command
  = Send
  | Receive
  | Bounce  -- Send and receive to the same server.
  deriving (Eq, Show)

commandParser :: Opt.Parser Command
commandParser = Opt.hsubparser $
  Opt.command "send" (Opt.info (pure Send) (Opt.progDesc "Send a text message, file, or directory")) <>
  Opt.command "receive" (Opt.info (pure Receive) (Opt.progDesc "Receive a text message, file, or directory")) <>
  Opt.command "bounce" (Opt.info (pure Bounce) (Opt.progDesc "Send and receive a message through a server"))

makeOptions :: Text -> Opt.Parser a -> Opt.ParserInfo a
makeOptions headerText parser = Opt.info (Opt.helper <*> parser) (Opt.fullDesc <> Opt.header (toS headerText))

-- | A password used to exchange with a Magic Wormhole peer.
--
-- XXX: Just picking ByteString because that's the least amount of work. Need
-- to look up exact type of password in the magic-wormhole docs.
type Password = ByteString

-- | Send a text message to a Magic Wormhole peer.
sendText :: MagicWormhole.Session -> Password -> Text -> IO ()
sendText session password message = do
  nameplate <- MagicWormhole.allocate session
  mailbox <- MagicWormhole.claim session nameplate
  peer <- MagicWormhole.open session mailbox  -- XXX: We should run `close` in the case of exceptions?
  let (MagicWormhole.Nameplate n) = nameplate
  MagicWormhole.withEncryptedConnection peer (Spake2.makePassword (toS n <> "-" <> password))
    (\conn -> do
        let offer = MagicWormhole.Message message
        MagicWormhole.sendMessage conn (MagicWormhole.PlainText (toS (Aeson.encode offer))))

-- | Receive a text message from a Magic Wormhole peer.
receiveText :: MagicWormhole.Session -> IO Text
receiveText session = do
  nameplates <- MagicWormhole.list session
  putStrLn $ "Nameplates: " <> Text.intercalate ", " [ n | MagicWormhole.Nameplate n <- nameplates ]
  putText "Choose nameplate: "
  nameplate <- getLine
  mailbox <- MagicWormhole.claim session (MagicWormhole.Nameplate nameplate)
  peer <- MagicWormhole.open session mailbox
  putText "Password: "
  password <- getLine
  let fullPassword = toS nameplate <> "-" <> toS password
  MagicWormhole.withEncryptedConnection peer (Spake2.makePassword fullPassword)
    (\conn -> do
        MagicWormhole.PlainText received <- atomically $ MagicWormhole.receiveMessage conn
        case Aeson.eitherDecode (toS received) of
          Left err -> panic $ "Could not decode message: " <> show err
          Right (MagicWormhole.Message message) -> pure message
          Right (MagicWormhole.File _ _) -> pure $ "File transfer is not supported"
          Right (MagicWormhole.Directory _ _ _ _ _) -> pure $ "Directory transfer is not supported")

-- | Bounce a trivial message to and from a Rendezvous server.
bounce :: MagicWormhole.WebSocketEndpoint -> MagicWormhole.AppID -> IO ()
bounce endpoint appID = do
  side1 <- MagicWormhole.generateSide
  side2 <- MagicWormhole.generateSide
  MagicWormhole.runClient endpoint appID side1 Nothing $ \session1 -> do
    nameplate <- MagicWormhole.allocate session1
    mailbox1 <- MagicWormhole.claim session1 nameplate
    peer1 <- MagicWormhole.open session1 mailbox1
    MagicWormhole.runClient endpoint appID side2 Nothing $ \session2 -> do
      mailbox2 <- MagicWormhole.claim session2 nameplate
      peer2 <- MagicWormhole.open session2 mailbox2
      let message = "aoeu"
      (_, output) <- concurrently (send peer1 message) (receive peer2)
      unless (output == message) $ panic $ "Mismatched messages: " <> show message <> " != " <> show output
  where
    send peer message = MagicWormhole.withEncryptedConnection peer password $ \conn -> do
      let offer = MagicWormhole.Message message
      MagicWormhole.sendMessage conn (MagicWormhole.PlainText (toS (Aeson.encode offer)))

    receive peer = MagicWormhole.withEncryptedConnection peer password $ \conn -> do
      MagicWormhole.PlainText received <- atomically $ MagicWormhole.receiveMessage conn
      case Aeson.eitherDecode (toS received) of
        Left err -> panic $ "Could not decode message: " <> show err
        Right (MagicWormhole.Message message) -> pure message
        Right (MagicWormhole.File _ _) -> pure $ "File transfer is not supported"
        Right (MagicWormhole.Directory _ _ _ _ _) -> pure $ "Directory transfer is not supported"

    password = Spake2.makePassword "potato"


main :: IO ()
main = do
  options <- Opt.execParser (makeOptions "hocus-pocus - summon and traverse magic wormholes" optionsParser)
  side <- MagicWormhole.generateSide
  let endpoint = rendezvousEndpoint options
  case cmd options of
    Send -> MagicWormhole.runClient endpoint appID side Nothing $ \session ->
      sendText session "potato" "Brave new world that has such offers in it"
    Receive -> MagicWormhole.runClient endpoint appID side Nothing $ \session -> do
      message <- receiveText session
      putStr message
    Bounce -> bounce endpoint appID
  where
    appID = MagicWormhole.AppID "jml.io/hocus-pocus"
