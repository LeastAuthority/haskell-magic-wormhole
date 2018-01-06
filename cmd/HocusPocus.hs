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
import qualified MagicWormhole.Internal.ClientProtocol as ClientProtocol
import qualified MagicWormhole.Internal.FileTransfer as FileTransfer
import qualified MagicWormhole.Internal.Messages as Messages
import qualified MagicWormhole.Internal.Peer as Peer
import qualified MagicWormhole.Internal.Rendezvous as Rendezvous
import MagicWormhole.Internal.WebSockets (WebSocketEndpoint(..), parseWebSocketEndpoint)

data Options
  = Options
  { cmd :: Command
  , rendezvousEndpoint :: WebSocketEndpoint
  } deriving (Eq, Show)

optionsParser :: Opt.Parser Options
optionsParser
  = Options
  <$> commandParser
  <*> Opt.option
        (Opt.maybeReader parseWebSocketEndpoint)
        ( Opt.long "rendezvous-url" <>
          Opt.help "Endpoint for the Rendezvous server" <>
          Opt.value defaultEndpoint <>
          Opt.showDefault )
  where
    -- | Default URI for rendezvous server.
    --
    -- This is Brian Warner's personal server.
    defaultEndpoint = fromMaybe (panic "Invalid default URL") (parseWebSocketEndpoint "ws://relay.magic-wormhole.io:4000/v1")


data Command
  = Send
  | Receive
  | Bounce  -- Send and receive to the same server.
  deriving (Eq, Show)

commandParser :: Opt.Parser Command
commandParser = Opt.hsubparser $
  Opt.command "send" (Opt.info (pure Send) (Opt.progDesc "Send a text message, file, or directory")) <>
  Opt.command "receive" (Opt.info (pure Receive) (Opt.progDesc "Receive a text message, file, or directory")) <>
  Opt.command "bounce" (Opt.info (pure Bounce) (Opt.progDesc "Bounce a message to and from a server (i.e. send and receive)"))

makeOptions :: Text -> Opt.Parser a -> Opt.ParserInfo a
makeOptions headerText parser = Opt.info (Opt.helper <*> parser) (Opt.fullDesc <> Opt.header (toS headerText))

-- | A password used to exchange with a Magic Wormhole peer.
--
-- XXX: Just picking ByteString because that's the least amount of work. Need
-- to look up exact type of password in the magic-wormhole docs.
type Password = ByteString

-- | Send a text message to a Magic Wormhole peer.
sendText :: Rendezvous.Session -> Password -> Text -> IO ()
sendText session password message = do
  nameplate <- Rendezvous.allocate session
  mailbox <- Rendezvous.claim session nameplate
  peer <- Rendezvous.open session mailbox  -- XXX: We should run `close` in the case of exceptions?
  let (Messages.Nameplate n) = nameplate
  Peer.withEncryptedConnection peer (Spake2.makePassword (toS n <> "-" <> password))
    (\conn -> do
        let offer = FileTransfer.Message message
        Peer.sendMessage conn (ClientProtocol.PlainText (toS (Aeson.encode offer))))

-- | Receive a text message from a Magic Wormhole peer.
receiveText :: Rendezvous.Session -> IO Text
receiveText session = do
  nameplates <- Rendezvous.list session
  putStrLn $ "Nameplates: " <> Text.intercalate ", " [ n | Messages.Nameplate n <- nameplates ]
  putText "Choose nameplate: "
  nameplate <- getLine
  mailbox <- Rendezvous.claim session (Messages.Nameplate nameplate)
  peer <- Rendezvous.open session mailbox
  putText "Password: "
  password <- getLine
  let fullPassword = toS nameplate <> "-" <> toS password
  Peer.withEncryptedConnection peer (Spake2.makePassword fullPassword)
    (\conn -> do
        ClientProtocol.PlainText received <- atomically $ Peer.receiveMessage conn
        case Aeson.eitherDecode (toS received) of
          Left err -> panic $ "Could not decode message: " <> show err
          Right (FileTransfer.Message message) -> pure message)

-- | Bounce a trivial message to and from a Rendezvous server.
bounce :: WebSocketEndpoint -> Messages.AppID -> IO ()
bounce endpoint appID = do
  side1 <- Messages.generateSide
  side2 <- Messages.generateSide
  Rendezvous.runClient endpoint appID side1 $ \session1 -> do
    nameplate <- Rendezvous.allocate session1
    mailbox1 <- Rendezvous.claim session1 nameplate
    peer1 <- Rendezvous.open session1 mailbox1
    Rendezvous.runClient endpoint appID side2 $ \session2 -> do
      mailbox2 <- Rendezvous.claim session2 nameplate
      peer2 <- Rendezvous.open session2 mailbox2
      let message = "aoeu"
      (_, output) <- concurrently (send peer1 message) (receive peer2)
      unless (output == message) $ panic $ "Mismatched messages: " <> show message <> " != " <> show output
  where
    send peer message = Peer.withEncryptedConnection peer password $ \conn -> do
      let offer = FileTransfer.Message message
      Peer.sendMessage conn (ClientProtocol.PlainText (toS (Aeson.encode offer)))

    receive peer = Peer.withEncryptedConnection peer password $ \conn -> do
      ClientProtocol.PlainText received <- atomically $ Peer.receiveMessage conn
      case Aeson.eitherDecode (toS received) of
        Left err -> panic $ "Could not decode message: " <> show err
        Right (FileTransfer.Message message) -> pure message

    password = Spake2.makePassword "potato"


main :: IO ()
main = do
  options <- Opt.execParser (makeOptions "hocus-pocus - summon and traverse magic wormholes" optionsParser)
  side <- Messages.generateSide
  let endpoint = rendezvousEndpoint options
  case cmd options of
    Send -> Rendezvous.runClient endpoint appID side $ \session ->
      sendText session "potato" "Brave new world that has such offers in it"
    Receive -> Rendezvous.runClient endpoint appID side $ \session -> do
      message <- receiveText session
      putStr message
    Bounce -> bounce endpoint appID
  where
    appID = Messages.AppID "jml.io/hocus-pocus"
