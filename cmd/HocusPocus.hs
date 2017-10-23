-- | Command-line tool for opening and communicating through magic wormholes.
--
-- Intended to inter-operate with the `wormhole` command-line tool from
-- [magic-wormhole](https://github.com/warner/magic-wormhole).
module Main (main) where

import Protolude

import Data.String (String)
import Network.Socket (withSocketsDo)
import Network.URI (URI(..), URIAuth(..), parseURI)
import qualified Network.WebSockets as WS
import qualified Options.Applicative as Opt


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
        (Opt.maybeReader (toWebSocketEndpoint <=< parseURI))
        ( Opt.long "rendezvous-url" <>
          Opt.help "Endpoint for the Rendezvous server" <>
          Opt.value defaultEndpoint <>
          Opt.showDefault )
  where
    -- | Default URI for rendezvous server.
    --
    -- This is Brian Warner's personal server.
    defaultEndpoint = fromMaybe (panic "Invalid default URL") (toWebSocketEndpoint <=< parseURI $ "ws://relay.magic-wormhole.io:4000/v1")


-- | Endpoint for a websocket connection.
data WebSocketEndpoint = WebSocketEndpoint Hostname Port Path deriving (Eq, Show)

type Hostname = String
type Port = Int
type Path = String

-- | Turn a 'URI' into a 'WebSocketEndpoint', if we can.
--
-- Requires that the URI has an authority (i.e. host & port).
-- Discards information from scheme, query, and fragment.
toWebSocketEndpoint :: URI -> Maybe WebSocketEndpoint
toWebSocketEndpoint uri = do
  authority <- uriAuthority uri
  port <- case uriPort authority of
            "" -> empty
            _:rest -> readMaybe rest
  pure $ WebSocketEndpoint (uriRegName authority) port (uriPath uri)

data Command
  = Send
  | Receive
  deriving (Eq, Show)

commandParser :: Opt.Parser Command
commandParser = Opt.hsubparser $
  Opt.command "send" (Opt.info (pure Send) (Opt.progDesc "Send a text message, file, or directory")) <>
  Opt.command "receive" (Opt.info (pure Receive) (Opt.progDesc "Receive a text message, file, or directory"))

makeOptions :: Text -> Opt.Parser a -> Opt.ParserInfo a
makeOptions headerText parser = Opt.info (Opt.helper <*> parser) (Opt.fullDesc <> Opt.header (toS headerText))

-- | Execute 'Command' against a Wormhole Rendezvous server.
app :: Command -> WS.ClientApp ()
app command conn = do
  print command
  -- XXX: Just block waiting for the server to tell us stuff. To be a proper
  -- client, we want to get stuff from the server and send stuff more or less
  -- simultaneously.
  void $ forever $ do
    message <- WS.receiveData conn
    liftIO $ putStrLn @Text message

main :: IO ()
main = do
  options <- Opt.execParser (makeOptions "hocus-pocus - summon and traverse magic wormholes" optionsParser)
  let WebSocketEndpoint host port path = rendezvousEndpoint options
  withSocketsDo $ WS.runClient host port path (app (cmd options))
