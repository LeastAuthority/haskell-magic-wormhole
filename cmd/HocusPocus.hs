-- | Command-line tool for opening and communicating through magic wormholes.
--
-- Intended to inter-operate with the `wormhole` command-line tool from
-- [magic-wormhole](https://github.com/warner/magic-wormhole).
module Main (main) where

import Protolude

import Network.URI (URI, parseURI)
import qualified Options.Applicative as Opt


data Options
  = Options
  { cmd :: Command
  , rendezvousServerURL :: URI
  } deriving (Eq, Show)

optionsParser :: Opt.Parser Options
optionsParser
  = Options
  <$> commandParser
  <*> Opt.option
        (Opt.maybeReader parseURI)
        ( Opt.long "rendezvous-url" <>
          Opt.help "Endpoint for the Rendezvous server" <>
          Opt.value defaultRendezvousURL <>
          Opt.showDefault )
  where
    -- | Default URI for rendezvous server.
    --
    -- This is Brian Warner's personal server.
    defaultRendezvousURL = mustParseURI "ws://relay.magic-wormhole.io:4000/v1"
    mustParseURI uri = fromMaybe (panic . toS $ "Invalid URL: " <> uri) (parseURI uri)

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

main :: IO ()
main = do
  options <- Opt.execParser (makeOptions "hocus-pocus - summon and traverse magic wormholes" optionsParser)
  print $ rendezvousServerURL options
  case cmd options of
    -- XXX: What's the Protolude way of saying "print this as text"?
    Send -> putStrLn @Text "send"
    Receive -> putStrLn @Text "receive"
