-- | Command-line tool for opening and communicating through magic wormholes.
--
-- Intended to inter-operate with the `wormhole` command-line tool from
-- [magic-wormhole](https://github.com/warner/magic-wormhole).
module Main (main) where

import Protolude

import Options.Applicative
  ( Parser
  , ParserInfo
  , command
  , execParser
  , header
  , helper
  , hsubparser
  , fullDesc
  , info
  , progDesc
  )


newtype Options
  = Options
  { cmd :: Command
  } deriving (Eq, Show)

optionsParser :: Parser Options
optionsParser = Options <$> commandParser

data Command
  = Send
  | Receive
  deriving (Eq, Show)

commandParser :: Parser Command
commandParser = hsubparser $
  command "send" (info (pure Send) (progDesc "Send a text message, file, or directory")) <>
  command "receive" (info (pure Receive) (progDesc "Receive a text message, file, or directory"))

makeOptions :: Text -> Parser a -> ParserInfo a
makeOptions headerText parser = info (helper <*> parser) (fullDesc <> header (toS headerText))

main :: IO ()
main = do
  options <- execParser (makeOptions "hocus-pocus - summon and traverse magic wormholes" optionsParser)
  case cmd options of
    -- XXX: What's the Protolude way of saying "print this as text"?
    Send -> putStrLn @Text "send"
    Receive -> putStrLn @Text "receive"
