{-# OPTIONS_HADDOCK not-home #-}
-- | Help interaction with websockets.
module MagicWormhole.Internal.WebSockets
  ( WebSocketEndpoint(..)
  , parseWebSocketEndpoint
  , uriToWebSocketEndpoint
  , Hostname
  , Port
  , Path
  ) where

import Protolude

import Data.String (String)
import Network.URI (URI(..), URIAuth(..), parseURI)

-- | Endpoint for a websocket connection.
--
-- Construct directly or with 'parseWebSocketEndpoint'.
data WebSocketEndpoint = WebSocketEndpoint Hostname Port Path deriving (Eq, Show)

-- | Host name for a websocket endpoint. e.g. @example.com@.
type Hostname = String

-- | Port number for a websocket endpoint. e.g. @80@
type Port = Int

-- | Path to a websocket endpoint. e.g. @\/v1\/foo@
type Path = String

-- | Turn a 'URI' into a 'WebSocketEndpoint', if we can.
--
-- Requires that the URI has an authority (i.e. host & port).
-- Discards information from scheme, query, and fragment.
uriToWebSocketEndpoint :: URI -> Maybe WebSocketEndpoint
uriToWebSocketEndpoint uri = do
  authority <- uriAuthority uri
  port <- case uriPort authority of
            "" -> empty
            _:rest -> readMaybe rest
  pure $ WebSocketEndpoint (uriRegName authority) port (uriPath uri)

-- | Parse a 'WebSocketEndpoint'.
parseWebSocketEndpoint :: String -> Maybe WebSocketEndpoint
parseWebSocketEndpoint = uriToWebSocketEndpoint <=< parseURI
