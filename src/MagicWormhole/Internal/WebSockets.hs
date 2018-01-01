-- | Help interaction with websockets.
module MagicWormhole.Internal.WebSockets
  ( WebSocketEndpoint(..)
  , parseWebSocketEndpoint
  , uriToWebSocketEndpoint
  , runClient
  ) where

import Protolude

import Data.String (String)
import Network.URI (URI(..), URIAuth(..), parseURI)
import qualified Network.WebSockets as WS

-- | Endpoint for a websocket connection.
--
-- Construct directly or with 'parseWebSocketEndpoint'.
data WebSocketEndpoint = WebSocketEndpoint Hostname Port Path deriving (Eq, Show)

type Hostname = String
type Port = Int
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

-- | Run a websocket client application against a particular endpoint.
--
-- This is a work-around for a bug in websockets where it raises an exception
-- after running the client action if the connection was uncleanly closed
-- *and* a bug in magic-wormhole, which does not cleanly close the websocket
-- connection.
--
-- See https://github.com/jaspervdj/websockets/issues/142
runClient :: WebSocketEndpoint -> WS.ClientApp a -> IO a
runClient (WebSocketEndpoint host port path) app = do
  output <- newEmptyMVar
  catchJust isClosed (WS.runClient host port path (action output)) (\_ -> pure ())
  readMVar output
  where
    action output ws = do
      -- If the client app throws a connection closed error, then wrap it in
      -- our custom type and re-raise. This lets us propagate those
      -- exceptions, which probably aren't the results of bugs in websockets.
      result <- catchJust isClosed (app ws) (throwIO . ConnectionClosed)
      putMVar output result

    isClosed e =
      case e of
        WS.ConnectionClosed -> Just e  -- XXX: This is "closed unexpectedly", and should arguably not be caught here
        WS.CloseRequest{} -> Just e  -- Everything closed OK.
        _ -> Nothing


newtype ConnectionClosed = ConnectionClosed WS.ConnectionException deriving (Show)
instance Exception ConnectionClosed
