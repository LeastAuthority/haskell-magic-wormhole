-- | Interface for talking to a magic-wormhole peer.
module MagicWormhole.Internal.Peer
  ( Connection(..)
  ) where

import Protolude

import qualified MagicWormhole.Internal.Messages as Messages

-- | A connection to a peer via the Rendezvous server.
--
-- Normally construct this with 'Rendezvous.open'.
data Connection
  = Connection
  { -- | The application ID for this connection.
    appID :: Messages.AppID
    -- | The identifier for this side of the connection.
  , ourSide :: Messages.Side
    -- | Send a message to the other side.
  , send :: Messages.Phase -> Messages.Body -> IO ()
    -- | Receive a message from the other side.
  , receive :: STM Messages.MailboxMessage
  }
