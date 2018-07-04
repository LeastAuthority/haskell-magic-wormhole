-- |
-- Description : A Magic Wormhole client.
--
-- Magic Wormhole is a technology for getting things from one computer to another, safely.
--
-- To use it, you must
--
--   1. Start a 'Rendezvous.Session' with the Rendezvous server, to allow peers to find each other ('Rendezvous.runClient')
--   2. Negotiate a shared 'Messages.Nameplate' so peers can find each other on the server ('Rendezvous.allocate', 'Rendezvous.list')
--   3. Use the shared 'Messages.Nameplate' to 'Rendezvous.open' a shared 'Messages.Mailbox'
--   4. Use a secret password shared between peers to establish an encrypted connection ('Peer.withEncryptedConnection')
--
-- Once you've done this, you can communicate with your peer via 'Peer.sendMessage' and 'Peer.receiveMessage'.
--
-- The password is never sent over the wire.
-- Rather, it is used to negotiate a session key using SPAKE2,
-- and that key itself is used to derive many per-message keys,
-- so that each message is encrypted using NaCl SecretBox.
--
-- This library is a client library for the Rendezvous server /and/ a library
-- for communicating with Magic Wormhole peers.
module MagicWormhole
  ( -- * Client/server
    --
    -- | Before you can communicate with a Magic Wormhole peer, you must first find them.
    -- The way to do this is to establish a 'Rendezvous.Session' with a Magic Wormhole Rendezvous server.

    -- ** Establishing a session
    Rendezvous.Session
  , Rendezvous.runClient
  , Messages.AppID(..)
  , Messages.Side(..)
  , Messages.generateSide
    -- ** Locating the server
    --
    -- | Rendezvous servers are implemented as web sockets.
  , WebSockets.WebSocketEndpoint(..)
  , WebSockets.parseWebSocketEndpoint
    -- ** Operations on the server
  , Rendezvous.allocate
  , Messages.Nameplate(..)
  , Rendezvous.list
  , Rendezvous.claim
  , Messages.Mailbox(..)
  , Rendezvous.open
  , Rendezvous.close
    -- ** Errors
  , Rendezvous.ServerError(..)
  , Rendezvous.ClientError(..)
    -- * Peer-to-peer
    --
    -- | Opening a 'Messgaes.Mailbox' shared with a peer gets you a 'ClientProtocol.Connection',
    -- but this is not enough to securely communicate with a peer.
    -- The next step is to establish an 'Peer.EncryptedConnection'
    -- (via -- 'Peer.withEncryptedConnection'),
    -- and then communicate with 'Peer.sendMessage' and 'Peer.receiveMessage'.

    -- ** Establishing a secure connection
  , Peer.withEncryptedConnection
  , ClientProtocol.Connection
  , Peer.EncryptedConnection
  , Peer.deriveKey
    -- *** Errors
  , ClientProtocol.PeerError
  , Versions.VersionsError
  , Pake.PakeError
    -- ** Communicating with a peer
  , Peer.sendMessage
  , Peer.receiveMessage
  , ClientProtocol.PlainText(..)
    -- * Magic Wormhole applications
    --
    -- | Once you've established an 'Peer.EncryptedConnection' to your peer, the world is your oyster.
    -- You can send whatever data you'd like.
    --
    -- However, Magic Wormhole comes with at least one built-in "application": message and file transfer.
    -- This Haskell implementation only supports sending and receiving a simple message.
  , FileTransfer.Offer(..)
  ) where

import qualified MagicWormhole.Internal.ClientProtocol as ClientProtocol
import qualified MagicWormhole.Internal.FileTransfer as FileTransfer
import qualified MagicWormhole.Internal.Messages as Messages
import qualified MagicWormhole.Internal.Pake as Pake
import qualified MagicWormhole.Internal.Peer as Peer
import qualified MagicWormhole.Internal.Rendezvous as Rendezvous
import qualified MagicWormhole.Internal.Versions as Versions
import qualified MagicWormhole.Internal.WebSockets as WebSockets
