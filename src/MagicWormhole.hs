-- |
-- Description : A Magic Wormhole client.
module MagicWormhole
  ( Messages.Nameplate(..)
  , Messages.AppID(..)
  , Messages.Side(..)
  , Messages.generateSide
  , ClientProtocol.PlainText(..)
  , ClientProtocol.CipherText(..)
  , Rendezvous.Session
  , Rendezvous.runClient
  , Rendezvous.allocate
  , Rendezvous.list
  , Rendezvous.claim
  , Rendezvous.open
  , Rendezvous.close
  , Peer.withEncryptedConnection
  , Peer.sendMessage
  , Peer.receiveMessage
  , FileTransfer.Offer(..)
  , WebSockets.WebSocketEndpoint(..)
  , WebSockets.parseWebSocketEndpoint
  ) where

import qualified MagicWormhole.Internal.ClientProtocol as ClientProtocol
import qualified MagicWormhole.Internal.FileTransfer as FileTransfer
import qualified MagicWormhole.Internal.Messages as Messages
import qualified MagicWormhole.Internal.Rendezvous as Rendezvous
import qualified MagicWormhole.Internal.Peer as Peer
import qualified MagicWormhole.Internal.WebSockets as WebSockets
