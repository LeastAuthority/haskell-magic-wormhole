module MagicWormhole.Internal.Peer
  ( Connection(..)
  , appID
  , ourSide
  , send
  , receive
  ) where

import Protolude

import qualified MagicWormhole.Internal.Messages as Messages
import qualified MagicWormhole.Internal.Rendezvous as Rendezvous

-- | A connection to a peer via the Rendezvous server.
newtype Connection = Connection { unConnection :: Rendezvous.Session }

appID :: Connection -> Messages.AppID
appID = Rendezvous.sessionAppID . unConnection

ourSide :: Connection -> Messages.Side
ourSide = Rendezvous.sessionSide . unConnection

send :: Connection -> Messages.Phase -> Messages.Body -> IO ()
send = Rendezvous.add . unConnection

receive :: Connection -> STM Messages.MailboxMessage
receive = Rendezvous.readFromMailbox . unConnection
