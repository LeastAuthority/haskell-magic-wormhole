-- | Domain-specific Hedgehog generators.
--
-- These are used in multiple test modules, so broken out here for clearer re-use.
module Generator
  ( clientMessages
  , serverMessages
  , appIDs
  , phases
  , sides
  , offerMessages
  ) where

import Protolude

import Hedgehog (MonadGen(..))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import MagicWormhole.Internal.Messages
  ( ClientMessage(..)
  , ServerMessage(..)
  , MailboxMessage(..)
  , WelcomeMessage(..)
  , AppID(..)
  , Side(..)
  , MessageID(..)
  , Phase(..)
  , Body(..)
  , Nameplate(..)
  , Mailbox(..)
  , Mood(..)
  )
import qualified MagicWormhole.Internal.FileTransfer as F
  ( Offer(..)
  )

clientMessages :: MonadGen m => m ClientMessage
clientMessages = Gen.choice
  [ Bind <$> appIDs <*> sides
  , pure List
  , pure Allocate
  , Claim <$> genNameplates
  , Release <$> Gen.maybe genNameplates
  , Open <$> mailboxes
  , Add <$> phases <*> bodies
  , Close <$> Gen.maybe mailboxes <*> Gen.maybe moods
  , Ping <$> Gen.int Range.linearBounded
  ]

messageIDs :: MonadGen m => m MessageID
messageIDs = MessageID <$> Gen.int16 (Range.linear 0 maxBound)

appIDs :: MonadGen m => m AppID
appIDs = AppID <$> Gen.choice [ Gen.text (Range.linear 0 100) Gen.unicode
                              , Gen.element
                                [ "lothar.com/wormhole/text-or-file-xfer"
                                , "tahoe-lafs.org/tahoe-lafs/v1"
                                ]
                              ]

sides :: MonadGen m => m Side
sides = Side <$> Gen.text (Range.linear 0 10) Gen.hexit

phases :: MonadGen m => m Phase
phases = Gen.choice
  [ pure PakePhase
  , pure VersionPhase
  , ApplicationPhase <$> Gen.int (Range.linear 0 maxBound)
  ]

bodies :: MonadGen m => m Body
bodies = Body <$> Gen.bytes (Range.linear 0 1024)

genNameplates :: MonadGen m => m Nameplate
genNameplates = Nameplate <$> Gen.text (Range.linear 0 10) Gen.unicode

mailboxes :: MonadGen m => m Mailbox
mailboxes = Mailbox <$> Gen.text (Range.singleton 13) alphaNum
  where
    alphaNum = Gen.element "abcdefghijklmnopqrstuvwxyz09123456789"

moods :: MonadGen m => m Mood
moods = Gen.element [ Happy, Lonely, Scary, Errory ]

serverMessages :: MonadGen m => m ServerMessage
serverMessages = Gen.choice
  [ Welcome <$> welcomeMessages
  , Nameplates <$> Gen.list (Range.linear 0 10) genNameplates
  , Allocated <$> genNameplates
  , Claimed <$> mailboxes
  , pure Released
  , Message <$> mailboxMessages
  , pure Closed
  , pure Ack
  , Pong <$> Gen.int Range.linearBounded
  , Error <$> Gen.text (Range.linear 0 100) Gen.unicode <*> clientMessages
  ]

mailboxMessages :: MonadGen m => m MailboxMessage
mailboxMessages = MailboxMessage <$> sides <*> phases <*> Gen.maybe messageIDs <*> bodies

welcomeMessages :: MonadGen m => m WelcomeMessage
welcomeMessages = WelcomeMessage <$> Gen.maybe (Gen.text (Range.linear 0 1024) Gen.unicode) <*> Gen.maybe (Gen.text (Range.linear 0 1024) Gen.unicode)

offerMessages :: MonadGen m => m F.Offer
offerMessages = Gen.choice
  [ F.Message <$> (Gen.text (Range.linear 0 1024) Gen.unicode)
  , F.File <$> toS <$> (Gen.text (Range.linear 0 100) Gen.unicode)  <*> (fromIntegral <$> (Gen.int (Range.linear 0 maxBound)))
  , F.Directory
    <$> (toS <$> (Gen.text (Range.linear 0 100) Gen.unicode))
    <*> (toS <$> (Gen.text (Range.linear 0 100) Gen.unicode))
    <*> (fromIntegral <$> (Gen.int (Range.linear 0 maxBound)))
    <*> (fromIntegral <$> (Gen.int (Range.linear 0 maxBound)))
    <*> (fromIntegral <$> (Gen.int (Range.linear 0 maxBound)))
  ]
