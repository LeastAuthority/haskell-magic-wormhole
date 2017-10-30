module Rendezvous (tests) where

import Protolude

import Data.Aeson (encode, eitherDecode)
import Hedgehog (MonadGen(..), forAll, property, tripping)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

import MagicWormhole.Internal.Rendezvous
  ( ClientMessage(..)
  , ServerMessage(..)
  , AppID(..)
  , Side(..)
  , MessageID(..)
  , Phase(..)
  , Body(..)
  , Nameplate(..)
  , Mailbox(..)
  , Mood(..)
  )

tests :: IO TestTree
tests = pure $ testGroup "Rendezvous"
  [ testProperty "client messages roundtrip" $ property $ do
      x <- forAll clientMessages
      tripping x encode eitherDecode
  , testProperty "server messages roundtrip" $ property $ do
      x <- forAll serverMessages
      tripping x encode eitherDecode
  ]

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
appIDs = AppID <$> Gen.text (Range.linear 0 100) Gen.unicode

sides :: MonadGen m => m Side
sides = Side <$> Gen.text (Range.linear 0 10) Gen.hexit

phases :: MonadGen m => m Phase
phases = Phase <$> Gen.text (Range.linear 0 20) Gen.unicode

bodies :: MonadGen m => m Body
bodies = Body <$> Gen.bytes (Range.linear 0 1024)

genNameplates :: MonadGen m => m Nameplate
genNameplates = Nameplate <$> Gen.text (Range.linear 0 10) Gen.unicode

mailboxes :: MonadGen m => m Mailbox
mailboxes = Mailbox <$> Gen.text (Range.linear 0 20) Gen.unicode  -- XXX: Probably wrong.

moods :: MonadGen m => m Mood
moods = Gen.element [ Happy, Lonely, Scary, Errory ]

serverMessages :: MonadGen m => m ServerMessage
serverMessages = Gen.choice
  [ Welcome <$> Gen.maybe (Gen.text (Range.linear 0 1024) Gen.unicode) <*> Gen.maybe (Gen.text (Range.linear 0 1024) Gen.unicode)
  -- TODO: Generate rest of possible server messages.
  , Nameplates <$> Gen.list (Range.linear 0 10) genNameplates
  , Allocated <$> genNameplates
  , Claimed <$> mailboxes
  , pure Released
  , Message <$> sides <*> phases <*> messageIDs <*> bodies
  , pure Closed
  , pure Ack
  , Pong <$> Gen.int Range.linearBounded
  , Error <$> Gen.text (Range.linear 0 100) Gen.unicode <*> clientMessages
  ]
