module MagicWormhole.Internal.FileTransfer
  ( Offer(..)
  ) where

import Protolude

import Data.Aeson
  ( FromJSON(..)
  , ToJSON(..)
  , (.:)
  , (.=)
  , object
  , withObject
  )

-- | An offer made by a sender as part of the Magic Wormhole file transfer protocol.
--
-- Currently only supports sending simple text messages. A full version would
-- also support sending files and directories.
newtype Offer = Message Text deriving (Eq, Show)

instance ToJSON Offer where
  toJSON (Message text) = object [ "offer" .= object [ "message" .= text ] ]

instance FromJSON Offer where
  parseJSON = withObject "Offer" $ \obj -> do
    offer <- obj .: "offer"
    Message <$> offer .: "message"
