{-# OPTIONS_HADDOCK not-home #-}
-- |
-- Description : File transfer and simple text message protocol
--
-- Partial implementation of the [Magic Wormhole file transfer protocol](https://github.com/warner/magic-wormhole/blob/master/docs/file-transfer-protocol.md).
--
-- Once a connection has been made between peers (see 'MagicWormhole.withEncryptedConnection'),
-- you can send an 'Offer' to share a simple text message.
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
newtype Offer
  -- | A simple text message.
  = Message Text deriving (Eq, Show)

instance ToJSON Offer where
  toJSON (Message text) = object [ "offer" .= object [ "message" .= text ] ]

instance FromJSON Offer where
  parseJSON = withObject "Offer" $ \obj -> do
    offer <- obj .: "offer"
    Message <$> offer .: "message"
