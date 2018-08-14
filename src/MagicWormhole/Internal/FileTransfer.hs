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
import System.Posix.Types (FileOffset)

-- | An offer made by a sender as part of the Magic Wormhole file transfer protocol.
--
-- Currently only supports sending simple text messages. A full version would
-- also support sending files and directories.
data Offer
  -- | A simple text message.
  = Message Text
  -- | Offer a File with filename and size.
  | File FilePath FileOffset
  -- | Offer a Directory
  | Directory
    Text
    -- ^ Mode. Currently always "zipfile/deflated".
    Text
    -- ^ Directory Name.
    Integer
    -- ^ size of the transmitted compressed data in bytes
    Integer
    -- ^ estimated total size of the uncompressed directory
    Integer
    -- ^ number of files and directories being sent
  deriving (Eq, Show)

instance ToJSON Offer where
  toJSON (Message text) = object [ "offer" .= object [ "message" .= text ] ]
  toJSON (File name size) = object [ "offer" .= object [ "file" .= object [ "filename" .= name, "filesize" .= fromEnum size ] ] ]
  toJSON (Directory mode dirname zipsize numbytes numfiles) = object [ "offer" .= object [ "directory" .= object [ "mode" .= mode, "dirname" .= dirname, "zipsize" .= zipsize, "numbytes" .= numbytes, "numfiles" .= numfiles ] ] ]

instance FromJSON Offer where
  parseJSON = withObject "Offer" $ \obj -> do
    offer <- obj .: "offer"
    asum [ Message <$> offer .: "message"
         , File
           <$> ((offer .: "file") >>= (.: "filename"))
           <*> (toEnum <$> ((offer .: "file") >>= (.: "filesize")))
         , Directory
           <$> ((offer .: "directory") >>= (.: "mode"))
           <*> ((offer .: "directory") >>= (.: "dirname"))
           <*> (toEnum <$> ((offer .: "directory") >>= (.: "zipsize")))
           <*> (toEnum <$> ((offer .: "directory") >>= (.: "numbytes")))
           <*> (toEnum <$> ((offer .: "directory") >>= (.: "numfiles")))
         ]

