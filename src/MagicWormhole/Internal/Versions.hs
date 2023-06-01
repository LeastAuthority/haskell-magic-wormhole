{-# OPTIONS_HADDOCK not-home #-}
-- |
-- Description : Peer version exchange
--
-- Once a shared 'ClientProtocol.SessionKey' has been negotiated, the peers
-- need to confirm that they have the same key. They do this with
-- 'versionExchange'.
module MagicWormhole.Internal.Versions
    ( versionExchange
    , Versions(..)
    , VersionsError(..)
    ) where

import Protolude hiding (phase, toS)
import Protolude.Conv (toS)

import Data.Aeson (FromJSON, ToJSON, (.=), object, Value(..), (.:))
import Data.Aeson.Types (typeMismatch)
import qualified Data.Aeson as Aeson
import Data.String (String)

import qualified MagicWormhole.Internal.ClientProtocol as ClientProtocol
import qualified MagicWormhole.Internal.Messages as Messages

-- NOTE: Versions
-- ~~~~~~~~~~~~~~
--
-- Magic Wormhole Python implementation sends the following as the 'version' phase:
--     {"app_versions": {}}
--
-- The idea is that /some time in the future/ this will be used to indicate
-- capabilities of peers. At present, it is unused, save as a confirmation
-- that the SPAKE2 exchange worked.

-- | Exchange version information with a Magic Wormhole peer.
--
-- Can throw an 'Error' if something goes wrong.
versionExchange
  :: ClientProtocol.Connection -- ^ A connection to a peer
  -> ClientProtocol.SessionKey -- ^ A shared session key. Obtain this via 'MagicWormhole.Internal.Pake.pakeExchange'.
  -> appversions  -- ^ Anything aeson-able
  -> IO Versions  -- ^ Shared version information
versionExchange conn key = do
  (_, theirVersions) <- concurrently sendVersion (atomically receiveVersion)
  if theirVersions /= Versions then throwIO VersionMismatch else pure Versions
  where
   sendVersion = ClientProtocol.sendEncrypted conn key Messages.VersionPhase (ClientProtocol.PlainText (toS (Aeson.encode Versions appversions)))
    receiveVersion = do
      (phase, ClientProtocol.PlainText plaintext) <- ClientProtocol.receiveEncrypted conn key
      unless (phase == Messages.VersionPhase) retry
      either (throwSTM . ParseError) pure $ Aeson.eitherDecode (toS plaintext)

-- | Information about the versions supported by this Magic Wormhole client.
--
-- There are no extant Magic Wormhole implementations that send any meaningful
-- information in their versions message, so this is just a single-valued
-- type.
data Versions = Versions a deriving (Eq, Show)

instance ToJSON a => ToJSON Versions a where
  toJSON _ = object ["app_versions" .= (toJSON a)]

instance FromJSON Versions where
  parseJSON (Object v) = do
    -- Make sure there's an object in the "app_versions" key and abort if not.
    (Object _versions) <- v .: "app_versions"
    pure Versions
  parseJSON unknown = typeMismatch "Versions" unknown

-- | An error occurred during 'versionExchange'.
data VersionsError
  -- | We could not interpret the other side's version information
  = ParseError String
  -- | The other side sent us version information, but it does not match ours,
  -- so we cannot proceed.
  | VersionMismatch
  deriving (Eq, Show, Typeable)

instance Exception VersionsError
