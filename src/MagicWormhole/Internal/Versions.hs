module MagicWormhole.Internal.Versions
    ( versionExchange
    , Versions(..)
    , Error(..)
    ) where

import Protolude hiding (phase)

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
-- Obtain the 'SessionKey' from 'pakeExchange'.
versionExchange :: ClientProtocol.Connection -> ClientProtocol.SessionKey -> IO (Either Error Versions)
versionExchange conn key = do
  (_, theirVersions) <- concurrently sendVersion (atomically receiveVersion)
  pure $ case theirVersions of
    Left err -> Left err
    Right theirs
      | theirs /= Versions -> Left VersionMismatch
      | otherwise -> Right Versions
  where
    sendVersion = ClientProtocol.sendEncrypted conn key Messages.VersionPhase (toS (Aeson.encode Versions))
    receiveVersion = runExceptT $ do
      (phase, plaintext) <- ExceptT $ first CryptoError <$> ClientProtocol.receiveEncrypted conn key
      lift $ unless (phase == Messages.VersionPhase) retry
      ExceptT $ pure $ first ParseError (Aeson.eitherDecode (toS plaintext))

data Versions = Versions deriving (Eq, Show)

instance ToJSON Versions where
  toJSON _ = object ["app_versions" .= object []]

instance FromJSON Versions where
  parseJSON (Object v) = do
    -- Make sure there's an object in the "app_versions" key and abort if not.
    (Object _versions) <- v .: "app_versions"
    pure Versions
  parseJSON unknown = typeMismatch "Versions" unknown


data Error
  = ParseError String
  | VersionMismatch
  | CryptoError ClientProtocol.Error
  deriving (Eq, Show)