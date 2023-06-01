
import qualified MagicWormhole.Internal.Versions as Versions
import Data.Aeson.Types
import Data.String (String)

data InvitesVersion0 = InvitesVersion0 deriving (Eq, Show)

instance ToJSON InvitesVersion0 where
  toJSON InvitesVersion0 =
    object ["magic-folder" .= object ["supported-messages" .= ["invite-v1" :: String]]]


{-
app_versions={
     "magic-folder": {
            "supported-messages": [
                "invite-v1",
            ],
        },
}
-}


main = do
  let a = InvitesVersion0
  let v = (Versions.Versions InvitesVersion0)
  print $ toJSON a
  print v
