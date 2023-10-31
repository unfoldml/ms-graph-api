module MSGraphAPI.Drive where

import Data.Int (Int64)
import GHC.Generics (Generic(..))

-- aeson
import qualified Data.Aeson as A (ToJSON(..), FromJSON(..), genericParseJSON)
-- ms-auth
import MSAuth (AccessToken(..))
-- req
import Network.HTTP.Req (Req)
-- text
import Data.Text (Text, pack, unpack)

import qualified MSGraphAPI.Internal.Common as MSG (get, post, Collection, aesonOptions)

-- | Get drive of current user
getDriveMe :: AccessToken -> Req Drive
getDriveMe = MSG.get ["me", "drive"] mempty

-- | List children in the root of the current user's drive
--
-- https://learn.microsoft.com/en-us/graph/api/driveitem-list-children?view=graph-rest-1.0&tabs=http#list-children-in-the-root-of-the-current-users-drive
getDriveItemsMe :: AccessToken -> Req (MSG.Collection DriveItem)
getDriveItemsMe = MSG.get ["me", "drive", "root", "children"] mempty

-- | List children in the root of the current user's drive
--
-- @GET \/drives\/{drive-id}\/items\/{item-id}\/children@
--
-- https://learn.microsoft.com/en-us/graph/api/driveitem-list-children?view=graph-rest-1.0&tabs=http#list-children-in-the-root-of-the-current-users-drive
getDriveItemChildren :: Text -- ^ drive ID
                     -> Text -- ^ item ID
                     -> AccessToken
                     -> Req (MSG.Collection DriveItem)
getDriveItemChildren did itemId = MSG.get ["drives", did, "items", itemId, "children"] mempty

data Drive = Drive {
  dId :: Text
                   } deriving (Eq, Show, Generic)

instance A.FromJSON Drive where
  parseJSON = A.genericParseJSON (MSG.aesonOptions "d")


data DriveItem = DriveItem {
  diName :: Text
  , diSize :: Maybe Int64
                           } deriving (Eq, Show, Generic)

instance A.FromJSON DriveItem where
  parseJSON = A.genericParseJSON (MSG.aesonOptions "di")
