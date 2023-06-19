-- | User
module MSGraphAPI.Users.Group where

import GHC.Generics (Generic(..))

-- aeson
import qualified Data.Aeson as A (ToJSON(..), FromJSON(..), eitherDecode, genericParseJSON, defaultOptions, Options(..), withObject, withText, (.:), (.:?), object, (.=))
-- hoauth
import Network.OAuth.OAuth2.Internal (AccessToken(..))
-- req
import Network.HTTP.Req (Req)
-- text
import Data.Text (Text, pack, unpack)

import qualified MSGraphAPI.Internal.Common as MSG (Collection(..), get, post, aesonOptions)
import MSGraphAPI.Files.DriveItem (DriveItem)

-- | Groups are collections of principals with shared access to resources in Microsoft services or in your app. Different principals such as users, other groups, devices, and applications can be part of groups. 
--
-- https://learn.microsoft.com/en-us/graph/api/resources/groups-overview?view=graph-rest-1.0&tabs=http
data Group = Group {
  gId :: Text
  , gDisplayName :: Text
  , gDescription :: Text
                   } deriving (Eq, Ord, Show, Generic)
instance A.FromJSON Group where
  parseJSON = A.genericParseJSON (MSG.aesonOptions "g")

-- | Get the teams in Microsoft Teams that the user is a direct member of.
--
-- @GET \/users\/{id | user-principal-name}\/joinedTeams@
--
-- https://learn.microsoft.com/en-us/graph/api/user-list-joinedteams?view=graph-rest-1.0&tabs=http
getUserJoinedTeams :: Text -- ^ User ID
                   -> AccessToken -> Req (MSG.Collection Group)
getUserJoinedTeams uid = MSG.get ["users", uid, "joinedTeams"] mempty

-- | Get the 'DriveItem's in the 'Group' storage, starting from the root item
--
-- @GET \/groups\/{group-id}\/drive\/root\/children@
--
-- https://learn.microsoft.com/en-us/graph/api/driveitem-list-children?view=graph-rest-1.0&tabs=http
getGroupsDriveItems :: Text -- ^ Group ID
                    -> AccessToken -> Req (MSG.Collection DriveItem)
getGroupsDriveItems gid = MSG.get ["groups", gid, "drive", "root", "children"] mempty


-- data X = X { xName :: Text } deriving (Eq, Ord, Show, Generic)
-- instance A.FromJSON X where
--   parseJSON = A.genericParseJSON (MSG.aesonOptions "x")

-- pt0 :: Either String (MSG.Collection DriveItem)
-- pt0 = A.eitherDecode t0
--   where
--     t0 = "{\r\n  \"value\": [\r\n    {\"name\": \"myfile.jpg\"  },\r\n    {\"name\": \"Documents\" },\r\n    {\"name\": \"Photos\" },\r\n    {\"name\": \"my sheet(1).xlsx\"}\r\n  ]\r\n}"
