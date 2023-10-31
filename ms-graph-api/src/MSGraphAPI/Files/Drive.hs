module MSGraphAPI.Files.Drive (
  listDrivesMe
  , listDrivesGroup
  -- * types
  , Drive(..)
                              ) where

-- import Control.Applicative (Alternative(..))
-- import Data.Int (Int32)
import GHC.Generics (Generic(..))

-- aeson
import qualified Data.Aeson as A (ToJSON(..), FromJSON(..), genericParseJSON)
-- import qualified Data.Aeson.Types as A (Parser)
-- bytestring
-- import qualified Data.ByteString.Lazy as LBS (ByteString)
-- ms-auth
import MSAuth (AccessToken(..))
-- req
import Network.HTTP.Req (Req)
-- text
import Data.Text (Text)
-- time
import Data.Time (ZonedTime)

import qualified MSGraphAPI.Internal.Common as MSG (get, Collection, aesonOptions)

-- | The top-level object that represents a user's OneDrive or a document library in SharePoint.
--
-- OneDrive users will always have at least one drive available, their default drive. Users without a OneDrive license may not have a default drive available.
--
-- https://learn.microsoft.com/en-us/graph/api/resources/drive?view=graph-rest-1.0
data Drive = Drive {
  dId :: Text
  , dName :: Text
  , dDescription :: Text
  , dLastModifiedDateTime :: ZonedTime -- 2022-11-28T09:18:45Z
  , dDriveType :: Text
                           } deriving (Show, Generic)
instance A.FromJSON Drive where
  parseJSON = A.genericParseJSON (MSG.aesonOptions "d")
instance A.ToJSON Drive

-- | List the current user's drives
--
-- @GET \/me\/drives@
listDrivesMe :: AccessToken -> Req (MSG.Collection Drive)
listDrivesMe = MSG.get ["me", "drives"] mempty


-- | To list the document libraries for a group, your app requests the drives relationship on the Group.
--
-- @GET \/groups\/{groupId}\/drives@
listDrivesGroup :: Text -- ^ group ID
                -> AccessToken -> Req (MSG.Collection Drive)
listDrivesGroup gid = MSG.get ["groups", gid, "drives"] mempty
