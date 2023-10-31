-- | Files.DriveItem
module MSGraphAPI.Files.DriveItem (
  -- * list items
  listRootChildrenMe
  , listGroupItemChildren
  -- , listGroupRootChildren
  -- * download items
  , downloadFile
  , downloadFileMe
  -- * types
  , DriveItem(..)
  , DIItem(..)
  , File(..), Folder(..), Package(..)
                                  ) where

import Control.Applicative (Alternative(..))
import Data.Int (Int32)
import GHC.Generics (Generic(..))

-- aeson
import qualified Data.Aeson as A (ToJSON(..), FromJSON(..), genericParseJSON, (.:), Object, withObject, Key)
import qualified Data.Aeson.Types as A (Parser)
-- bytestring
import qualified Data.ByteString.Lazy as LBS (ByteString)
-- ms-auth
import MSAuth (AccessToken(..))
-- req
import Network.HTTP.Req (Req)
-- text
import Data.Text (Text)
-- time
import Data.Time (ZonedTime)

import qualified MSGraphAPI.Internal.Common as MSG (get, getLbs, Collection, aesonOptions)

-- | The 'DriveItem' resource represents a file, folder, or other item stored in a drive.
--
-- All file system objects in OneDrive and SharePoint are returned as driveItem resources. 
--
-- https://learn.microsoft.com/en-us/graph/api/resources/driveitem?view=graph-rest-1.0
data DriveItem = DriveItem {
  diId :: Text
  , diName :: Text
  , diLastModifiedDateTime :: ZonedTime -- 2022-11-28T09:18:45Z
  , diItem :: DIItem
                           } deriving (Show, Generic)
instance A.ToJSON DriveItem

instance A.FromJSON DriveItem where
  parseJSON = A.withObject "DriveItem" $ \o -> DriveItem <$>
    o A..: "id" <*>
    o A..: "name" <*>
    o A..: "lastModifiedDateTime" <*>
    diItemP o

diItemP :: A.Object -> A.Parser DIItem
diItemP o =
  (DIIFile <$> o A..: "file") <|>
  (DIIFolder <$> o A..: "folder") <|>
  (DIIRemoteItem <$ o .: "remoteItem") <|>
  (DIIPhoto <$ o .: "photo") <|>
  (DIIVideo <$ o .: "video") <|>
  (DIIBundle <$ o .: "bundle") <|>
  (DIIPackage <$> o A..: "package")


(.:) :: A.Object -> A.Key -> A.Parser ()
(.:) = (A..:)

-- | A sum type for the various drive item types
--
-- This is a departure from the original API but makes it convenient to pattern match on constructors
data DIItem = DIIFile File
            | DIIFolder Folder
            | DIIRemoteItem
            | DIIPhoto
            | DIIVideo
            | DIIBundle
            | DIIPackage Package
            deriving (Eq, Ord, Show, Generic)
instance A.ToJSON DIItem where
  toJSON = \case
    DIIFile f -> A.toJSON f
    DIIFolder f -> A.toJSON f
    DIIPackage f -> A.toJSON f
    e -> A.toJSON $ drop 3 (show e) -- FIXME hack

-- | The Folder resource groups folder-related data on an item into a single structure. DriveItems with a non-null folder facet are containers for other DriveItems.
--
-- https://learn.microsoft.com/en-us/graph/api/resources/folder?view=graph-rest-1.0
data Folder = Folder {
  difoChildCount :: Int32
                     } deriving (Eq, Ord, Show, Generic)
instance A.FromJSON Folder where
  parseJSON = A.genericParseJSON (MSG.aesonOptions "difo")
instance A.ToJSON Folder

-- | The File resource groups file-related data items into a single structure.
--
-- https://learn.microsoft.com/en-us/graph/api/resources/file?view=graph-rest-1.0
data File = File {
  difiMimeType :: Text
                     } deriving (Eq, Ord, Show, Generic)
instance A.FromJSON File where
  parseJSON = A.genericParseJSON (MSG.aesonOptions "difi")
instance A.ToJSON File

data Package = Package {
  dipType :: Text
                       } deriving (Eq, Ord, Show, Generic)
instance A.FromJSON Package where
  parseJSON = A.genericParseJSON (MSG.aesonOptions "dip")
instance A.ToJSON Package

-- | download a complete file from user's directory
--
-- @GET \/me\/drive\/items\/{item-id}\/content@
--
-- https://learn.microsoft.com/en-us/graph/api/driveitem-get-content?view=graph-rest-1.0&tabs=http#request
downloadFileMe :: Text -- ^ item ID
               -> AccessToken -> Req LBS.ByteString
downloadFileMe itemId = MSG.getLbs ["me", "drive", "items", itemId, "content"] mempty

-- | download a file from a drive
--
-- @GET \/drives\/{drive-id}\/items\/{item-id}\/content@
--
-- https://learn.microsoft.com/en-us/graph/api/driveitem-get-content?view=graph-rest-1.0&tabs=http#request
downloadFile :: Text -- ^ drive ID
             -> Text -- ^ file ID
             -> AccessToken -> Req LBS.ByteString
downloadFile did itemId = MSG.getLbs ["drives", did, "items", itemId, "content"] mempty

-- | List children in the root of the current user's drive
--
-- @GET \/me\/drive\/root\/children@
--
-- https://learn.microsoft.com/en-us/graph/api/driveitem-list-children?view=graph-rest-1.0&tabs=http#list-children-in-the-root-of-the-current-users-drive
listRootChildrenMe :: AccessToken -> Req (MSG.Collection DriveItem)
listRootChildrenMe = MSG.get ["me", "drive", "root", "children"] mempty


-- | List children of an item of a group drive
--
-- @GET \/groups\/{group-id}\/drive\/items\/{item-id}\/children@
--
-- https://learn.microsoft.com/en-us/graph/api/driveitem-list-children?view=graph-rest-1.0&tabs=http
listGroupItemChildren :: Text -- ^ group ID
                      -> Text -- ^ item ID
                      -> AccessToken -> Req (MSG.Collection DriveItem)
listGroupItemChildren gid iid =
  MSG.get ["groups", gid, "drive", "items", iid, "children"] mempty

-- -- | List children of the root item of a group drive
-- --
-- -- @GET \/groups\/{group-id}\/drive\/root\/children@
-- listGroupRootChildren :: Text -- ^ group ID
--                       -> AccessToken -> Req (MSG.Collection DriveItem)
-- listGroupRootChildren gid =
--   MSG.get ["groups", gid, "drive", "root", "children"] mempty -- TODO DOUBLE CHECK PATH
