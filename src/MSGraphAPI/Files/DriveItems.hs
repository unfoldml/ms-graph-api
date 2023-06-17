module MSGraphAPI.Files.DriveItems where

import GHC.Generics (Generic(..))

-- aeson
import qualified Data.Aeson as A (ToJSON(..), FromJSON(..), genericParseJSON)
-- bytestring
import qualified Data.ByteString.Lazy as LBS (ByteString)
-- hoauth
import Network.OAuth.OAuth2.Internal (AccessToken(..))
-- req
import Network.HTTP.Req (Req)
-- text
import Data.Text (Text, pack, unpack)

import qualified MSGraphAPI.Internal.Common as MSG (get, getLbs, post, Collection, aesonOptions)

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
