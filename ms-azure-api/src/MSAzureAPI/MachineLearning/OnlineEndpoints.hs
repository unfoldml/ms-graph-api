-- |
--
-- auth: needs @user_impersonation@ scope
module MSAzureAPI.MachineLearning.OnlineEndpoints where

import Control.Applicative (Alternative(..))
import Control.Monad.IO.Class (MonadIO(..))
import Data.Foldable (asum)
import Data.Functor (void)
-- import Data.Maybe (listToMaybe)
import GHC.Generics (Generic(..))

-- aeson
import qualified Data.Aeson as A (ToJSON(..), genericToEncoding, FromJSON(..), genericParseJSON, defaultOptions, Options(..), withObject, withText, (.:), (.:?), object, (.=), Key, Value, camelTo2)
-- bytestring
import qualified Data.ByteString as BS (ByteString)
import qualified Data.ByteString.Char8 as BS8 (pack, unpack)
import qualified Data.ByteString.Lazy as LBS (ByteString)
-- ms-auth
import MSAuth (AccessToken(..))
-- req
import Network.HTTP.Req (Req, Url, Option, Scheme(..))
-- text
import Data.Text (Text, pack, unpack)
import qualified Data.Text.Lazy as TL (Text, pack, unpack, toStrict)
-- time
import Data.Time (UTCTime, getCurrentTime)
import Data.Time.Format (FormatTime, formatTime, defaultTimeLocale)
import Data.Time.LocalTime (ZonedTime, getZonedTime)

import qualified MSAzureAPI.Internal.Common as MSA (Collection, APIPlane(..), (==:), put, get, getBs, post, getLbs, aesonOptions)

-- | list online endpoints
--
-- docs : https://learn.microsoft.com/en-us/rest/api/azureml/2023-10-01/online-endpoints/list?tabs=HTTP
--
-- @GET https:\/\/management.azure.com\/subscriptions\/{subscriptionId}\/resourceGroups\/{resourceGroupName}\/providers\/Microsoft.MachineLearningServices\/workspaces\/{workspaceName}\/onlineEndpoints?api-version=2023-10-01@
listOnlineEndpoints :: Text -- ^ subscription id
             -> Text -- ^ res group id
             -> Text -- ^ ML workspace id
             -> AccessToken -> Req (MSA.Collection OnlineEndpoint)
listOnlineEndpoints sid rgid wsid = MSA.get MSA.APManagement [
  "subscriptions", sid,
    "resourceGroups", rgid,
    "providers", "Microsoft.MachineLearningServices",
    "workspaces", wsid,
    "onlineEndpoints"
  ] ("api-version" MSA.==: "2023-10-01")

data OnlineEndpoint = OnlineEndpoint {
  oeId :: Text
  , oeType :: Text
  , oeName :: Text
  , oeLocation :: Text
  , oeProperties :: OnlineEndpointProperties
                       } deriving (Show, Generic)
instance A.FromJSON OnlineEndpoint where
  parseJSON = A.genericParseJSON (MSA.aesonOptions "oe")
instance A.ToJSON OnlineEndpoint where
  toEncoding = A.genericToEncoding (MSA.aesonOptions "oe")


data OnlineEndpointProperties = OnlineEndpointProperties {
  oepProperties :: A.Value
  , oepScoringUri :: Text
                       } deriving (Show, Generic)
instance A.FromJSON OnlineEndpointProperties where
  parseJSON = A.genericParseJSON (MSA.aesonOptions "oep")
instance A.ToJSON OnlineEndpointProperties where
  toEncoding = A.genericToEncoding (MSA.aesonOptions "oep")
