-- |
--
-- auth: needs @user_impersonation@ scope
module MSAzureAPI.MachineLearning.Compute where

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

-- | list computes in a workspace
--
-- docs : https://learn.microsoft.com/en-us/rest/api/azureml/2023-04-01/compute/list?tabs=HTTP
--
-- @GET https:\/\/management.azure.com\/subscriptions\/{subscriptionId}\/resourceGroups\/{resourceGroupName}\/providers\/Microsoft.MachineLearningServices\/workspaces\/{workspaceName}\/computes?api-version=2023-04-01@
listComputes :: Text -- ^ subscription id
             -> Text -- ^ res group id
             -> Text -- ^ ML workspace id
             -> AccessToken -> Req (MSA.Collection Compute)
listComputes sid rgid wsid = MSA.get MSA.APManagement [
  "subscriptions", sid,
    "resourceGroups", rgid,
    "providers", "Microsoft.MachineLearningServices",
    "workspaces", wsid,
    "computes"
  ] ("api-version" MSA.==: "2023-04-01")

data Compute = Compute {
  cmpId :: Text
  , cmpType :: Text
  , cmpName :: Text
  , cmpLocation :: Text
  , cmpProperties :: ComputeInstance
                       } deriving (Show, Generic)
instance A.FromJSON Compute where
  parseJSON = A.genericParseJSON (MSA.aesonOptions "cmp")
instance A.ToJSON Compute where
  toEncoding = A.genericToEncoding (MSA.aesonOptions "cmp")

-- | ComputeInstance https://learn.microsoft.com/en-us/rest/api/azureml/2023-10-01/compute/get?tabs=HTTP#computeinstance
data ComputeInstance = ComputeInstance {
  cmpiCreatedOn :: ZonedTime
  , cmpiModifiedOn :: ZonedTime
  , cmpiResourceId :: Text
  , cmpiComputeType :: ComputeType
  , cmpiProperties :: ComputeInstanceProperties
  , cmpiProvisioningState :: ProvisioningState
                                           } deriving (Show, Generic)
instance A.ToJSON ComputeInstance where
  toEncoding = A.genericToEncoding (MSA.aesonOptions "cmpi")
instance A.FromJSON ComputeInstance where
  parseJSON = A.genericParseJSON (MSA.aesonOptions "cmpi")

data ComputeInstanceProperties = ComputeInstanceProperties {
  cmpipState :: Text
                                                           } deriving (Show, Generic)
instance A.ToJSON ComputeInstanceProperties where
  toEncoding = A.genericToEncoding (MSA.aesonOptions "cmpip")
instance A.FromJSON ComputeInstanceProperties where
  parseJSON = A.genericParseJSON (MSA.aesonOptions "cmpip")

-- data ComputeInstanceState = ComputeInstanceState {
--   cmpis
--                                                  } deriving (Show, Generic)
-- instance A.FromJSON ComputeInstanceState where
--   parseJSON = A.genericParseJSON (MSA.aesonOptions "cmpis")

data ComputeType = AKS deriving (Eq, Show, Generic)
instance A.ToJSON ComputeType
instance A.FromJSON ComputeType

data ProvisioningState = Succeeded deriving (Eq, Show, Generic)
instance A.ToJSON ProvisioningState
instance A.FromJSON ProvisioningState
