module MSAzureAPI.MachineLearning.Jobs (
  createJob
  , listJobs
  , JobBaseResource(..)
  , JobBase(..)
  , Status(..)
  , SystemData(..)
                                       ) where

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
import Network.HTTP.Req (Req, Url, Option, Scheme(..), header, (=:))
-- text
import Data.Text (Text, pack, unpack)
import qualified Data.Text.Lazy as TL (Text, pack, unpack, toStrict)
-- time
import Data.Time (UTCTime, getCurrentTime)
-- import Data.Time.Format (FormatTime, formatTime, defaultTimeLocale)
-- import Data.Time.LocalTime (getZonedTime)
-- -- xeno
-- import qualified Xeno.DOM.Robust as X (Node, Content(..), name, contents, children)
-- -- xmlbf-xeno
-- import qualified Xmlbf.Xeno as XB (fromRawXml)
-- xmlbf
-- import qualified Xmlbf as XB (Parser, runParser, pElement, pText)

import qualified MSAzureAPI.Internal.Common as MSA (APIPlane(..), Collection, (==:), put, get, getBs, post, getLbs, aesonOptions)

-- | List jobs
--
-- @ GET https:\/\/management.azure.com\/subscriptions\/{subscriptionId}\/resourceGroups\/{resourceGroupName}\/providers\/Microsoft.MachineLearningServices\/workspaces\/{workspaceName}\/jobs?api-version=2023-04-01&$skip={$skip}&jobType={jobType}&tag={tag}&listViewType={listViewType}@
listJobs ::
  Text -- ^ subscription id
  -> Text -- ^ res group id
  -> Text -- ^ ML workspace id
  -> AccessToken -> Req (MSA.Collection JobBaseResource)
listJobs sid rgid wsid = MSA.get MSA.APManagement [
    "subscriptions", sid,
    "resourceGroups", rgid,
    "providers", "Microsoft.MachineLearningServices",
    "workspaces", wsid,
    "jobs"
    ] ("api-version" MSA.==: "2023-04-01")


-- | Create a job
--
-- docs: <https://learn.microsoft.com/en-us/rest/api/azureml/2023-04-01/jobs/create-or-update?tabs=HTTP>
--
-- @PUT https:\/\/management.azure.com\/subscriptions\/{subscriptionId}\/resourceGroups\/{resourceGroupName}\/providers\/Microsoft.MachineLearningServices\/workspaces\/{workspaceName}\/jobs\/{id}?api-version=2023-04-01@
createJob ::
  Text -- ^ subscription id
  -> Text -- ^ res group id
  -> Text -- ^ ML workspace id
  -> Text -- ^ job id
  -> JobBase
  -> AccessToken -> Req JobBaseResource
createJob sid rgid wsid jid =
  MSA.put MSA.APManagement [
  "subscriptions", sid,
    "resourceGroups", rgid,
    "providers", "Microsoft.MachineLearningServices",
    "workspaces", wsid,
    "jobs", jid] ("api-version" MSA.==: "2023-04-01")

-- | https://learn.microsoft.com/en-us/rest/api/azureml/2023-04-01/jobs/create-or-update?tabs=HTTP#jobbaseresource
data JobBaseResource = JobBaseResource {
  jbrId :: Text
  , jbrName :: Text
  , jbrSystemData :: SystemData
  , jbrProperties :: JobBase
                                       } deriving (Eq, Show, Generic)
instance A.FromJSON JobBaseResource where
  parseJSON = A.genericParseJSON (MSA.aesonOptions "jbr")

data SystemData = SystemData {
  sdCreatedAt :: UTCTime
  , sdCreatedBy :: Text
  , srLastModifiedAt :: UTCTime
  , srLastModifiedBy :: Text
                             } deriving (Eq, Show, Generic)
instance A.FromJSON SystemData where
  parseJSON = A.genericParseJSON (MSA.aesonOptions "sd")

-- | JobBase
--
-- https://learn.microsoft.com/en-us/rest/api/azureml/2023-04-01/jobs/create-or-update?tabs=HTTP
-- data JobBase = JBAutoMLJob {
--   jbStatus :: Status
--   , jbComponentId :: Text
--   , jb
--                            } -- ^ https://learn.microsoft.com/en-us/rest/api/azureml/2023-04-01/jobs/list?tabs=HTTP#automljob
--              | JBCommandJob {
--   jbStatus :: Status
--                            }
--              | JBPipelineJob {
--   jbStatus :: Status
--                            }
--              | JBSweepJob {
--   jbStatus :: Status
--                            }
data JobBase = JobBase {
    jbStatus :: Status
  , jbComponentId :: Text
  , jbComputeId :: Text
  , jbDescription :: Text
  , jbDisplayName :: Text
  -- , jbInputs :: A.Value -- AutoMLJob doesn't have inputs
  , jbOutputs :: A.Value
  , jbProperties :: A.Value
                       }
             deriving (Eq, Show, Generic)
instance A.FromJSON JobBase where
  parseJSON = A.genericParseJSON (MSA.aesonOptions "jb")
instance A.ToJSON JobBase where
  toEncoding = A.genericToEncoding (MSA.aesonOptions "jb")

data Status = CancelRequested
            | Canceled
            | Completed
            | Failed
            | Finalizing
            | NotResponding
            | NotStarted
            | Paused
            | Preparing
            | Provisioning
            | Queued
            | Running
            | Starting
            | Unknown
            deriving (Eq, Show, Generic)
instance A.FromJSON Status
instance A.ToJSON Status
