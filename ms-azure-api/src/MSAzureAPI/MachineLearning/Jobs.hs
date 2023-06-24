module MSAzureAPI.MachineLearning.Jobs where

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
-- hoauth2
-- import Network.OAuth.OAuth2 (OAuth2Token(..))
import Network.OAuth.OAuth2.Internal (AccessToken(..))
-- req
import Network.HTTP.Req (Req, Url, Option, Scheme(..), header, (=:))
-- text
import Data.Text (Text, pack, unpack)
import qualified Data.Text.Lazy as TL (Text, pack, unpack, toStrict)
-- -- time
-- import Data.Time (UTCTime, getCurrentTime)
-- import Data.Time.Format (FormatTime, formatTime, defaultTimeLocale)
-- import Data.Time.LocalTime (getZonedTime)
-- -- xeno
-- import qualified Xeno.DOM.Robust as X (Node, Content(..), name, contents, children)
-- -- xmlbf-xeno
-- import qualified Xmlbf.Xeno as XB (fromRawXml)
-- xmlbf
import qualified Xmlbf as XB (Parser, runParser, pElement, pText)

import qualified MSAzureAPI.Internal.Common as MSA (APIPlane(..), (==:), put, get, getBs, post, getLbs, aesonOptions)



-- | create a job
--
-- docs: <https://learn.microsoft.com/en-us/rest/api/azureml/2023-04-01/jobs/create-or-update?tabs=HTTP>
--
-- @PUT https:\/\/management.azure.com\/subscriptions\/{subscriptionId}\/resourceGroups\/{resourceGroupName}\/providers\/Microsoft.MachineLearningServices\/workspaces\/{workspaceName}\/jobs\/{id}?api-version=2023-04-01@
createJob :: (A.FromJSON b) =>
             Text -- ^ subscription id
          -> Text -- ^ res group id
          -> Text -- ^ ML workspace id
          -> Text -- ^ job id
          -> JobBase
          -> AccessToken -> Req b
createJob sid rgid wsid jid =
  MSA.put MSA.APManagement ["subscriptions", sid,
                    "resourceGroups", rgid,
                    "providers", "Microsoft.MachineLearningServices",
                    "workspaces", wsid,
                    "jobs", jid] ("api-version" MSA.==: "2023-04-01")

data JobBase = JB {
  jbTODO :: Text
  } deriving (Eq, Show, Generic)
instance A.ToJSON JobBase where
  toEncoding = A.genericToEncoding (MSA.aesonOptions "jb")
