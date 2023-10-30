module MSAzureAPI.MachineLearning.Usages where

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

import qualified MSAzureAPI.Internal.Common as MSA (APIPlane(..), (==:), put, get, getBs, post, getLbs, Collection, Location, showLocation, aesonOptions)

-- | Gets the current usage information as well as limits for AML resources for given subscription and location.
--
-- docs : <https://learn.microsoft.com/en-us/rest/api/azureml/2023-04-01/usages/list?tabs=HTTP>
--
-- @GET https:\/\/management.azure.com\/subscriptions\/{subscriptionId}\/providers\/Microsoft.MachineLearningServices\/locations\/{location}\/usages?api-version=2023-04-01@
getUsages :: Text -- ^ subscription ID
          -> MSA.Location -- ^ location
          -> AccessToken -> Req (MSA.Collection Usage)
getUsages sid loc = MSA.get MSA.APManagement [
  "subscriptions", sid,
  "providers", "Microsoft.MachineLearningServices",
  "locations", MSA.showLocation loc,
  "usages"
  ] ("api-version" MSA.==: "2023-04-01")

data Usage = Usage {
  uCurrentValue :: Int
  , uLimit :: Int
  , uType :: Text
  , uName :: UsageName
                   } deriving (Eq, Show, Ord, Generic)
instance A.FromJSON Usage where
  parseJSON = A.genericParseJSON (MSA.aesonOptions "u")

data UsageName = UsageName {
  unLocalizedValue :: Text
  , unValue :: Text
                           } deriving (Eq, Show, Ord, Generic)
instance A.FromJSON UsageName where
  parseJSON = A.genericParseJSON (MSA.aesonOptions "un")


