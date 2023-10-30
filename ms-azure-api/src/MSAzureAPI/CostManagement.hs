-- |
--
-- auth: needs @user_impersonation@ scope
module MSAzureAPI.CostManagement where

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
import Data.Time.Calendar (Day)
import Data.Time (UTCTime, getCurrentTime)
import Data.Time.Format (FormatTime, formatTime, defaultTimeLocale)
import Data.Time.LocalTime (ZonedTime, getZonedTime)

import qualified MSAzureAPI.Internal.Common as MSA (Collection, APIPlane(..), (==:), put, get, getBs, post, getLbs, aesonOptions)


{- generate cost details report https://learn.microsoft.com/en-us/rest/api/cost-management/generate-cost-details-report/create-operation?tabs=HTTP
-}

-- POST https://management.azure.com/{scope}/providers/Microsoft.CostManagement/generateCostDetailsReport?api-version=2023-08-01

generateCostDetailsReport :: (A.FromJSON b) =>
                             Text -> CDROptions -> AccessToken -> Req b
generateCostDetailsReport rid = MSA.post MSA.APManagement [
  rid
  , "providers", "Microsoft.CostManagement"
  , "generateCostDetailsReport"
  ] ("api-version" MSA.==: "2023-08-01")

data CDRQueryResult = CDRQResult {
  cdrqrLocation :: Text
                                 } deriving (Show, Generic)
instance A.FromJSON CDRQueryResult where
  parseJSON = A.genericParseJSON (MSA.aesonOptions "cdrqr")


data CDROptions = CDROptions {
  cdrTimePeriod :: CDRTimePeriod
                       } deriving (Show, Generic)
instance A.FromJSON CDROptions where
  parseJSON = A.genericParseJSON (MSA.aesonOptions "cdr")
instance A.ToJSON CDROptions where
  toEncoding = A.genericToEncoding (MSA.aesonOptions "cdr")

data CDRTimePeriod = CDRTimePeriod {
  cdrtpStart :: Day
  , cdrtpEnd :: Day
                       } deriving (Show, Generic)
instance A.FromJSON CDRTimePeriod where
  parseJSON = A.genericParseJSON (MSA.aesonOptions "cdrtp")
instance A.ToJSON CDRTimePeriod where
  toEncoding = A.genericToEncoding (MSA.aesonOptions "cdrtp")
