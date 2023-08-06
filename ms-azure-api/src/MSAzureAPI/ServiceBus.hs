module MSAzureAPI.ServiceBus where

import GHC.Generics (Generic(..))

-- aeson
import qualified Data.Aeson as A (ToJSON(..), genericToJSON, object, (.=), ToJSONKey(..), FromJSON(..), genericParseJSON, encode)
-- containers
import qualified Data.Map as M (Map, singleton, fromList)
-- hoauth2
import Network.OAuth.OAuth2.Internal (AccessToken(..))
-- req
import Network.HTTP.Req (HttpException, runReq, HttpConfig, defaultHttpConfig, Req, Url, Option, Scheme(..), header, (=:))
-- text
import Data.Text (Text, pack, unpack)
-- time
import Data.Time (UTCTime, getCurrentTime)
import Data.Time.Format (FormatTime, formatTime, defaultTimeLocale)
import Data.Time.LocalTime (getZonedTime)

import MSAzureAPI.Internal.Common (run, APIPlane(..), Location(..), locationDisplayName, (==:), get, getBs, post, getLbs, put, tryReq, aesonOptions)


newtype MessageBatch a = MessageBatch [a] deriving (Eq, Show)
instance A.ToJSON a => A.ToJSON (MessageBatch a) where
  toJSON (MessageBatch xs) = A.toJSON $ map (\x -> M.singleton ("Body" :: String) x) xs

-- | Create a service bus topic
--
-- https://learn.microsoft.com/en-us/rest/api/servicebus/controlplane-stable/topics/create-or-update?tabs=HTTP
createTopic ::
  Text -- ^ subscription id
  -> Text -- ^ RG name
  -> Text -- ^ namespace name
  -> Text -- ^ topic name
  -> TopicCreate
  -> AccessToken -> Req ()
createTopic subid rgname nname tname = put APManagement [
  "subscriptions", subid
  , "resourceGroup", rgname
  , "providers", "Microsoft.ServiceBus"
  , "namespaces", nname
  , "topicName", tname
  ] ("api-version" ==: "2021-11-01")

data TopicCreate = TopicCreate {
  tcProperties :: TCProperties
                               } deriving (Eq, Show, Generic)

instance A.ToJSON TopicCreate where
  toJSON = A.genericToJSON (aesonOptions "tc")
data TCProperties = TCProperties {
  tcpEnableBatchedOperations :: Bool -- ^ enable batched operations on the backend
                                 } deriving (Eq, Show, Generic)
instance A.ToJSON TCProperties where
  toJSON = A.genericToJSON (aesonOptions "tcp")

-- | Create a service bus queue using default options
--
-- https://learn.microsoft.com/en-us/rest/api/servicebus/controlplane-stable/queues/create-or-update?tabs=HTTP
createQueue ::
  Text -- ^ subscription id
  -> Text -- ^ RG name
  -> Text -- ^ namespace name
  -> Text -- ^ queue name
  -> AccessToken
  -> Req QueueCreateResponse
createQueue subid rgname nname qname = put APManagement [
    "subscriptions", subid
  , "resourceGroup", rgname
  , "providers", "Microsoft.ServiceBus"
  , "namespaces", nname
  , "queues", qname
  ] ("api-version" ==: "2021-11-01") ()

data QueueCreateResponse = QueueCreateResponse {
  qcrId :: Text
  , qcrProperties :: QCRProperties
                                               } deriving (Eq, Show, Generic)
instance A.FromJSON QueueCreateResponse where
  parseJSON = A.genericParseJSON (aesonOptions "qcr")

data QCRProperties = QCRProperties {
  qcrpMaxMessageSizeInKilobytes :: Int
                                   } deriving (Eq, Show, Generic)
instance A.FromJSON QCRProperties where
  parseJSON = A.genericParseJSON (aesonOptions "qcrp")

-- | Create a service bus namespace
--
-- https://learn.microsoft.com/en-us/rest/api/servicebus/controlplane-stable/namespaces/create-or-update?tabs=HTTP#namespacecreate
createNamespace ::
  Text -- ^ subscription id
  -> Text -- ^ RG name
  -> Text -- ^ namespace name
  -> NameSpaceCreate
  -> AccessToken
  -> Req NameSpaceCreateResponse
createNamespace subid rgname nname = put APManagement [
  "subscriptions", subid
  , "resourceGroup", rgname
  , "providers", "Microsoft.ServiceBus"
  , "namespaces", nname
  ] ("api-version" ==: "2021-11-01")

-- | https://learn.microsoft.com/en-us/rest/api/servicebus/controlplane-stable/namespaces/create-or-update?tabs=HTTP#namespacecreate
data NameSpaceCreate = NameSpaceCreate {
  sku :: Sku
  , location :: Location
                                       } deriving (Eq, Show, Generic)
instance A.ToJSON NameSpaceCreate

data NameSpaceCreateResponse = NameSpaceCreateResponse {
  nscrId :: Text
  , nscrProperties :: NSCRProperties
                                                       } deriving (Eq, Show, Generic)
instance A.FromJSON NameSpaceCreateResponse where
  parseJSON = A.genericParseJSON (aesonOptions "nscr")

data NSCRProperties = NSCRProperties {
  nscrpCreatedAt :: UTCTime
  , nscrpServiceBusEndpoint :: Text
                                     } deriving (Eq, Show, Generic)
instance A.FromJSON NSCRProperties where
  parseJSON = A.genericParseJSON (aesonOptions "nscrp")

data Sku = Sku {
  skuName :: SkuName
               } deriving (Eq, Show)
-- | name and tier are rendered as the same thing
instance A.ToJSON Sku where
  toJSON (Sku n) = A.object [
    "name" A..= n
    , "tier" A..= n
                              ]

data SkuName = Basic | Premium | Standard deriving (Eq, Show, Generic)
instance A.ToJSON SkuName
