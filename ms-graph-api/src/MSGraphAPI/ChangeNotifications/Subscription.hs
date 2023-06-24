-- | Subscription to change notifications through webhooks
--
-- see <https://learn.microsoft.com/en-us/graph/webhooks#supported-resources> for a list of resources that can produce change notifications
--
-- see <https://learn.microsoft.com/en-us/graph/change-notifications-delivery-webhooks?tabs=http> for protocol details
module MSGraphAPI.ChangeNotifications.Subscription where

import Data.List.NonEmpty (NonEmpty)
import GHC.Generics (Generic(..))

-- aeson
import qualified Data.Aeson as A (ToJSON(..), FromJSON(..), eitherDecode, genericParseJSON, genericToEncoding, defaultOptions, Options(..), withObject, withText, (.:), (.:?), object, (.=))
import qualified Data.Aeson.Encoding as A (text)
-- hoauth
import Network.OAuth.OAuth2.Internal (AccessToken(..))
-- req
import Network.HTTP.Req (Req)
-- text
import Data.Text (Text, pack, unpack)
-- time
import Data.Time (LocalTime)
-- import Data.UUID.Types (UUID)

import qualified MSGraphAPI.Internal.Common as MSG (Collection(..), get, post, aesonOptions)
import MSGraphAPI.Files.DriveItem (DriveItem)

-- | Represents the notification sent to the subscriber. https://learn.microsoft.com/en-us/graph/api/resources/changenotification?view=graph-rest-1.0
--
-- 
data ChangeNotification a = ChangeNotification {
  cnChangeType :: ChangeType
  , cnClientState :: Text
  , cnId :: Text
  , cnResource :: Text
  , cnResourceData :: Maybe a
  , cnSubscriptionId :: Text
  , cnTenantId :: Text
                                             } deriving (Eq, Show, Generic)
instance A.FromJSON a => A.FromJSON (ChangeNotification a) where
  parseJSON = A.genericParseJSON (MSG.aesonOptions "cn")

-- | Create a subscription https://learn.microsoft.com/en-us/graph/api/subscription-post-subscriptions?view=graph-rest-1.0&tabs=http
--
-- @ POST https:\/\/graph.microsoft.com\/v1.0\/subscriptions@
--
-- Creating a subscription requires read scope to the resource. For example, to get change notifications on messages, your app needs the @Mail.Read@ permission.
createSubscription :: (A.FromJSON b) => Subscription -> AccessToken -> Req b
createSubscription = MSG.post ["subscriptions"] mempty

-- | A subscription allows a client app to receive change notifications about changes to data in Microsoft Graph.
--
-- https://learn.microsoft.com/en-us/graph/api/resources/subscription?view=graph-rest-1.0
data Subscription = Subscription {
  cnsId :: Text
  , cnsChangeType :: NonEmpty ChangeType
  , cnsClientState :: Text
  , cnsExpirationDateTime :: LocalTime
  , cnsNotificationUrl :: Text -- ^ The URL of the endpoint that will receive the change notifications. This URL must make use of the HTTPS protocol. Any query string parameter included in the notificationUrl property will be included in the HTTP POST request when Microsoft Graph sends the change notifications.
  , cnsResource :: Text -- ^ Specifies the resource that will be monitored for changes. Do not include the base URL (@https:\/\/graph.microsoft.com\/v1.0\/@)
  , cnsLatestSupportedTLSVersion :: LatestTLSVer
                   } deriving (Eq, Show, Generic)
instance A.FromJSON Subscription where
  parseJSON = A.genericParseJSON (MSG.aesonOptions "cns")
instance A.ToJSON Subscription where
  toEncoding = A.genericToEncoding (MSG.aesonOptions "cns")

data LatestTLSVer = LTV10 | LTV11 | LTV12 | LTV13 deriving (Eq, Show, Generic)
instance A.FromJSON LatestTLSVer where
  parseJSON = A.withText "LatestTLSVer" $ \t ->
    case t of
      "v1_0" -> pure LTV10
      "v1_1" -> pure LTV11
      "v1_2" -> pure LTV12
      "v1_3" -> pure LTV13
      x -> fail $ unwords ["LatestTLSVer : unexpected value:", unpack x]
instance A.ToJSON LatestTLSVer where
  toEncoding = \case
    LTV10 -> A.text "v1_0"
    LTV11 -> A.text "v1_1"
    LTV12 -> A.text "v1_2"
    LTV13 -> A.text "v1_3"

data ChangeType = CTCreated
                | CTUpdated
                | CTDeleted
                deriving (Eq, Show, Generic)
instance A.FromJSON ChangeType where
  parseJSON = A.withText "ChangeType" $ \t ->
    case t of
      "created" -> pure CTCreated
      "updated" -> pure CTUpdated
      "deleted" -> pure CTDeleted
      x -> fail $ unwords ["ChangeType : unexpected value:", unpack x]
instance A.ToJSON ChangeType where
  toEncoding = \case
    CTCreated -> A.text "created"
    CTUpdated -> A.text "updated"
    CTDeleted -> A.text "deleted"
