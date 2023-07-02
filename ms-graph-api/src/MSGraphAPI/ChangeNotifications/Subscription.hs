-- | Subscription to change notifications through webhooks
--
-- NB: Creating a subscription requires read scope to the resource. For example, to get change notifications on messages, your app needs the @Mail.Read@ permission.
--
-- Currently, subscriptions are enabled for the following resources. Note: Subscriptions to resources marked with an asterisk (*) are supported on the @\/beta@ endpoint only.
--
--
    -- * An alert from the Microsoft Graph Security API.
--
    -- * A baseTask (deprecated) of a user in Microsoft To-Do.*
--
    -- * A callRecord produced after a call or meeting in Microsoft Teams.
--
    -- * A channel in Microsoft Teams.
--
    -- * A chat in Microsoft Teams.
--
    -- * A chatMessage sent via teams or channels in Microsoft Teams.
--
    -- * A conversation in a Microsoft 365 group.
--
    -- * A conversationMember in a team, channel, or chat in Microsoft Teams.
--
    -- * Content in the hierarchy of a root folder 'DriveItem' in OneDrive for Business, or of a root folder or subfolder 'DriveItem' in a user's personal OneDrive.
--
    -- * A 'Group' in Azure Active Directory.
--
    -- * A list under a SharePoint site.
--
    -- * A message, event, or contact in Outlook.
--
    -- * An online meeting in Microsoft Teams.*
--
    -- * The presence of a user in Microsoft Teams.
--
    -- * A team in Microsoft Teams.
--
    -- * A printer (when a print job for the printer gets to JobFetchable state - ready to be fetched for printing) and a printTaskDefinition in Universal Print. For more information, see Subscribe to change notifications from cloud printing APIs.
--
    -- * A todoTask of a user in Microsoft To Do (webhooks are only available in the worldwide endpoint and no other national clouds).
--
    -- * A 'User' in Azure Active Directory.
--
--
-- See <https://learn.microsoft.com/en-us/graph/webhooks#supported-resources> for an up to date list of resources that can produce change notifications
--
-- see <https://learn.microsoft.com/en-us/graph/change-notifications-delivery-webhooks?tabs=http> for protocol details
module MSGraphAPI.ChangeNotifications.Subscription (
  -- * Sender
  createSubscription
  -- ** types
  , Subscription(..)
    , ChangeType(..)
  -- * Receiver
  -- ** types
  , ChangeNotification(..)
                                                   ) where

import Data.List.NonEmpty (NonEmpty(..))
import GHC.Generics (Generic(..))

-- aeson
import qualified Data.Aeson as A (ToJSON(..), FromJSON(..), encode, eitherDecode, genericParseJSON, genericToEncoding, defaultOptions, Options(..), withObject, withText, (.:), (.:?), object, (.=))
import qualified Data.Aeson.Encoding as A (text)
-- hoauth
import Network.OAuth.OAuth2.Internal (AccessToken(..))
-- req
import Network.HTTP.Req (Req)
-- text
import Data.Text (Text, pack, unpack)
-- time
import Data.Time (UTCTime)
-- import Data.UUID.Types (UUID)

import qualified MSGraphAPI.Internal.Common as MSG (Collection(..), get, post, aesonOptions)
import MSGraphAPI.Files.DriveItem (DriveItem)
import MSGraphAPI.Users.Group (Group)
import MSGraphAPI.Users.User (User)

-- | Represents the notification sent to the subscriber. https://learn.microsoft.com/en-us/graph/api/resources/changenotification?view=graph-rest-1.0
--
--
data ChangeNotification a = ChangeNotification {
  cnChangeType :: ChangeType -- ^ type of change that will raise the change notification.
  , cnClientState :: Text -- ^ Value of the clientState property sent in the subscription request (if any). The maximum length is 255 characters. The client can check whether the change notification came from the service by comparing the values of the clientState property.
  , cnId :: Text -- ^ Unique ID for the notification.
  , cnResource :: Text -- ^ The URI of the resource that emitted the change notification relative to @https:\/\/graph.microsoft.com@
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
-- NB: Creating a subscription requires read scope to the resource. For example, to get change notifications on messages, your app needs the @Mail.Read@ permission.
--
-- NB2: When you create a subscription to receive change notifications through webhooks, MS Graph first validates the notification endpoint that's provided in the @notificationUrl@ property of the subscription request. MS Graph encodes a validation token and includes it in a @POST@ request to the notification URL, the client must properly decode the URL to get the plain text validation token, and respond within 10 seconds.  See   https://learn.microsoft.com/en-us/graph/change-notifications-delivery-webhooks?tabs=http#notificationurl-validation for full details
createSubscription :: (A.FromJSON b) =>
                      Subscription -- ^ Configuration of the change webhook to be created by this request
                   -> AccessToken -> Req b
createSubscription = MSG.post ["subscriptions"] mempty

-- | A subscription allows a client app to receive change notifications about changes to data in Microsoft Graph.
--
-- https://learn.microsoft.com/en-us/graph/api/resources/subscription?view=graph-rest-1.0
data Subscription = Subscription {
  cnsChangeType :: NonEmpty ChangeType -- ^ Type of change in the subscribed resource that will raise a change notification. 
  , cnsClientState :: Text -- ^ Value of the clientState property sent by the service in each change notification. The maximum length is 128 characters. The client can check that the change notification came from the service by comparing the value of the clientState property sent with the subscription with the value of the clientState property received with each change notification.
  , cnsExpirationDateTime :: UTCTime -- ^ date and time when the webhook subscription expires. The time is in UTC, and can be an amount of time from subscription creation that varies for the resource subscribed to. For the maximum supported subscription length of time, see https://learn.microsoft.com/en-us/graph/api/resources/subscription?view=graph-rest-1.0#maximum-length-of-subscription-per-resource-type
  , cnsNotificationUrl :: Text -- ^ URL of the endpoint that will receive the change notifications. This URL must make use of the HTTPS protocol. Any query string parameter included in the notificationUrl property will be included in the HTTP POST request when Microsoft Graph sends the change notifications.
  , cnsResource :: Text -- ^ The resource that will be monitored for changes, e.g. @"\/users\/{id}\/drive\/root"@. Do not include the base URL (@https:\/\/graph.microsoft.com\/v1.0\/@)
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

-- | the type of change in the subscribed resource that will raise a change notification.
--
-- Note:
--
-- * Drive root item and list change notifications support only the updated changeType.
--
-- * User and group change notifications support updated and deleted changeType. Use updated to receive notifications when user or group is created, updated or soft deleted. Use deleted to receive notifications when user or group is permanently deleted.
data ChangeType = CTCreated -- ^ created
                | CTUpdated -- ^ updated
                | CTDeleted -- ^ deleted
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
