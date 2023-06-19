module MSGraphAPI.ChangeNotifications.Subscription where

import Data.List.NonEmpty (NonEmpty)
import GHC.Generics (Generic(..))

-- aeson
import qualified Data.Aeson as A (ToJSON(..), FromJSON(..), eitherDecode, genericParseJSON, defaultOptions, Options(..), withObject, withText, (.:), (.:?), object, (.=))
import qualified Data.Aeson.Encoding as A (text)
-- hoauth
import Network.OAuth.OAuth2.Internal (AccessToken(..))
-- req
import Network.HTTP.Req (Req)
-- text
import Data.Text (Text, pack, unpack)
-- time
import Data.Time (LocalTime)

import qualified MSGraphAPI.Internal.Common as MSG (Collection(..), get, post, aesonOptions)
import MSGraphAPI.Files.DriveItem (DriveItem)

-- | A subscription allows a client app to receive change notifications about changes to data in Microsoft Graph.
--
-- https://learn.microsoft.com/en-us/graph/api/resources/subscription?view=graph-rest-1.0
data Subscription = Subscription {
  cnsId :: Text
  , cnsChangeType :: NonEmpty ChangeType
  , cnsClientState :: Text
  , cnsExpirationDateTime :: LocalTime
  , cnsNotificationUrl :: Text -- ^ The URL of the endpoint that will receive the change notifications. This URL must make use of the HTTPS protocol. Any query string parameter included in the notificationUrl property will be included in the HTTP POST request when Microsoft Graph sends the change notifications.
  , cnsResource :: Text -- ^ Specifies the resource that will be monitored for changes. Do not include the base URL (https://graph.microsoft.com/v1.0/)
                   } deriving (Eq, Show, Generic)
instance A.FromJSON Subscription where
  parseJSON = A.genericParseJSON (MSG.aesonOptions "cns")


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
