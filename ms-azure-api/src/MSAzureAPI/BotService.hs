-- | Azure Bot Framework
--
-- https://learn.microsoft.com/en-us/azure/bot-service/rest-api/bot-framework-rest-connector-quickstart?view=azure-bot-service-4.0
module MSAzureAPI.BotService (
  sendMessage
  , sendReply
  -- * Types
  , Activity(..)
  , Attachment(..)
  -- ** Adaptive Card
  , AdaptiveCard(..)
  , ACElement(..)
  -- *** adaptive card elements
  , Image(..)
  , TextBlock(..)
  , ColumnSet(..)
  , Column(..)
                             ) where

import Data.Char (toLower)
import GHC.Exts (IsString(..))
import GHC.Generics (Generic(..))

-- aeson
import qualified Data.Aeson as A (ToJSON(..), genericToJSON, object, (.=), encode, ToJSONKey(..), FromJSON(..), genericParseJSON, withObject, withText, Value(..))
-- hoauth2
import Network.OAuth.OAuth2.Internal (AccessToken(..))

-- req
import Network.HTTP.Req (HttpException, runReq, HttpConfig, defaultHttpConfig, Req, Url, Option, Scheme(..), header, (=:))
-- text
import Data.Text (Text, pack, unpack)

import MSAzureAPI.Internal.Common (run, APIPlane(..), Location(..), locationDisplayName, (==:), get, getBs, post, postRaw, getLbs, put, tryReq, aesonOptions)


-- * Send and receive messages with the Bot Framework : https://learn.microsoft.com/en-us/azure/bot-service/rest-api/bot-framework-rest-connector-send-and-receive-messages?view=azure-bot-service-4.0

-- | Send a reply to a user message
--
-- https://learn.microsoft.com/en-us/azure/bot-service/rest-api/bot-framework-rest-connector-send-and-receive-messages?view=azure-bot-service-4.0#create-a-reply
sendReply :: Activity -- ^ data from the user
          -> Text -- ^ reply text
          -> [Attachment] -- ^ reply attachments
          -> AccessToken -> Req ()
sendReply acti txt atts atok =
  case aReplyToId acti of
    Nothing -> pure ()
    Just aid -> postRaw urib ["v3", "conversations", cid, "activities", aid] mempty actO atok
      where
        urib = aServiceUrl acti
        cid = coaId $ aConversation acti
        actO = mkReplyActivity acti txt atts

mkReplyActivity :: Activity -- ^ coming from the user
                -> Text -- ^ reply text
                -> [Attachment] -- ^ reply attachments
                -> Activity
mkReplyActivity actI = Activity ATMessage Nothing Nothing conO froO recO surl replid
  where
    conO = aConversation actI
    froO = aRecipient actI
    recO = aFrom actI
    surl = aServiceUrl actI
    replid = aReplyToId actI

-- | Send a standalone message
--
-- https://learn.microsoft.com/en-us/azure/bot-service/rest-api/bot-framework-rest-connector-send-and-receive-messages?view=azure-bot-service-4.0#send-a-non-reply-message
sendMessage :: (A.FromJSON b) =>
               Text
            -> Text
            -> Activity
            -> AccessToken -> Req b
sendMessage urib cid =
  postRaw urib ["v3", "conversations", cid, "activities"] mempty

-- | Activity object. Defines a message that is exchanged between bot and user.
--
-- https://learn.microsoft.com/en-us/azure/bot-service/rest-api/bot-framework-rest-connector-api-reference?view=azure-bot-service-4.0#activity-object
data Activity = Activity {
  aType :: ActivityType
  , aId :: Maybe Text
  , aChannelId :: Maybe Text
  , aConversation :: ConversationAccount
  , aFrom :: ChannelAccount
  , aRecipient :: ChannelAccount
  , aServiceUrl :: Text -- ^ URL that specifies the channel's service endpoint. Set by the channel.
  , aReplyToId :: Maybe Text
  , aText :: Text
  , aAttachments :: [Attachment]
                         } deriving (Show, Generic)
instance A.FromJSON Activity where
  parseJSON = A.genericParseJSON (aesonOptions "a")
instance A.ToJSON Activity where
  toJSON = A.genericToJSON (aesonOptions "a")

-- | https://learn.microsoft.com/en-us/azure/bot-service/rest-api/bot-framework-rest-connector-api-reference?view=azure-bot-service-4.0#attachment-object
data Attachment = Attachment {
  attContent :: AdaptiveCard
  , attContentType :: Text
                             } deriving (Show, Generic)
instance A.FromJSON Attachment where
  parseJSON = A.genericParseJSON (aesonOptions "att")
instance A.ToJSON Attachment where
  toJSON = A.genericToJSON (aesonOptions "att")

data AdaptiveCard = AdaptiveCard {
  acBody :: [ACElement] } deriving (Show, Generic)
instance A.FromJSON AdaptiveCard where
  parseJSON = A.genericParseJSON (aesonOptions "ac")
instance A.ToJSON AdaptiveCard where
  toJSON = A.genericToJSON (aesonOptions "ac")

data ACElement = ACEColumnSet ColumnSet
               | ACEColumn Column
               | ACETextBlock TextBlock
               | ACEImage Image
               deriving (Show)
instance A.FromJSON ACElement where
  parseJSON j =
    (ACEColumnSet <$> A.parseJSON j) -- <|> ..
instance A.ToJSON ACElement where
  toJSON = \case
    ACEColumnSet cs -> A.object [
      "type" A..= ("ColumnSet" :: String)
      , "columns" A..= cs ]
    ACEColumn c -> A.object [
      "type" A..= ("Column" :: String)
      , "items" A..= c ]
    ACETextBlock tb -> A.object [
      "type" A..= ("TextBlock" :: String)
      , "text" A..= tb ]
    ACEImage imu -> A.object [
      "type" A..= ("Image" :: String)
      , "url" A..= imu ]

data Image = Image {
  imgUrl :: Text } deriving (Show, Generic)
instance A.ToJSON Image where
  toJSON = A.genericToJSON (aesonOptions "img")
newtype TextBlock = TextBlock {
  tbText :: Text } deriving (Show, Generic) deriving newtype (IsString)
instance A.ToJSON TextBlock where
  toJSON = A.genericToJSON (aesonOptions "tb")
data ColumnSet = ColumnSet {
  colsColumns :: [Column] } deriving (Show, Generic)
instance A.FromJSON ColumnSet where
  parseJSON = A.genericParseJSON (aesonOptions "cols")
instance A.ToJSON ColumnSet where
  toJSON = A.genericToJSON (aesonOptions "cols")
data Column = Column {
  colItems :: [ACElement] } deriving (Show, Generic)
instance A.FromJSON Column where
  parseJSON = A.genericParseJSON (aesonOptions "col")
instance A.ToJSON Column where
  toJSON = A.genericToJSON (aesonOptions "col")


-- | https://learn.microsoft.com/en-us/azure/bot-service/rest-api/bot-framework-rest-connector-api-reference?view=azure-bot-service-4.0#conversationaccount-object
data ConversationAccount = ConversationAccount {
  coaAadObjectId :: Text
  , coaId :: Text
  , coaName :: Text
  , coaIsGroup :: Bool
  } deriving (Show, Generic)
instance A.FromJSON ConversationAccount where
  parseJSON = A.genericParseJSON (aesonOptions "coa")
instance A.ToJSON ConversationAccount where
  toJSON = A.genericToJSON (aesonOptions "coa")

-- | https://learn.microsoft.com/en-us/azure/bot-service/rest-api/bot-framework-rest-connector-api-reference?view=azure-bot-service-4.0#channelaccount-object
data ChannelAccount = ChannelAccount {
  caAadObjectId :: Text
  , caId :: Text
  , caName :: Text
  } deriving (Show, Generic)
instance A.FromJSON ChannelAccount where
  parseJSON = A.genericParseJSON (aesonOptions "ca")
instance A.ToJSON ChannelAccount where
  toJSON = A.genericToJSON (aesonOptions "ca")

data ActivityType = ATMessage
                  | ATContactRelationUpdate
                  | ATConversationUpdate
                  | ATTyping
                  | ATEndOfConversation
                  | ATEvent
                  | ATInvoke
                  | ATDeleteUserData
                  | ATMessageUpdate
                  | ATMessageDelete
                  | ATInstallationUpdate
                  | ATMessageReaction
                  | ATSuggestion
                  | ATTrace
                  | ATHandoff deriving (Eq, Show)
instance A.FromJSON ActivityType where
  parseJSON = A.withText "ActivityType" $ \t -> case t of
    "message" -> pure ATMessage
    "contactRelationUpdate" -> pure ATContactRelationUpdate
    "conversationUpdate" -> pure ATConversationUpdate
    "typing" -> pure ATTyping
    "endOfConversation" -> pure ATEndOfConversation
    "event" -> pure ATEvent
    "invoke" -> pure ATInvoke
    "deleteUserData" -> pure ATDeleteUserData
    "messageUpdate" -> pure ATMessageUpdate
    "messageDelete" -> pure ATMessageDelete
    "installationUpdate" -> pure ATInstallationUpdate
    "messageReaction" -> pure ATMessageReaction
    "suggestion" -> pure ATSuggestion
    "trace" -> pure ATTrace
    "handoff" -> pure ATHandoff
    errstr -> fail $ unwords [unpack errstr, "not a valid ActivityType"]
instance A.ToJSON ActivityType where
  toJSON v = A.String $ (pack . headLower . drop 2 . show) v
    where
      headLower (x:xs) = toLower x : xs
      headLower [] = []
