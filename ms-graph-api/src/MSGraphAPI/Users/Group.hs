-- | Users.Group
module MSGraphAPI.Users.Group (
  -- * Teams
  -- ** Joined teams
  listUserJoinedTeams
  , listMeJoinedTeams
  -- ** Associated teams
  , listUserAssociatedTeams
  , listMeAssociatedTeams
  -- * Team channels
  , listTeamChannels
  -- ** Channel messages
  , listChannelMessages
  , listMessageReplies
  -- * Drive items
  , listGroupsDriveItems
  -- * types
  , Group(..)
  , Channel(..)
  -- ** Chat messages
  , ChatMessage(..)
  , ChatMessageBody(..)
                              )where

import GHC.Generics (Generic(..))

-- aeson
import qualified Data.Aeson as A (ToJSON(..), FromJSON(..), genericParseJSON)
-- hoauth
import Network.OAuth.OAuth2.Internal (AccessToken(..))
-- req
import Network.HTTP.Req (Req)
-- text
import Data.Text (Text, unpack)
-- time
import Data.Time (ZonedTime)

import qualified MSGraphAPI.Internal.Common as MSG (Collection(..), get, aesonOptions)
import MSGraphAPI.Files.DriveItem (DriveItem)

-- | Groups are collections of principals with shared access to resources in Microsoft services or in your app. Different principals such as users, other groups, devices, and applications can be part of groups. 
--
-- https://learn.microsoft.com/en-us/graph/api/resources/groups-overview?view=graph-rest-1.0&tabs=http
data Group = Group {
  gId :: Text
  , gDisplayName :: Text
  , gDescription :: Text
                   } deriving (Eq, Ord, Show, Generic)
instance A.FromJSON Group where
  parseJSON = A.genericParseJSON (MSG.aesonOptions "g")
instance A.ToJSON Group

-- | Teams are made up of channels, which are the conversations you have with your teammates. Each channel is dedicated to a specific topic, department, or project. Channels are where the work actually gets done - where text, audio, and video conversations open to the whole team happen, where files are shared, and where tabs are added.
--
-- https://learn.microsoft.com/en-us/graph/api/resources/channel?view=graph-rest-1.0
data Channel = Channel {
  chId :: Text
  , chDisplayName :: Text
  , chDescription :: Text
                       } deriving (Eq, Ord, Show, Generic)
instance A.FromJSON Channel where
  parseJSON = A.genericParseJSON (MSG.aesonOptions "ch")
instance A.ToJSON Channel

-- | Get the list of channels either in this team or shared with this team (incoming channels).
--
-- @GET \/teams\/{team-id}\/allChannels@
listTeamChannels :: Text -- ^ team ID
                 -> AccessToken -> Req (MSG.Collection Channel)
listTeamChannels tid = MSG.get ["teams", tid, "allChannels"] mempty



-- | Retrieve the list of messages (without the replies) in a channel of a team.
--
-- To get the replies for a message, call the 'listMessageReplies' or the get message reply API.
--
-- @GET \/teams\/{team-id}\/channels\/{channel-id}\/messages@
listChannelMessages ::
  Text -- ^ team ID
  -> Text -- ^ channel ID
  -> AccessToken -> Req (MSG.Collection ChatMessage)
listChannelMessages tid chid =
  MSG.get ["teams", tid, "channels", chid, "messages"] mempty

-- | List all the replies to a message in a channel of a team.
--
-- This method lists only the replies of the specified message, if any. To get the message itself, simply call get channel message.
--
-- GET /teams/{team-id}/channels/{channel-id}/messages/{message-id}/replies
listMessageReplies ::
  Text -- ^ team ID
  -> Text -- ^ channel ID
  -> Text -- ^ message ID
  -> AccessToken -> Req (MSG.Collection ChatMessage)
listMessageReplies tid chid mid =
  MSG.get ["teams", tid, "channels", chid, "messages", mid, "replies"] mempty


-- | An individual chat message within a channel or chat. The message can be a root message or part of a thread 
--
-- https://learn.microsoft.com/en-us/graph/api/resources/chatmessage?view=graph-rest-1.0
data ChatMessage = ChatMessage {
  chamBody :: ChatMessageBody
  , chamId :: Text
  , chamCreatedDateTime :: ZonedTime
  , chamDeletedDateTime :: Maybe ZonedTime
                               } deriving (Show, Generic)
instance A.FromJSON ChatMessage where
  parseJSON = A.genericParseJSON (MSG.aesonOptions "cham")
instance A.ToJSON ChatMessage

data ChatMessageBody = ChatMessageBody {
  chambId :: Text
                                       } deriving (Eq, Ord, Generic)
instance Show ChatMessageBody where
  show = unpack . chambId
instance A.FromJSON ChatMessageBody where
  parseJSON = A.genericParseJSON (MSG.aesonOptions "chamb")
instance A.ToJSON ChatMessageBody



-- | Get the list of teams in Microsoft Teams that a user is associated with.
--
-- @GET \/users\/{user-id}\/teamwork\/associatedTeams@
--
-- Currently, a user can be associated with a team in two different ways:
--
--  * A user can be a direct member of a team.
--  * A user can be a member of a shared channel that is hosted inside a team.
listUserAssociatedTeams :: Text -- ^ User ID
                       -> AccessToken -> Req (MSG.Collection Group)
listUserAssociatedTeams uid = MSG.get ["users", uid, "teamwork", "associatedTeams"] mempty

-- | Get the teams in Microsoft Teams that the current user is associated with (see 'getUserAssociatedTeams').
listMeAssociatedTeams :: AccessToken -> Req (MSG.Collection Group)
listMeAssociatedTeams = MSG.get ["me", "teamwork", "associatedTeams"] mempty

-- | Get the teams in Microsoft Teams that the given user is a direct member of.
--
-- @GET \/users\/{id | user-principal-name}\/joinedTeams@
--
-- https://learn.microsoft.com/en-us/graph/api/user-list-joinedteams?view=graph-rest-1.0&tabs=http
listUserJoinedTeams :: Text -- ^ User ID
                   -> AccessToken -> Req (MSG.Collection Group)
listUserJoinedTeams uid = MSG.get ["users", uid, "joinedTeams"] mempty

-- | Get the teams in Microsoft Teams that the current user is a direct member of.
--
-- @GET \/me\/joinedTeams@
--
-- https://learn.microsoft.com/en-us/graph/api/user-list-joinedteams?view=graph-rest-1.0&tabs=http
listMeJoinedTeams :: AccessToken -> Req (MSG.Collection Group)
listMeJoinedTeams = MSG.get ["me", "joinedTeams"] mempty

-- | Get the 'DriveItem's in the 'Group' storage, starting from the root item
--
-- @GET \/groups\/{group-id}\/drive\/root\/children@
--
-- https://learn.microsoft.com/en-us/graph/api/driveitem-list-children?view=graph-rest-1.0&tabs=http
--
-- NB : requires @Files.Read.All@, since it tries to access all files a user has access to.
listGroupsDriveItems :: Text -- ^ Group ID
                    -> AccessToken -> Req (MSG.Collection DriveItem)
listGroupsDriveItems gid = MSG.get ["groups", gid, "drive", "root", "children"] mempty


-- data X = X { xName :: Text } deriving (Eq, Ord, Show, Generic)
-- instance A.FromJSON X where
--   parseJSON = A.genericParseJSON (MSG.aesonOptions "x")

-- pt0 :: Either String (MSG.Collection DriveItem)
-- pt0 = A.eitherDecode t0
--   where
--     t0 = "{\r\n  \"value\": [\r\n    {\"name\": \"myfile.jpg\"  },\r\n    {\"name\": \"Documents\" },\r\n    {\"name\": \"Photos\" },\r\n    {\"name\": \"my sheet(1).xlsx\"}\r\n  ]\r\n}"
