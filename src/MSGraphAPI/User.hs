module MSGraphAPI.User where

import GHC.Generics (Generic(..))

import Data.List (sort, sortBy, stripPrefix, uncons)
import Data.Maybe (listToMaybe, fromMaybe)
-- import Data.Ord (comparing)
import Data.Char (toLower)
import Data.String (IsString(..))
import Data.Word (Word)

-- aeson
import qualified Data.Aeson as A (ToJSON(..), FromJSON(..), genericParseJSON, defaultOptions, Options(..), withObject, withText, (.:), (.:?), object, (.=), Key, Value, camelTo2)
import qualified Data.Aeson.Types as A (Parser, Object)
-- import qualified Data.Aeson.KeyMap as AKV (KeyMap, lookup)
-- containers
-- import qualified Data.Map as M (Map, empty, insert, lookup)
-- hoauth
import Network.OAuth.OAuth2.Internal (AccessToken(..))
-- req
import Network.HTTP.Req (Req)
-- text
import Data.Text (Text, pack, unpack)
-- time
import Data.Time.LocalTime (ZonedTime, zonedTimeToLocalTime)

import qualified MSGraphAPI.Internal.Common as MSG (get, post, aesonOptions)

-- | https://learn.microsoft.com/en-us/graph/api/user-get?view=graph-rest-1.0&tabs=http#request
get :: Text -- ^ user id
    -> AccessToken -> Req User
get uid = MSG.get ["users", uid] mempty

-- | get signed-in user https://learn.microsoft.com/en-us/graph/api/user-get?view=graph-rest-1.0&tabs=http#request-1
getMe :: AccessToken -> Req User
getMe = MSG.get ["me"] mempty


data User = User {
  uId :: Text
  , uUserPrincipalName :: Text
  , uDisplayName :: Text
                 } deriving (Eq, Ord, Show, Generic)
instance A.FromJSON User where
  parseJSON = A.genericParseJSON (MSG.aesonOptions "u")


