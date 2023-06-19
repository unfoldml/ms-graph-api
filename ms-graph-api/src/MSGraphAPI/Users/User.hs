-- | User
module MSGraphAPI.Users.User where

import GHC.Generics (Generic(..))

-- aeson
import qualified Data.Aeson as A (ToJSON(..), FromJSON(..), genericParseJSON, defaultOptions, Options(..), withObject, withText, (.:), (.:?), object, (.=))
-- hoauth
import Network.OAuth.OAuth2.Internal (AccessToken(..))
-- req
import Network.HTTP.Req (Req)
-- text
import Data.Text (Text, pack, unpack)

import qualified MSGraphAPI.Internal.Common as MSG (get, post, aesonOptions)


data User = User {
  uId :: Text
  , uUserPrincipalName :: Text
  , uDisplayName :: Text
                 } deriving (Eq, Ord, Show, Generic)
instance A.FromJSON User where
  parseJSON = A.genericParseJSON (MSG.aesonOptions "u")


-- | Get user information
--
-- @GET \/users\/{user-id}@
--
-- https://learn.microsoft.com/en-us/graph/api/user-get?view=graph-rest-1.0&tabs=http#request
get :: Text -- ^ user id
    -> AccessToken -> Req User
get uid = MSG.get ["users", uid] mempty

-- | Get information on signed-in user
--
-- Calling the \/me endpoint requires a signed-in user and therefore a delegated permission. Application permissions are not supported when using the \/me endpoint.
--
-- @GET \/me@
--
-- https://learn.microsoft.com/en-us/graph/api/user-get?view=graph-rest-1.0&tabs=http#request-1
getMe :: AccessToken -> Req User
getMe = MSG.get ["me"] mempty
