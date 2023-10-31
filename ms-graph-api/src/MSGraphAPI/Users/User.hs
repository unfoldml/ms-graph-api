-- | Users.User
module MSGraphAPI.Users.User (
  get
  , getMe
  -- * types
  , User(..)) where

import GHC.Generics (Generic(..))

-- aeson
import qualified Data.Aeson as A (ToJSON(..), FromJSON(..), genericParseJSON)
-- ms-auth
import MSAuth (AccessToken(..))
-- req
import Network.HTTP.Req (Req)
-- text
import Data.Text (Text)

import qualified MSGraphAPI.Internal.Common as MSG (get, aesonOptions)

-- | Representation of a user in the MS Graph API
--
-- https://learn.microsoft.com/en-us/graph/api/resources/users?view=graph-rest-1.0
data User = User {
  uId :: Text
  , uUserPrincipalName :: Text
  , uDisplayName :: Text
                 } deriving (Eq, Ord, Show, Generic)
instance A.FromJSON User where
  parseJSON = A.genericParseJSON (MSG.aesonOptions "u")
instance A.ToJSON User


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
