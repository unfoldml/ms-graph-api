-- | Functions for implementing Azure AD-based authentication
--
-- Both @Auth Code Grant@ (i.e. with a user involved in the autorization loop) and @Client Credentials Grant@ (i.e. app only) authentication flows are supported. The former is useful when a user needs to login and delegate some permissions to the application (i.e. accessing personal data), whereas the second is for server processes and automation accounts.
module MSAuth (
  -- * A Client Credentials flow (server-to-server)
  Token
  , tokenUpdateLoop
  , expireToken
  , readToken
  -- ** Default Azure Credential
  , defaultAzureCredential
  -- * B Auth code grant flow (interactive)
  -- ** OAuth endpoints
  , loginEndpoint
  , replyEndpoint
  -- ** In-memory user session
  , Tokens
  , newTokens
  , UserSub
  , lookupUser
  , expireUser
  , tokensToList
  -- * Scotty misc
  -- ** Azure App Service
  , withAADUser
  , Scotty
  , Action
  -- * OAuth types
  , OAuth2Token(..), AccessToken(..), ExchangeToken(..), RefreshToken(..), OAuth2Error(..), IdToken(..)
  ) where

import Network.OAuth2.Session

import Network.OAuth.OAuth2.Internal (OAuth2Token(..), AccessToken(..), ExchangeToken(..), RefreshToken(..), OAuth2Error(..), IdToken(..))
