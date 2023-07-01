-- | Functions for implementing Azure AD-based authentication
--
-- Both @Auth Code Grant@ (i.e. with browser client interaction) and @App-only@ (i.e. Client Credentials) authentication flows are supported. The former is useful when a user needs to login and delegate some permissions to the application (i.e. accessing personal data), whereas the second is for server processes and automation accounts.
module MSAuth (
  applyDotEnv
  -- * A App-only flow (server-to-server)
  , Token
  , newNoToken
  , expireToken
  , readToken
  , fetchUpdateToken
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
              ) where

import Network.OAuth2.Session
import DotEnv (applyDotEnv)
