{-# LANGUAGE LambdaCase #-}
{-# language DeriveGeneric, GeneralizedNewtypeDeriving, DerivingStrategies #-}
{-# LANGUAGE QuasiQuotes, RecordWildCards #-}
{-# language OverloadedStrings #-}
{-# language DataKinds, TypeFamilies, TypeApplications #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# options_ghc -Wno-ambiguous-fields #-}
-- | Settings for using Azure Active Directory as OAuth identity provider
--
-- Both @Auth Code Grant@ (i.e. with browser client interaction) and @Client Credentials Grant@ authentication flows are supported. The former is useful when a user needs to login and delegate some permissions to the application (i.e. accessing personal data), whereas the second is for server processes and automation accounts.
--
-- Azure Bot Framework is supported since v 0.4
module Network.OAuth2.Provider.AzureAD (
    AzureAD
    , AzureBotFramework
    -- * Environment variables
    , envClientId
    , envClientSecret
    , envTenantId
    -- * Client Credentials auth flow
    , azureADApp
    , azureBotFrameworkADApp
    -- * Auth Code Grant auth flow
    , OAuthCfg(..)
    , AzureADUser
    , azureOAuthADApp
    -- * Exceptions
    , AzureADException(..)
    ) where

-- import Data.String (IsString(..))
-- import GHC.Generics

import Control.Monad.IO.Class (MonadIO(..))
import Control.Exception (Exception(..))
import System.Environment (lookupEnv)

-- aeson
import Data.Aeson
-- containers
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
-- hoauth2
import Network.OAuth.OAuth2 (ClientAuthenticationMethod(..), authGetJSON)
import Network.OAuth2.Experiment (IdpApplication(..), Idp(..), IdpUserInfo, GrantTypeFlow(..), ClientId(..), ClientSecret(..), Scope, AuthorizeState)
-- text
import qualified Data.Text as T (Text)
import qualified Data.Text.Lazy as TL (Text, pack)
-- unliftio
import UnliftIO.Exception (throwIO, Typeable)
-- uri-bytestring
import URI.ByteString (URI)
import URI.ByteString.QQ (uri)


data AzureAD = AzureAD deriving (Eq, Show)

-- | @AZURE_CLIENT_ID@
envClientId :: MonadIO f => f ClientId
envClientId = env ClientId "AZURE_CLIENT_ID"
-- | @AZURE_TENANT_ID@
envTenantId :: MonadIO f => f TL.Text
envTenantId = env id "AZURE_TENANT_ID"
-- | @AZURE_CLIENT_SECRET@
envClientSecret :: MonadIO f => f ClientSecret
envClientSecret = env ClientSecret "AZURE_CLIENT_SECRET"


env :: MonadIO m => (TL.Text -> b) -> String -> m b
env mk e = do
  me <- liftIO $ lookupEnv e
  case me of
    Nothing -> throwIO $ AADNoEnvVar e
    Just x -> pure $ (mk . TL.pack) x

data AzureADException = AADNoEnvVar String deriving (Typeable)
instance Exception AzureADException
instance Show AzureADException where
  show = \case
    AADNoEnvVar e -> unwords ["Env var", e, "not found"]

-- * Client Credentials Grant flow

-- | Azure OAuth application
--
-- NB : scope @offline_access@ is ALWAYS requested
--
-- create app at https://go.microsoft.com/fwlink/?linkid=2083908
--
-- also be aware to find the right client id.
-- see https://stackoverflow.com/a/70670961
--
--
-- Throws 'AzureADException' if @AZURE_CLIENT_ID@ and/or @AZURE_CLIENT_SECRET@ credentials are not found in the environment
azureADApp :: MonadIO m =>
              TL.Text -- ^ application name
           -> [Scope] -- ^ scopes
           -> m (IdpApplication 'ClientCredentials AzureAD)
azureADApp appname scopes = do
  clid <- envClientId
  sec <- envClientSecret
  pure $ defaultAzureADApp{
    idpAppName = appname
    , idpAppClientId = clid
    , idpAppClientSecret = sec
    , idpAppScope = Set.fromList (scopes <> ["offline_access"])
    }

defaultAzureADApp :: IdpApplication 'ClientCredentials AzureAD
defaultAzureADApp =
  ClientCredentialsIDPAppConfig
    { idpAppClientId = ""
    , idpAppClientSecret = ""
    , idpAppScope = Set.fromList ["offline_access"] -- https://learn.microsoft.com/EN-US/azure/active-directory/develop/scopes-oidc#openid-connect-scopes
    , idpAppTokenRequestExtraParams = Map.empty
    , idpAppName = "default-azure-app" --
    , idp = defaultAzureADIdp
    }

-- | Initialize an Client Credentials token exchange application for the Bot Framework
--
--
-- Throws 'AzureADException' if @AZURE_CLIENT_ID@ and/or @AZURE_CLIENT_SECRET@ credentials are not found in the environment
azureBotFrameworkADApp :: MonadIO m =>
                          TL.Text -- ^ app name
                       -> m (IdpApplication 'ClientCredentials AzureBotFramework)
azureBotFrameworkADApp appname = do
    clid <- envClientId
    sec <- envClientSecret
    pure $ ClientCredentialsIDPAppConfig {idpAppClientId = clid,
                                          idpAppClientSecret = sec,
                                          idpAppName = appname,
                                          idpAppScope = Set.fromList ["https://api.botframework.com/.default"],
                                          idpAppTokenRequestExtraParams = mempty,
                                          idp = defaultAzureBotFrameworkIdp
                                         }


data AzureBotFramework = AzureBotFramework deriving (Eq, Show)

defaultAzureBotFrameworkIdp :: Idp AzureBotFramework
defaultAzureBotFrameworkIdp = Idp {
  idpFetchUserInfo = authGetJSON @(IdpUserInfo AzureBotFramework)
  , idpTokenEndpoint = [uri|https://login.microsoftonline.com/botframework.com/oauth2/v2.0/token|]
  , idpUserInfoEndpoint = error $ unwords ["Azure Bot Framework Idp:", "OAuth user info endpoint is not defined"]
  , idpAuthorizeEndpoint = error $ unwords ["Azure Bot Framework Idp:", "OAuth authorize endpoint is not defined"]
                                  }







-- * Authorization Code Grant flow

type instance IdpUserInfo AzureAD = AzureADUser

-- | Configuration object of the OAuth2 application
data OAuthCfg = OAuthCfg {
  oacAppName :: TL.Text -- ^ application name
  , oacScopes :: [Scope]  -- ^ OAuth2 and OIDC scopes
  , oacAuthState :: AuthorizeState -- ^ OAuth2 'state' (a random string, https://www.rfc-editor.org/rfc/rfc6749#section-10.12 )
  , oacRedirectURI :: URI -- ^ OAuth2 redirect URI
                         }

-- | Azure OAuth application (i.e. with user consent screen)
--
-- NB : scopes @openid@ and @offline_access@ are ALWAYS requested since the library assumes we have access to refresh tokens and ID tokens
--
-- Reference on Microsoft Graph permissions : https://learn.microsoft.com/en-us/graph/permissions-reference
--
-- create app at https://go.microsoft.com/fwlink/?linkid=2083908
--
-- also be aware to find the right client id.
-- see https://stackoverflow.com/a/70670961
--
--
-- Throws 'AzureADException' if @AZURE_CLIENT_ID@ and/or @AZURE_CLIENT_SECRET@ credentials are not found in the environment
azureOAuthADApp :: MonadIO m =>
                   OAuthCfg -- ^ OAuth configuration
                -> m (IdpApplication 'AuthorizationCode AzureAD)
azureOAuthADApp (OAuthCfg appname scopes authstate reduri) = do
  clid <- envClientId
  sec <- envClientSecret
  pure $ defaultAzureOAuthADApp{
    idpAppName = appname
    , idpAppClientId = clid
    , idpAppClientSecret = sec
    , idpAppScope = Set.fromList (scopes <> ["openid", "offline_access"])
    , idpAppAuthorizeState = authstate
    , idpAppRedirectUri = reduri
    }

defaultAzureOAuthADApp :: IdpApplication 'AuthorizationCode AzureAD
defaultAzureOAuthADApp =
  AuthorizationCodeIdpApplication
    { idpAppClientId = ""
    , idpAppClientSecret = ""
    , idpAppScope = Set.fromList ["openid", "offline_access", "profile", "email"] -- https://learn.microsoft.com/EN-US/azure/active-directory/develop/scopes-oidc#openid-connect-scopes
    , idpAppAuthorizeState = "CHANGE_ME" -- https://stackoverflow.com/questions/26132066/what-is-the-purpose-of-the-state-parameter-in-oauth-authorization-request
    , idpAppAuthorizeExtraParams = Map.empty
    , idpAppRedirectUri = [uri|http://localhost|] --
    , idpAppName = "" --
    , idpAppTokenRequestAuthenticationMethod = ClientSecretBasic
    , idp = defaultAzureADIdp
    }

-- | https://login.microsoftonline.com/common/v2.0/.well-known/openid-configuration
defaultAzureADIdp :: Idp AzureAD
defaultAzureADIdp =
  Idp
    { idpFetchUserInfo = authGetJSON @(IdpUserInfo AzureAD)
    , idpUserInfoEndpoint = [uri|https://graph.microsoft.com/oidc/userinfo|]
    , idpAuthorizeEndpoint = [uri|https://login.microsoftonline.com/common/oauth2/v2.0/authorize|]
    , idpTokenEndpoint = [uri|https://login.microsoftonline.com/common/oauth2/v2.0/token|]
    }



-- | https://learn.microsoft.com/en-us/azure/active-directory/develop/userinfo
data AzureADUser = AzureADUser
  { sub :: T.Text
  , email :: Maybe T.Text -- requires the “email” OIDC scope.
  , familyName :: Maybe T.Text -- all names require the “profile” OIDC scope.
  , givenName :: Maybe T.Text
  , name :: Maybe T.Text
  }
  deriving (Eq, Ord, Show)

instance FromJSON AzureADUser where
  parseJSON = withObject "AzureADUser" $ \o -> AzureADUser <$>
    o .: "sub" <*>
    o .:? "email" <*>
    o .:? "family_name" <*>
    o .:? "given_name" <*>
    o .:? "name"

--   parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = camelTo2 '_'}

