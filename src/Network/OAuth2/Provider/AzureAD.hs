{-# language DeriveGeneric, GeneralizedNewtypeDeriving, DerivingStrategies #-}
{-# LANGUAGE QuasiQuotes, RecordWildCards #-}
{-# language OverloadedStrings #-}
{-# language DataKinds, TypeFamilies, TypeApplications #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# options_ghc -Wno-ambiguous-fields #-}
module Network.OAuth2.Provider.AzureAD where

-- import Data.String (IsString(..))
import GHC.Generics

-- aeson
import Data.Aeson
-- containers
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
-- hoauth2
import Network.OAuth.OAuth2 -- (authGetJSON, ClientSecretBasic)
import Network.OAuth2.Experiment (IdpApplication(..), Idp(..), IdpUserInfo, GrantTypeFlow(..), ClientId(..), ClientSecret, Scope, AuthorizeState)
-- text
import Data.Text.Lazy (Text)
-- uri-bytestring
import URI.ByteString (URI)
import URI.ByteString.QQ (uri)


data AzureAD = AzureAD deriving (Eq, Show)

type instance IdpUserInfo AzureAD = AzureADUser

data OAuthCfg = OAuthCfg {
  oacAppName :: Text -- ^ application name
  , oacClientId :: ClientId -- ^ app client ID : see https://stackoverflow.com/a/70670961
  , oacClientSecret :: ClientSecret -- ^ app client secret "
  , oacScopes :: [Scope]  -- ^ OAuth2 and OIDC scopes
  , oacAuthState :: AuthorizeState -- ^ OAuth2 'state' (a random string, https://www.rfc-editor.org/rfc/rfc6749#section-10.12 )
  , oacRedirectURI :: URI -- ^ OAuth2 redirect URI
                         }

azureADApp :: OAuthCfg
           -> IdpApplication 'AuthorizationCode AzureAD
azureADApp (OAuthCfg appname clid sec scopes authstate reduri) = defaultAzureADApp{
  idpAppName = appname
  , idpAppClientId = clid
  , idpAppClientSecret = sec
  , idpAppScope = Set.fromList scopes
  , idpAppAuthorizeState = authstate
  , idpAppRedirectUri = reduri
  }

-- create app at https://go.microsoft.com/fwlink/?linkid=2083908
--
-- also be aware to find the right client id.
-- see https://stackoverflow.com/a/70670961
defaultAzureADApp :: IdpApplication 'AuthorizationCode AzureAD
defaultAzureADApp =
  AuthorizationCodeIdpApplication
    { idpAppClientId = ""
    , idpAppClientSecret = ""
    , idpAppScope = Set.fromList ["openid", "profile", "email"]
    , idpAppAuthorizeState = "CHANGE_ME" -- https://stackoverflow.com/questions/26132066/what-is-the-purpose-of-the-state-parameter-in-oauth-authorization-request
    , idpAppAuthorizeExtraParams = Map.empty
    , idpAppRedirectUri = [uri|http://localhost|] --
    , idpAppName = "default-azure-app" --
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
  { sub :: Text
  , email :: Text
  , familyName :: Text
  , givenName :: Text
  , name :: Text
  }
  deriving (Show, Generic)

instance FromJSON AzureADUser where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = camelTo2 '_'}

