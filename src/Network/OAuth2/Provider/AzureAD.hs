{-# language DeriveGeneric, GeneralizedNewtypeDeriving, DerivingStrategies #-}
{-# LANGUAGE QuasiQuotes, RecordWildCards #-}
{-# language OverloadedStrings #-}
{-# language DataKinds, TypeFamilies, TypeApplications #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# options_ghc -Wno-ambiguous-fields #-}
module Network.OAuth2.Provider.AzureAD (
  -- * OAuth2 configuration
  OAuthCfg(..)
  , AzureAD
  , AzureADUser
  , azureADApp) where

-- import Data.String (IsString(..))
-- import GHC.Generics

-- aeson
import Data.Aeson
-- containers
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
-- hoauth2
import Network.OAuth.OAuth2 -- (authGetJSON, ClientSecretBasic)
import Network.OAuth2.Experiment (IdpApplication(..), Idp(..), IdpUserInfo, GrantTypeFlow(..), ClientId(..), ClientSecret, Scope, AuthorizeState)
-- text
import qualified Data.Text as T (Text)
import qualified Data.Text.Lazy as TL (Text)
-- uri-bytestring
import URI.ByteString (URI)
import URI.ByteString.QQ (uri)


data AzureAD = AzureAD deriving (Eq, Show)

type instance IdpUserInfo AzureAD = AzureADUser

data OAuthCfg = OAuthCfg {
  oacAppName :: TL.Text -- ^ application name
  , oacClientId :: ClientId -- ^ app client ID : see https://stackoverflow.com/a/70670961
  , oacClientSecret :: ClientSecret -- ^ app client secret "
  , oacScopes :: [Scope]  -- ^ OAuth2 and OIDC scopes
  , oacAuthState :: AuthorizeState -- ^ OAuth2 'state' (a random string, https://www.rfc-editor.org/rfc/rfc6749#section-10.12 )
  , oacRedirectURI :: URI -- ^ OAuth2 redirect URI
                         }

-- | NB : OIDC scopes @openid@ and @offline_access@ are ALWAYS requested since the library assumes we have access to refresh tokens and ID tokens
azureADApp :: OAuthCfg -- ^ OAuth configuration
           -> IdpApplication 'AuthorizationCode AzureAD
azureADApp (OAuthCfg appname clid sec scopes authstate reduri) = defaultAzureADApp{
  idpAppName = appname
  , idpAppClientId = clid
  , idpAppClientSecret = sec
  , idpAppScope = Set.fromList (scopes <> ["openid", "offline_access"])
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
    , idpAppScope = Set.fromList ["openid", "offline_access", "profile", "email"] -- https://learn.microsoft.com/EN-US/azure/active-directory/develop/scopes-oidc#openid-connect-scopes
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

