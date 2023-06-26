{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds, TypeFamilies #-}
{-# language DeriveGeneric, GeneralizedNewtypeDeriving, DerivingStrategies, DeriveDataTypeable  #-}
{-# language OverloadedStrings #-}
{-# options_ghc -Wno-unused-imports #-}
-- | MS Identity user session based on OAuth tokens
--
-- The library supports the following authentication scenarios :
--
-- * [Client Credentials](https://learn.microsoft.com/en-us/azure/active-directory/develop/v2-oauth2-client-creds-grant-flow) (server/server or automation accounts)
--
-- * [Authorization Code](https://learn.microsoft.com/en-us/azure/active-directory/develop/v2-oauth2-auth-code-flow) (with human users being prompted to delegate some access rights to the app)
--
-- and provides functions to keep tokens up to date in the background.
module Network.OAuth2.Session (
  -- * A App-only flow (server-to-server)
  Token
  , newNoToken
  , expireToken
  , readToken
  , fetchUpdateToken
  -- ** Default Azure Credential
  , defaultAzureCredential
  -- * B Auth code grant flow (with user in the loop)
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

import Control.Applicative (Alternative(..))
import Control.Exception (Exception(..), SomeException(..))
import Control.Monad.IO.Class (MonadIO(..))
import Data.Functor (void)
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (fromMaybe)
import Data.String (IsString(..))
import Data.Typeable (Typeable)
import GHC.Exception (SomeException)
import System.Environment (lookupEnv)

-- aeson
import qualified Data.Aeson as A (FromJSON(..), eitherDecode)
-- bytestring
import qualified Data.ByteString.Char8 as BS (pack)
import qualified Data.ByteString.Lazy.Char8 as BSL
-- containers
import qualified Data.Map as M (Map, insert, lookup, alter, toList)
-- -- heaps
-- import qualified Data.Heap as H (Heap, empty, null, size, insert, viewMin, deleteMin, Entry(..), )
-- hoauth2
import Network.OAuth.OAuth2 (OAuth2Token(..), AccessToken(..), ExchangeToken(..), RefreshToken(..), OAuth2Error(..), IdToken(..))
import Network.OAuth2.Experiment (IdpUserInfo, conduitUserInfoRequest, mkAuthorizeRequest, conduitTokenRequest, conduitRefreshTokenRequest, HasRefreshTokenRequest(..), WithExchangeToken, IdpApplication(..), GrantTypeFlow(..))
import Network.OAuth.OAuth2.TokenRequest (Errors)
-- http-client
import Network.HTTP.Client (Manager, parseRequest, requestHeaders, httpLbs, responseBody, responseStatus)
-- http-types
import Network.HTTP.Types (status302, status400, status401, statusCode)
import Network.HTTP.Types.Header (RequestHeaders, Header)
-- scotty
import Web.Scotty (scotty, RoutePattern)
import Web.Scotty.Trans (scottyT, ActionT, ScottyT, get, raise, redirect, params, header, setHeader, status, text)
-- text
import qualified Data.Text as T (Text, pack, unwords)
import qualified Data.Text.Lazy as TL (Text, pack, unpack, toStrict, takeWhile, fromStrict)
-- time
import Data.Time (UTCTime(..), getCurrentTime, fromGregorian, diffUTCTime, addUTCTime, Day, NominalDiffTime)
import Data.Time.Format (FormatTime, formatTime, iso8601DateFormat, defaultTimeLocale)
-- transformers
import Control.Monad.Trans.Except (ExceptT(..), withExceptT, runExceptT, throwE)
-- unliftio
import UnliftIO (MonadUnliftIO(..))
import UnliftIO.Concurrent (ThreadId, forkFinally, threadDelay)
import UnliftIO.Exception (throwIO)
import UnliftIO.STM (STM, TVar, atomically, newTVarIO, readTVar, writeTVar, modifyTVar)
-- uri-bytestring
import URI.ByteString (URI)
-- validation-selective
import Validation.Micro (Validation, failure, validationToEither)

import Network.OAuth2.Provider.AzureAD (OAuthCfg, azureADApp, AzureAD)
import Network.OAuth2.JWT (jwtClaims, UserSub(..), userSub, ApiAudience, apiAudience, decValidSub, decValidExp, decValidNbf, JWTException(..))

type Action = ActionT TL.Text
type Scotty = ScottyT TL.Text

-- * Azure App Service adds headers into each request, which the backend can access to identify the user
--
-- https://learn.microsoft.com/en-us/azure/app-service/configure-authentication-user-identities#access-user-claims-in-app-code

-- | The JWT identity token from the @X-MS-TOKEN-AAD-ID-TOKEN@ header injected by App Service can be decoded for its claims e.g. @sub@ (which is unique for each user for a given app)
--
-- https://bogdan.bynapse.com/azure/the-app-service-token-store-was-added-to-app-service-authentication-authorization-and-it-is-a-repository-of-oauth-tokens-associated-with-your-app-users-when-a-user-logs-into-your-app-via-an-iden/
--
-- https://stackoverflow.com/questions/46757665/authentication-for-azure-functions/
aadHeaderIdToken :: (MonadIO m) =>
                    (UserSub -> Action m ()) -- ^ look up the UserSub's token, do stuff with it
                 -> Action m ()
aadHeaderIdToken act = do
  let
    hdrName = "X-MS-TOKEN-AAD-ID-TOKEN"
  mh <- header hdrName
  case mh of
    Nothing -> do
      text $ TL.pack $ unwords ["header", TL.unpack hdrName, "not found in request"]
      status status400
    Just h -> do
      let
        idt = IdToken $ TL.toStrict h
      ide <- decValidIdToken idt
      case ide of
        Right usub -> act usub
        Left e -> do
          text $ TL.pack $ unwords ["AAD header ID token validation exception:", show e]
          status status401

-- | Decode the App Service ID token header @X-MS-TOKEN-AAD-ID-TOKEN@, look its user up in the local token store, supply token @t@ to continuation. If the user @sub@ cannot be found in the token store the browser is redirected to the login URI.
--
-- Special case of 'aadHeaderIdToken'
withAADUser :: MonadIO m =>
               Tokens UserSub t
            -> TL.Text -- ^ login URI
            -> (t -> Action m ()) -- ^ call MSGraph APIs with token @t@, etc.
            -> Action m ()
withAADUser ts loginURI act = aadHeaderIdToken $ \usub -> do
  mt <- lookupUser ts usub
  case mt of
    Just t -> act t
    _ -> do
      liftIO $ putStrLn $ unwords ["User", show usub, "not authenticated. Redirecting to login:", TL.unpack loginURI]
      redirect loginURI





-- * App-only authorization scenarios (i.e via automation accounts. Human users not involved)



-- | App has (at most) one token at a time
type Token t = TVar (Maybe t)

newNoToken :: MonadIO m => m (Token t)
newNoToken = newTVarIO Nothing
expireToken :: MonadIO m => Token t -> m ()
expireToken ts = atomically $ modifyTVar ts (const Nothing)
readToken :: MonadIO m => Token t -> m (Maybe t)
readToken ts = atomically $ readTVar ts

fetchUpdateTokenWith :: MonadIO m =>
                        (t1 -> t2 -> ExceptT [String] IO OAuth2Token)
                     -> t1 -> Token OAuth2Token -> t2 -> m ()
fetchUpdateTokenWith f idpApp ts mgr = liftIO $ void $ forkFinally loop cleanup
  where
    cleanup = \case
      Left e -> throwIO e
      Right _ -> pure ()
    loop = do
      tokenResp <- runExceptT $ f idpApp mgr -- allows different mechanisms of fetching OAuth2 token
      case tokenResp of
        Left es -> throwIO (OASEDefaultAzureCredentialsE es)
        Right oat -> do
          ein <- updateToken ts oat
          let
            dtSecs = (round ein - 30) -- 30 seconds before expiry
          threadDelay (dtSecs * 1000000) -- pause thread
          loop

-- | DefaultUserCredential mechanism as in the Python SDK https://pypi.org/project/azure-identity/
defaultAzureCredential :: MonadIO m =>
                          String
                       -> String
                       -> IdpApplication 'ClientCredentials AzureAD
                       -> Token OAuth2Token
                       -> Manager
                       -> m ()
defaultAzureCredential clid resuri = fetchUpdateTokenWith (
  \idp mgr ->
    tokenRequestNoExchange idp mgr <|>
    managedIdentity mgr clid resuri
    )

tokenRequestNoExchange :: (MonadIO m) =>
                          IdpApplication 'ClientCredentials AzureAD
                       -> Manager
                       -> ExceptT [String] m OAuth2Token
tokenRequestNoExchange idp mgr = withExceptT (pure . show) (conduitTokenRequest idp mgr)

-- | Fetch an OAuth token and keep it updated. Should be called as a first thing in the app
--
-- NB : forks a thread in the background
--
-- https://learn.microsoft.com/en-us/azure/active-directory/develop/v2-oauth2-client-creds-grant-flow
fetchUpdateToken :: MonadIO m =>
                    IdpApplication 'ClientCredentials AzureAD
                 -> Token OAuth2Token -- ^ token TVar
                 -> Manager
                 -> m ()
fetchUpdateToken idpApp ts mgr = liftIO $ void $ forkFinally loop cleanup
  where
    cleanup = \case
      Left e -> throwIO e
      Right _ -> pure ()
    loop = do
      tokenResp <- runExceptT $ conduitTokenRequest idpApp mgr -- OAuth2 token
      case tokenResp of
        Left es -> throwIO (OASEOAuth2Errors es)
        Right oat -> do
          ein <- updateToken ts oat
          let
            dtSecs = (round ein - 30) -- 30 seconds before expiry
          threadDelay (dtSecs * 1000000) -- pause thread
          loop

updateToken :: (MonadIO m) =>
               Token OAuth2Token -> OAuth2Token -> m NominalDiffTime
updateToken ts oat = do
  let
    ein = fromIntegral $ fromMaybe 3600 (expiresIn oat) -- expires in [sec]
  atomically $ do
    writeTVar ts (Just oat)
  pure ein



-- * Managed identity

-- | With its managed identity, an app can obtain tokens for Azure resources that are protected by Azure Active Directory, such as Azure SQL Database, Azure Key Vault, and Azure Storage. These tokens represent the application accessing the resource, and not any specific user of the application.
--
-- App Service and Azure Functions provide an internally accessible REST endpoint for token retrieval. 
--
-- https://learn.microsoft.com/en-us/azure/app-service/overview-managed-identity?tabs=portal%2Chttp#rest-endpoint-reference
managedIdentity :: Manager
                -> String -- ^ client ID
                -> String -- ^ Azure resource URI
                -> ExceptT [String] IO OAuth2Token
managedIdentity mgr clid resUri = ExceptT $ do
  mih <- lookupEnv "IDENTITY_ENDPOINT"
  mie <- lookupEnv "IDENTITY_HEADER"
  case (,) <$> mih <*> mie of
    Just (idEndpoint, ih) -> do
      let
        apiVer = "2019-08-01"
        xIdentityHeader = ih
      r <- parseRequest $ mconcat [idEndpoint, "?", kvs [("resource", resUri), ("api-version", apiVer), ("client_id", clid)]]
      let
        r' = r {
          requestHeaders = [
              ("X-IDENTITY-HEADER", BS.pack xIdentityHeader)
                           ]
               }
      res <- httpLbs r' mgr
      let
        rstat = responseStatus res
        sci = statusCode rstat
      if 200 <= sci && sci < 300
        then
        case A.eitherDecode (responseBody res) of
          Right oat -> pure $ Right oat
          Left e -> pure $ lefts $ unwords ["managedIdentity: Cannot decode OAuth token:", e]
        else
          pure $ lefts $ unwords ["managedIdentity: status code exception:", show rstat]
    _ -> pure $
          lefts $ unwords ["managedIdentity: Cannot find either IDENTITY_ENDPOINT or IDENTITY_HEADER env vars."]
lefts :: a -> Either [a] b
lefts s = Left [s]

kvs :: [(String, String)] -> String
kvs = foldr ins mempty
  where
    ins (k, v) acc = acc <> ("&" <> k <> "=" <> v)




-- * Auth code grant flow (i.e. human user involved)

-- | Login endpoint
--
-- see 'azureADApp'
loginEndpoint :: (MonadIO m) =>
                 IdpApplication 'AuthorizationCode AzureAD
              -> RoutePattern -- ^ e.g. @"/login"@
              -> Scotty m ()
loginEndpoint idpApp path = get path (loginH idpApp)

-- | login endpoint handler
loginH :: Monad m =>
          IdpApplication 'AuthorizationCode AzureAD
       -> Action m ()
loginH idpApp = do
  setHeader "Location" (mkAuthorizeRequest idpApp) -- redirect to OAuth consent screen
  status status302

-- | The identity provider redirects the client to the 'reply' endpoint as part of the OAuth flow : https://learn.microsoft.com/en-us/graph/auth-v2-user?view=graph-rest-1.0&tabs=http#authorization-response
--
-- NB : forks a thread per logged in user to keep their tokens up to date
replyEndpoint :: MonadIO m =>
                 IdpApplication 'AuthorizationCode AzureAD
              -> Tokens UserSub OAuth2Token -- ^ token TVar
              -> Manager
              -> RoutePattern -- ^ e.g. @"/oauth\/reply"@
              -> Scotty m ()
replyEndpoint idpApp ts mgr path =
  get path (replyH idpApp ts mgr)

replyH :: MonadIO m =>
          IdpApplication 'AuthorizationCode AzureAD
       -> Tokens UserSub OAuth2Token
       -> Manager
       -> Action m ()
replyH idpApp ts mgr = do
  ps <- params
  excepttToActionM $ do
       case lookup "code" ps of
         Just codeP -> do
           let
             etoken = ExchangeToken $ TL.toStrict codeP
           _ <- fetchUpdateTokenACG ts idpApp mgr etoken
           pure ()
         Nothing -> throwE OASEExchangeTokenNotFound

--

-- oauth2ErrorToText :: Show a => a -> T.Text
-- oauth2ErrorToText e = T.pack $ "Unable to fetch access token. Details : " ++ show e

-- bslToText :: BSL.ByteString -> T.Text
-- bslToText = T.pack . BSL.unpack


-- | 1) the ExchangeToken arrives with the redirect once the user has approved the scopes in the browser
-- https://learn.microsoft.com/en-us/graph/auth-v2-user?view=graph-rest-1.0&tabs=http#authorization-response
fetchUpdateTokenACG :: MonadIO m =>
                         Tokens UserSub OAuth2Token
                      -> IdpApplication 'AuthorizationCode AzureAD
                      -> Manager
                      -> ExchangeToken -- ^ also called 'code'. Expires in 10 minutes
                      -> ExceptT OAuthSessionError m OAuth2Token
fetchUpdateTokenACG ts idpApp mgr etoken = ExceptT $ do
  tokenResp <- runExceptT $ conduitTokenRequest idpApp mgr etoken -- OAuth2 token
  case tokenResp of
    Right oat -> case idToken oat of
      Nothing -> pure $ Left OASENoOpenID
      Just idt -> do
        idtClaimsE <- decValidIdToken idt -- decode and validate ID token
        case idtClaimsE of
          Right uid -> do
            _ <- refreshLoopACG ts idpApp mgr uid oat -- fork a thread and start refresh loop for this user
            pure $ Right oat
          Left es -> pure $ Left (OASEJWTException es) -- id token validation failed
    Left es -> pure $ Left (OASEOAuth2Errors es)

-- | 2) fork a thread and start token refresh loop for user @uid@
refreshLoopACG :: (MonadIO m, Ord uid, HasRefreshTokenRequest a) =>
                    Tokens uid OAuth2Token
                 -> IdpApplication a i
                 -> Manager
                 -> uid -- ^ user ID
                 -> OAuth2Token
                 -> m ThreadId
refreshLoopACG ts idpApp mgr uid oaToken = liftIO $ forkFinally (act oaToken) cleanup
  where
    cleanup = \case
      Left _ -> do
        expireUser ts uid -- auth error(s), remove user from memory
      Right _ -> pure ()
    act oat = do
      ein <- upsertToken ts uid oat -- replace new token for user uid in memory
      let
        dtSecs = (round ein - 30) -- 30 seconds before expiry
      threadDelay (dtSecs * 1000000) -- pause thread
      case refreshToken oat of
        Nothing -> do
          expireUser ts uid -- cannot refresh, remove user from memory
          throwIO OASERefreshTokenNotFound -- no refresh token
        Just rt -> do
          eo' <- runExceptT $ conduitRefreshTokenRequest idpApp mgr rt -- get a new OAuth2 token
          case eo' of
            Right oat' -> do
              act oat' -- loop
            Left e -> throwIO (OASEOAuth2Errors e) -- refresh token request failed

data OAuthSessionError = OASERefreshTokenNotFound
                       | OASEExchangeTokenNotFound
                       | OASEOAuth2Errors (OAuth2Error Errors)
                       | OASEDefaultAzureCredentialsE [String]
                       | OASEJWTException (NonEmpty JWTException)
                       | OASENoOpenID
                       deriving (Eq, Typeable)
instance Exception OAuthSessionError
instance Show OAuthSessionError where
  show = \case
    OASERefreshTokenNotFound -> unwords ["Refresh token not found in OAT"]
    OASEExchangeTokenNotFound -> unwords ["Exchange token not found. This shouldn't happen"]
    OASEOAuth2Errors oerrs ->
      unwords ["OAuth2 error(s):", show oerrs]
    OASEDefaultAzureCredentialsE es ->
      unwords ["defaultAzureCredential error(s):", mconcat es]
    OASEJWTException jwtes -> unwords ["JWT error(s):", show jwtes]
    OASENoOpenID -> unwords ["No ID token found. Ensure 'openid' scope appears in token request"]

-- | Insert or update a token in the 'Tokens' object
upsertToken :: (MonadIO m, Ord uid) =>
               Tokens uid OAuth2Token
            -> uid -- ^ user id
            -> OAuth2Token -- ^ new token
            -> m NominalDiffTime -- ^ token expires in
upsertToken ts uid oat = do
  let
    ein = fromIntegral $ fromMaybe 3600 (expiresIn oat) -- expires in [sec]
  atomically $ do
    thp <- readTVar ts
    let
      m' = M.insert uid oat (thUsersMap thp)
    writeTVar ts (TokensData m')
    pure ein

-- | Remove a user, i.e. they will have to authenticate once more
expireUser :: (MonadIO m, Ord uid) =>
              Tokens uid t
           -> uid -- ^ user identifier e.g. @sub@
           -> m ()
expireUser ts uid =
  atomically $ modifyTVar ts $ \td -> td{ thUsersMap = M.alter (const Nothing) uid (thUsersMap td)}

-- | Look up a user identifier and return their current token, if any
lookupUser :: (MonadIO m, Ord uid) =>
              Tokens uid t
           -> uid -- ^ user identifier e.g. @sub@
           -> m (Maybe t)
lookupUser ts uid = atomically $ do
  thp <- readTVar ts
  pure $ M.lookup uid (thUsersMap thp)

-- | return a list representation of the 'Tokens' object
tokensToList :: MonadIO m => Tokens k a -> m [(k, a)]
tokensToList ts = atomically $ do
  (TokensData m) <- readTVar ts
  pure $ M.toList m

-- | Create an empty 'Tokens' object
newTokens :: (MonadIO m, Ord uid) => m (Tokens uid t)
newTokens = newTVarIO (TokensData mempty)

-- | transactional token store
type Tokens uid t = TVar (TokensData uid t)
newtype TokensData uid t = TokensData {
  thUsersMap :: M.Map uid t
  } deriving (Eq, Show)

-- class HasTokens r where
--   hasTokens :: r -> Tokens uid t

-- | Decode and validate ID token
-- https://learn.microsoft.com/en-us/azure/active-directory/develop/userinfo#consider-using-an-id-token-instead
decValidIdToken :: MonadIO m =>
                   IdToken -- ^ appears in the OAuth2Token if scopes include @openid@
                -> m (Either (NonEmpty JWTException) UserSub) -- ^ (sub)
decValidIdToken (IdToken idt) = do
  t <- liftIO getCurrentTime
  let
    ve = validationToEither $
         case jwtClaims idt of
           Just c -> (,,) <$> decValidSub c <*> decValidExp Nothing t c <*> decValidNbf t c
           _ -> failure $ JEMalformedJWT (T.unwords ["cannot decode token string"])
  case ve of
    Right (usub, _, _) -> pure $ Right usub
    Left e -> pure $ Left e



-- | Lift ExceptT to ActionM which is basically the handler Monad in Scotty.
excepttToActionM :: (MonadIO m, Show e) =>
                    ExceptT e IO b -> Action m b
excepttToActionM e = do
  result <- liftIO $ runExceptT e
  either (raise . TL.pack . show) pure result


-- playground

-- atomicallyWithAfter :: MonadUnliftIO m =>
--                        TVar a
--                     -> Int -- ^ delay in microseconds (see 'threadDelay')
--                     -> (a -> a)
--                     -> m ThreadId
-- atomicallyWithAfter tv dt f = forkFinally act (\_ -> pure ())
--   where
--     act = do
--       threadDelay dt
--       atomically $ modifyTVar tv f
