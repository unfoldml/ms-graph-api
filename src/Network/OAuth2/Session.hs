{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# language DeriveGeneric, GeneralizedNewtypeDeriving, DerivingStrategies, DeriveDataTypeable  #-}
{-# language OverloadedStrings #-}
{-# options_ghc -Wno-unused-imports #-}
-- | OAuth user session
module Network.OAuth2.Session (
  -- * Azure App Service
  withAADUser
  -- * OAuth2 endpoints
  , loginEndpoint
  , replyEndpoint
  -- * In-memory user session
  , Tokens
  , UserSub
  , lookupUser
  , expireUser
  -- * Scotty misc
  , Scotty
  , Action
                              ) where

import Control.Exception (Exception(..), SomeException(..))
import Control.Monad.IO.Class (MonadIO(..))
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (fromMaybe)
import Data.String (IsString(..))
import Data.Typeable (Typeable)
import GHC.Exception (SomeException)

-- aeson
import Data.Aeson
-- bytestring
import qualified Data.ByteString.Lazy.Char8 as BSL
-- containers
import qualified Data.Map as M (Map, insert, lookup, alter)
-- -- heaps
-- import qualified Data.Heap as H (Heap, empty, null, size, insert, viewMin, deleteMin, Entry(..), )
-- hoauth2
import Network.OAuth.OAuth2 (OAuth2Token(..), AccessToken(..), ExchangeToken(..), RefreshToken(..), OAuth2Error, IdToken(..))
import Network.OAuth2.Experiment (IdpUserInfo, conduitUserInfoRequest, mkAuthorizeRequest, conduitTokenRequest, conduitRefreshTokenRequest, HasRefreshTokenRequest(..), WithExchangeToken, IdpApplication(..), GrantTypeFlow(..))
import Network.OAuth.OAuth2.TokenRequest (Errors)
-- http-client
import Network.HTTP.Client (Manager)
-- http-types
import Network.HTTP.Types (status302, status400, status401)
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
import Validation (Validation, failure, validationToEither)

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


-- * OAuth flow

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
  setHeader "Location" (mkAuthorizeRequest idpApp) -- $ azureADApp oacfg)
  status status302

-- | The identity provider redirects the client to the 'reply' endpoint as part of the OAuth flow : https://learn.microsoft.com/en-us/graph/auth-v2-user?view=graph-rest-1.0&tabs=http#authorization-response
--
-- see 'azureADApp'
replyEndpoint :: MonadIO m =>
                 IdpApplication 'AuthorizationCode AzureAD
              -> Tokens UserSub OAuth2Token
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
           _ <- fetchUpdateToken ts idpApp mgr etoken
           pure ()
         Nothing -> throwE OASEExchangeTokenNotFound -- $ T.pack $ unwords ["cannot decode token"]

--

-- oauth2ErrorToText :: Show a => a -> T.Text
-- oauth2ErrorToText e = T.pack $ "Unable to fetch access token. Details : " ++ show e

-- bslToText :: BSL.ByteString -> T.Text
-- bslToText = T.pack . BSL.unpack


-- | 1) the ExchangeToken arrives with the redirect once the user has approved the scopes in the browser
-- https://learn.microsoft.com/en-us/graph/auth-v2-user?view=graph-rest-1.0&tabs=http#authorization-response
fetchUpdateToken :: MonadUnliftIO m =>
                    Tokens UserSub OAuth2Token
                 -> IdpApplication 'AuthorizationCode AzureAD
                 -> Manager
                 -> ExchangeToken -- ^ also called 'code'. Expires in 10 minutes
                 -> ExceptT OAuthSessionError m OAuth2Token -- IO (Either T.Text OAuth2Token)
fetchUpdateToken ts idpApp mgr etoken = ExceptT $ do
  tokenResp <- runExceptT $ conduitTokenRequest idpApp mgr etoken -- OAuth2 token
  case tokenResp of
    Right oat -> case idToken oat of
      Nothing -> pure $ Left OASENoOpenID
      Just idt -> do
        idtClaimsE <- decValidIdToken idt -- decode and validate ID token
        case idtClaimsE of
          Right uid -> do
            _ <- refreshLoop ts idpApp mgr uid oat -- fork a thread and start refresh loop for this user
            pure $ Right oat
          Left es -> pure $ Left (OASEJWTException es) -- $ T.pack (show e) -- ^ id token validation failed
    Left es -> pure $ Left (OASEOAuth2Errors es)

refreshLoop :: (MonadUnliftIO m, Ord uid, HasRefreshTokenRequest a) =>
               Tokens uid OAuth2Token
            -> IdpApplication a i
            -> Manager
            -> uid
            -> OAuth2Token
            -> m ThreadId
refreshLoop ts idpApp mgr uid oaToken = forkFinally (act oaToken) cleanup
  where
    cleanup = \case
      Left _ -> do
        expireUser ts uid -- auth error(s), remove user from memory
      Right _ -> pure ()
    act oat = do
      ein <- updateToken ts uid oat -- replace new token in memory
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
    OASEJWTException jwtes -> unwords ["JWT error(s):", show jwtes]
    OASENoOpenID -> unwords ["No ID token found. Ensure 'openid' scope appears in token request"]


updateToken :: (MonadIO m, Ord uid) =>
               Tokens uid OAuth2Token
            -> uid -- ^ user id
            -> OAuth2Token -- ^ new token
            -> m NominalDiffTime -- ^ token expires in
updateToken ts uid oat = do
  let
    ein = fromIntegral $ fromMaybe 3600 (expiresIn oat) -- expires in [sec]
  atomically $ do
    thp <- readTVar ts
    let
      m' = M.insert uid oat (thUsersMap thp)
    writeTVar ts (TokensData m')
    pure ein

expireUser :: (MonadIO m, Ord uid) =>
              Tokens uid t
           -> uid -- ^ user identifier e.g. @sub@
           -> m ()
expireUser ts uid =
  atomically $ modifyTVar ts $ \td -> td{ thUsersMap = M.alter (const Nothing) uid (thUsersMap td)}

lookupUser :: (MonadIO m, Ord uid) =>
              Tokens uid t
           -> uid -- ^ user identifier e.g. @sub@
           -> m (Maybe t)
lookupUser ts uid = atomically $ do
  thp <- readTVar ts
  pure $ M.lookup uid (thUsersMap thp)

-- | transactional token store
type Tokens uid t = TVar (TokensData uid t)
data TokensData uid t = TokensData {
  thUsersMap :: M.Map uid t
                             }


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
