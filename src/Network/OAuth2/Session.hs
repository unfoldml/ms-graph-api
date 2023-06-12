{-# LANGUAGE DataKinds #-}
{-# language DeriveGeneric, GeneralizedNewtypeDeriving, DerivingStrategies #-}
{-# language OverloadedStrings #-}
{-# options_ghc -Wno-unused-imports #-}
module Network.OAuth2.Session where

import Control.Monad.IO.Class (MonadIO(..))
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (fromMaybe)
import Data.String (IsString(..))
import GHC.Exception (SomeException)

-- aeson
import Data.Aeson
-- bytestring
import qualified Data.ByteString.Lazy.Char8 as BSL
-- containers
import qualified Data.Map as M (Map, insert, lookup, alter)
-- heaps
import qualified Data.Heap as H (Heap, empty, null, size, insert, viewMin, deleteMin, Entry(..), )
-- hoauth2
import Network.OAuth.OAuth2 (OAuth2Token(..))
import Network.OAuth2.Experiment (IdpUserInfo, conduitUserInfoRequest, mkAuthorizeRequest, conduitTokenRequest, conduitRefreshTokenRequest)
import Network.OAuth2.Experiment.Types (HasRefreshTokenRequest(..), WithExchangeToken(..), IdpApplication(..), GrantTypeFlow(..))
import Network.OAuth.OAuth2.Internal (AccessToken(..), ExchangeToken(..), RefreshToken(..), OAuth2Error, IdToken(..))
import Network.OAuth.OAuth2.TokenRequest (Errors)
-- http-client
import Network.HTTP.Client (Manager)
-- http-conduit
import Network.HTTP.Conduit (newManager, tlsManagerSettings)
-- http-types
import Network.HTTP.Types (status302)
-- scotty
import Web.Scotty (scotty, RoutePattern)
import Web.Scotty.Trans (scottyT, ActionT, ScottyT, get, raise, params, setHeader, status)
-- stm
-- import Control.Monad.STM (STM)
-- import Control.Concurrent.STM.TVar (TVar)
-- text
import qualified Data.Text as T (Text, pack, unwords)
import qualified Data.Text.Lazy as TL (Text, pack, toStrict, takeWhile, fromStrict)
-- time
import Data.Time (UTCTime(..), getCurrentTime, fromGregorian, diffUTCTime, addUTCTime, Day, NominalDiffTime)
import Data.Time.Format (FormatTime, formatTime, iso8601DateFormat, defaultTimeLocale)
-- transformers
import Control.Monad.Trans.Except (ExceptT(..), withExceptT, runExceptT, throwE)
-- unliftio
import UnliftIO (MonadUnliftIO(..))
import UnliftIO.Concurrent (ThreadId, forkFinally, threadDelay)
import UnliftIO.STM (STM, TVar, atomically, newTVarIO, readTVar, writeTVar, modifyTVar)
-- uri-bytestring
import URI.ByteString (URI)
-- validation-selective
import Validation (Validation, failure, validationToEither)

import Network.OAuth2.Provider.AzureAD (OAuthCfg, azureADApp, AzureAD(..), AzureADUser(..))
import Network.OAuth2.JWT (jwtClaims, UserSub(..), userSub, ApiAudience, apiAudience, decValidSub, decValidExp, decValidNbf, AuthException(..))

type Action = ActionT TL.Text
type Scotty = ScottyT TL.Text


loginEndpoint :: (MonadIO m) =>
                 RoutePattern -- ^ e.g. "/login"
              -> OAuthCfg
              -> Scotty m ()
loginEndpoint path oac = get path (loginH oac)

-- | login endpoint handler
loginH :: Monad m =>
          OAuthCfg
       -> Action m ()
loginH oacfg = do
  setHeader "Location" (mkAuthorizeRequest $ azureADApp oacfg)
  status status302

-- -- replyH :: Show b =>
-- --                 OAuthCfg
-- --                 -> (OAuth2Token -> ExceptT T.Text IO b)
-- --                 -> Action IO b
-- replyH oacfg act = do
--   ps <- params
--   excepttToActionM $ do
--        case lookup "code" ps of
--          Just codeP -> do
--            let
--              etoken = ExchangeToken $ TL.toStrict codeP
--            withToken oacfg etoken act
--          Nothing -> throwE $ T.pack $ unwords ["cannot decode token"]

-- withToken :: MonadIO m =>
--           OAuthCfg
--        -> ExchangeToken -- ^ received
--        -> (OAuth2Token -> ExceptT T.Text m b)
--        -> ExceptT T.Text m b
-- withToken oacfg etoken act = do
--   let
--     idpApp = azureADApp oacfg
--   mgr <- liftIO $ newManager tlsManagerSettings
--   tokenResp <- withExceptT oauth2ErrorToText (conduitTokenRequest idpApp mgr etoken)
--   act withUserToken


-- tokenResp :: OAuthCfg
--               -> UserTokens AzureADUser
--               -> ExchangeToken
--               -> (OAuth2Token -> ExceptT TL.Text IO b)
--               -> ExceptT TL.Text IO b
-- withUserToken oacfg tv etoken act = do
--   let
--     idpApp = azureADApp oacfg
--   mgr <- liftIO $ newManager tlsManagerSettings
--   token <- withExceptT oauth2ErrorToText (conduitTokenRequest idpApp mgr etoken)
--   u <- withExceptT bslToText $ conduitUserInfoRequest idpApp mgr (accessToken token)
--   liftIO $ atomically $ updateUserToken tv u token
--   act token



-- type UserTokens u = TVar (M.Map u OAuth2Token)

-- updateUserToken :: (Ord u) => UserTokens u -> u -> OAuth2Token -> STM ()
-- updateUserToken tv k v = modifyTVar tv (M.insert k v)

--


-- | 1) the ExchangeToken arrives with the redirect once the user has approved the scopes in the browser
-- https://learn.microsoft.com/en-us/graph/auth-v2-user?view=graph-rest-1.0&tabs=http#authorization-response
fetchUpdateToken :: Tokens UserSub OAuth2Token
                 -> IdpApplication 'AuthorizationCode AzureAD
                 -> Manager
                 -> ExchangeToken -- ^ also called 'code'. Expires in 10 minutes
                 -> IO (Either T.Text (OAuth2Token, NominalDiffTime))
fetchUpdateToken ts idpApp mgr etoken = do
  tokenResp <- runExceptT $ do
    withExceptT oauth2ErrorToText (conduitTokenRequest idpApp mgr etoken) -- OAuth2 token
  case tokenResp of
    Right oat -> case idToken oat of
      Nothing -> pure $ Left "cannot find ID token in OAuth token. Ensure 'openid' scope appears in token request"
      Just idt -> do
        idtClaimsE <- decValidIdToken idt
        case idtClaimsE of
          -- Right usub -> do
          --   ein <- updateToken ts usub oat -- 2) FIXME here we should also fork a thread and start refresh timer
          --   pure $ Right (oat, ein)
          Left e -> pure $ Left $ T.pack (show e) -- ^ id token validation failed
    Left e -> pure $ Left e

oauth2ErrorToText :: Show a => a -> T.Text
oauth2ErrorToText e = T.pack $ "Unable to fetch access token. Details : " ++ show e

bslToText :: BSL.ByteString -> T.Text
bslToText = T.pack . BSL.unpack

-- scheduleRefresh ts idpApp mgr etoken = forkFinally act cleanup
--   where
--     cleanup = undefined -- FIXME
--     act = do
--       einm <- fetchUpdateToken ts idpApp mgr etoken
--       case einm of
--         Right (oat, ein) -> do
--           let
--             dtSecs = (round ein - 10) -- 10 seconds before expiry
--           threadDelay (dtSecs * 1000000) -- pause thread
--           -- case refreshToken oat of
--           --   Just rt -> 

type Tokens uid t = TVar (TokensHeap uid t)
data TokensHeap uid t = TokensHeap {
  thRefreshHeap :: H.Heap (H.Entry UTCTime t)
  , thUsersMap :: M.Map uid t
                             }

lookupUser :: (MonadIO m, Ord uid) =>
              Tokens uid t
           -> uid -- ^ user identifier e.g. 'sub'
           -> m (Maybe t)
lookupUser ts uid = atomically $ do
  thp <- readTVar ts
  pure $ M.lookup uid (thUsersMap thp)

updateToken :: (MonadIO m, Ord uid) =>
               Tokens uid OAuth2Token -> uid -> OAuth2Token -> m NominalDiffTime
updateToken ts uid oat = do
  t0 <- liftIO getCurrentTime
  let
    ein = fromIntegral $ fromMaybe 3600 (expiresIn oat) -- expires in [sec]
    t1 = ein `addUTCTime` t0 -- expiration UTCTime
    hentry = H.Entry t1 oat
  atomically $ do
    thp <- readTVar ts
    let
      m' = M.insert uid oat (thUsersMap thp)
      h' = H.insert hentry (thRefreshHeap thp)
    writeTVar ts (TokensHeap h' m')
    pure ein


-- | Decode and validate ID token
-- https://learn.microsoft.com/en-us/azure/active-directory/develop/userinfo#consider-using-an-id-token-instead
decValidIdToken :: MonadIO m =>
                   IdToken -- ^ appears in the OAuth2Token if scopes include 'openid'
                -> m (Either (NonEmpty AuthException) UserSub) -- ^ (sub)
decValidIdToken (IdToken idt) = do
  t <- liftIO getCurrentTime
  let
    ve = validationToEither $
         case jwtClaims idt of
           Just c -> (,,) <$> decValidSub c <*> decValidExp Nothing t c <*> decValidNbf t c
           _ -> failure $ AEMalformedJWT (T.unwords ["cannot decode token string"])
  case ve of
    Right (usub, _, _) -> pure $ Right usub
    Left e -> pure $ Left e





-- | Lift ExceptT to ActionM which is basically the handler Monad in Scotty.
excepttToActionM :: MonadIO m =>
                    ExceptT T.Text IO b -> Action m b
excepttToActionM e = do
  result <- liftIO $ runExceptT e
  either (raise . TL.fromStrict) pure result


-- retry-multi

