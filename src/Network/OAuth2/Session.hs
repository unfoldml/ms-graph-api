{-# language DeriveGeneric, GeneralizedNewtypeDeriving, DerivingStrategies #-}
{-# language OverloadedStrings #-}
{-# options_ghc -Wno-unused-imports #-}
module Network.OAuth2.Session where

import Control.Monad.IO.Class (MonadIO(..))
import Data.String (IsString(..))

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
import Network.OAuth.OAuth2.Internal (AccessToken(..), ExchangeToken(..), OAuth2Error)
import Network.OAuth.OAuth2.TokenRequest (Errors)
-- http-conduit
import Network.HTTP.Conduit (newManager, tlsManagerSettings)
-- http-types
import Network.HTTP.Types (status302)
-- scotty
import Web.Scotty (ActionM, scotty)
import Web.Scotty.Trans (ActionT, ScottyT)
import qualified Web.Scotty as Scotty
-- stm
import Control.Monad.STM (STM, atomically, retry, orElse, throwSTM)
import Control.Concurrent.STM.TVar (TVar, newTVarIO, readTVar, writeTVar, modifyTVar)
-- text
import qualified Data.Text as T (Text, pack)
import qualified Data.Text.Lazy as TL (Text, pack, toStrict, takeWhile)
-- time
import Data.Time (UTCTime(..), getCurrentTime, fromGregorian, diffUTCTime, addUTCTime, Day, NominalDiffTime)
import Data.Time.Format (FormatTime(..), formatTime, iso8601DateFormat, defaultTimeLocale)
-- transformers
import Control.Monad.Trans.Except (ExceptT(..), withExceptT, runExceptT, throwE)
-- uri-bytestring
import URI.ByteString (URI)

import Network.OAuth2.Provider.AzureAD (OAuthCfg, azureADApp, AzureADUser(..))

type Action = ActionT TL.Text
type Scotty = ScottyT TL.Text


loginEndpoint :: Scotty.RoutePattern -- ^ e.g. "/login"
              -> OAuthCfg
              -> Scotty.ScottyM ()
loginEndpoint path oac = Scotty.get path (loginH oac)

-- | login endpoint handler
loginH :: OAuthCfg
       -> ActionM ()
loginH oacfg = do
  Scotty.setHeader "Location" (mkAuthorizeRequest $ azureADApp oacfg)
  Scotty.status status302

replyH :: Show b =>
                OAuthCfg
                -> (OAuth2Token -> ExceptT TL.Text IO b)
                -> Action IO b
replyH oacfg act = do
  ps <- Scotty.params
  excepttToActionM $ do
       case lookup "code" ps of
         Just codeP -> do
           let
             etoken = ExchangeToken $ TL.toStrict codeP
           withToken oacfg etoken act
         Nothing -> throwE $ TL.pack $ unwords ["cannot decode token"]

withToken :: MonadIO m =>
          OAuthCfg
       -> ExchangeToken -- ^ received
       -> (OAuth2Token -> ExceptT TL.Text m b)
       -> ExceptT TL.Text m b
withToken oacfg etoken act = do
  let
    idpApp = azureADApp oacfg
  mgr <- liftIO $ newManager tlsManagerSettings
  tokenResp <- withExceptT oauth2ErrorToText (conduitTokenRequest idpApp mgr etoken)
  act tokenResp

withUserToken oacfg tv etoken act = do
  let
    idpApp = azureADApp oacfg
  mgr <- liftIO $ newManager tlsManagerSettings
  token <- withExceptT oauth2ErrorToText (conduitTokenRequest idpApp mgr etoken)
  u <- withExceptT bslToText $ conduitUserInfoRequest idpApp mgr (accessToken token)
  liftIO $ atomically $ updateUserToken tv u token
  act token



type UserTokens u = TVar (M.Map u OAuth2Token)

updateUserToken :: (Ord u) => UserTokens u -> u -> OAuth2Token -> STM ()
updateUserToken tv k v = modifyTVar tv (M.insert k v)

-- 

oauth2ErrorToText :: Show a => a -> TL.Text
oauth2ErrorToText e = TL.pack $ "Unable to fetch access token. Details : " ++ show e

bslToText :: BSL.ByteString -> TL.Text
bslToText = TL.pack . BSL.unpack

-- -- type UserSession = TVar UserState

-- startUserSession :: IO (TVar UserState)
-- startUserSession = newTVarIO USNotLoggedIn

-- setTokens :: TVar UserState -> OAuth2Token -> STM ()
-- setTokens tv oat = writeTVar tv (USLoggedIn oat)

-- data UserState = USNotLoggedIn
--                | USLoggedIn OAuth2Token
--                deriving (Eq)

type Tokens uid t = TVar (TokensHeap uid t)
data TokensHeap uid t = TokensHeap {
  thRefreshHeap :: H.Heap (H.Entry UTCTime t)
  , thUsersMap :: M.Map uid t
                             }

toRefreshHeapEntry :: MonadIO m =>
                      OAuth2Token -> m (H.Entry UTCTime OAuth2Token)
toRefreshHeapEntry oat = do
  t0 <- liftIO getCurrentTime
  let
    ein = fromIntegral $ maybe 3600 id (expiresIn oat)
    t1 = ein `addUTCTime` t0
  pure $ H.Entry t1 oat

-- | Lift ExceptT to ActionM which is basically the handler Monad in Scotty.
excepttToActionM :: Show a => ExceptT TL.Text IO a -> ActionM a
excepttToActionM e = do
  result <- liftIO $ runExceptT e
  either Scotty.raise pure result


-- retry-multi

