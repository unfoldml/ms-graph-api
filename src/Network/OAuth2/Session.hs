{-# language DeriveGeneric, GeneralizedNewtypeDeriving, DerivingStrategies #-}
{-# language OverloadedStrings #-}
{-# options_ghc -Wno-unused-imports #-}
module Network.OAuth2.Session where

import Control.Monad.IO.Class (MonadIO(..))
import Data.String (IsString(..))

-- aeson
import Data.Aeson
-- hoauth2
import Network.OAuth.OAuth2 (OAuth2Token(..))
import Network.OAuth2.Experiment (mkAuthorizeRequest, conduitTokenRequest)
import Network.OAuth.OAuth2.Internal (ExchangeToken, OAuth2Error)
import Network.OAuth.OAuth2.TokenRequest (Errors)
-- http-conduit
import Network.HTTP.Conduit (newManager, tlsManagerSettings)
-- http-types
import Network.HTTP.Types (status302)
-- scotty
import Web.Scotty (ActionM, scotty)
import qualified Web.Scotty as Scotty
-- stm
import Control.Monad.STM (STM, atomically, retry, orElse, throwSTM)
import Control.Concurrent.STM.TVar (TVar, newTVarIO, readTVar, writeTVar, modifyTVar)
-- text
import qualified Data.Text.Lazy as TL (Text, pack)
-- transformers
import Control.Monad.Trans.Except (ExceptT(..), withExceptT)
-- uri-bytestring
import URI.ByteString (URI)

import Network.OAuth2.Provider.AzureAD (OAuthCfg, azureADApp)


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


replyH oacfg code = do
  let
    idpApp = azureADApp oacfg
  mgr <- liftIO $ newManager tlsManagerSettings
  tokenResp <- withExceptT oauth2ErrorToText (conduitTokenRequest idpApp mgr code)
  undefined

oauth2ErrorToText :: Show a => a -> TL.Text
oauth2ErrorToText e = TL.pack $ "Unable to fetch access token. Details : " ++ show e


-- type UserSession = TVar UserState

startUserSession :: IO (TVar UserState)
startUserSession = newTVarIO USNotLoggedIn

setTokens :: TVar UserState -> OAuth2Token -> STM ()
setTokens tv oat = writeTVar tv (USLoggedIn oat)

data UserState = USNotLoggedIn
               | USLoggedIn OAuth2Token
               deriving (Eq)

