{-# LANGUAGE DataKinds #-}
{-# language OverloadedStrings #-}
{-# language QuasiQuotes #-}
{-# language ScopedTypeVariables #-}
{-# options_ghc -Wno-unused-imports #-}
module Main (main) where

import Control.Monad.IO.Class (MonadIO(..))
-- aeson
import Data.Aeson (eitherDecode)
import Data.Aeson.Encode.Pretty (encodePretty)
-- bytestring
import qualified Data.ByteString.Lazy.Char8 as LBS8 (pack, unpack, putStrLn)
-- dotenv-micro
import DotEnv.Micro (loadDotEnv)
-- hoauth2
import Network.OAuth.OAuth2 (OAuth2Token(..), AccessToken)
import Network.OAuth2.Experiment (IdpApplication, GrantTypeFlow(..))
-- http-client
import Network.HTTP.Client (Manager)
-- http-types
import Network.HTTP.Types.Status (status200)
-- req
import Network.HTTP.Req (HttpConfig)
-- scotty
import Web.Scotty.Trans (ScottyT, scottyT, get, post, json, text, html, jsonData, status, raise, RoutePattern, middleware)
-- text
import qualified Data.Text.Lazy as TL (Text, pack)
-- transformers
import Control.Monad.Trans.Reader (runReaderT)
-- unliftio
import UnliftIO (MonadUnliftIO(..))
import UnliftIO.Exception (throwIO)
import UnliftIO.STM (STM, TVar, atomically, newTVarIO, readTVar, writeTVar, modifyTVar)
-- uri-bytestring
import URI.ByteString.QQ (uri)
-- wai-extra
import Network.Wai.Middleware.RequestLogger (logStdoutDev)

import Network.OAuth2.Provider.AzureAD (azureBotFrameworkADApp, AzureAD)
import MSAuth (Token, tokenUpdateLoop, readToken, UserSub, Scotty, Action)
import MSAzureAPI (tryReq, run, withTLS)
import MSAzureAPI.BotService (Activity, sendReply)

main :: IO ()
main = server

server :: MonadIO m => m ()
server = do
  loadDotEnv Nothing
  ip <- idpApp
  withTLS $ \hc mgr -> do
    tv <- tokenUpdateLoop ip mgr
    let
      runR r = runReaderT r tv
    scottyT 3000 runR $ do
      middleware logStdoutDev
      pong tv hc "/pong"


pong :: (MonadIO m) =>
        Token OAuth2Token
     -> HttpConfig -> RoutePattern -> Scotty m ()
pong tv hc pth = post pth $ do
  (acti :: Activity) <- jsonData
  m <- (fmap accessToken) <$> readToken tv
  case m of
    Nothing -> raise "readToken: found Nothing"
    Just atok -> do
      ei <- run hc $ sendReply acti "It works!" [] atok
      case ei of
        Right _ -> do
          liftIO $ LBS8.putStrLn $ encodePretty acti
          status status200
        Left e -> raise $ TL.pack (show e)


-- also double check https://stackoverflow.com/a/63929994/2890063 in the AAD app manifest
idpApp :: MonadIO m => m (IdpApplication 'ClientCredentials AzureAD)
idpApp = azureBotFrameworkADApp "ms-azure-bot-framework-api-test"

