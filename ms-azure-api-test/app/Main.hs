{-# LANGUAGE DataKinds #-}
{-# language OverloadedStrings #-}
{-# language QuasiQuotes #-}
module Main (main) where

import Control.Monad.IO.Class (MonadIO(..))
-- hoauth2
import Network.OAuth.OAuth2 (OAuth2Token(..))
import Network.OAuth2.Experiment (IdpApplication, GrantTypeFlow(..))
-- unliftio
import UnliftIO (MonadUnliftIO(..))
import UnliftIO.Exception (throwIO)
import UnliftIO.STM (STM, TVar, atomically, newTVarIO, readTVar, writeTVar, modifyTVar)
-- uri-bytestring
import URI.ByteString.QQ (uri)

import Network.OAuth2.Provider.AzureAD (azureBotFrameworkADApp, AzureBotFramework)
import MSAuth (Token, tokenUpdateLoop, readToken, UserSub, Scotty, Action)
import MSAzureAPI (tryReq)

main :: IO ()
main = pure ()


-- also double check https://stackoverflow.com/a/63929994/2890063 in the AAD app manifest
idpApp :: MonadIO m => m (IdpApplication 'ClientCredentials AzureBotFramework)
idpApp = azureBotFrameworkADApp "ms-azure-bot-framework-api-test"

