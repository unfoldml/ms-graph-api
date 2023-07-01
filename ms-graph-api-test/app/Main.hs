{-# LANGUAGE DataKinds #-}
{-# language OverloadedStrings #-}
{-# language QuasiQuotes #-}
{-# options_ghc -Wno-unused-imports #-}
module Main (main) where

import Control.Monad.IO.Class (MonadIO(..))

-- aeson-pretty
import qualified Data.Aeson.Encode.Pretty as A (encodePretty)
-- bytestring
import qualified Data.ByteString.Lazy.Char8 as LBS (putStrLn)
-- hoauth2
import Network.OAuth.OAuth2 (OAuth2Token(..))
import Network.OAuth2.Experiment (IdpApplication, GrantTypeFlow(..))
-- http-client
import Network.HTTP.Client (Manager, newManager)
-- http-client-tls
import Network.HTTP.Client.TLS (tlsManagerSettings)
-- req
import Network.HTTP.Req (runReq, defaultHttpConfig, httpConfigAltManager)
-- scotty
import Web.Scotty.Trans (ScottyT, scottyT, get, text, html, RoutePattern, middleware)
-- text
import qualified Data.Text as T (unpack)
import qualified Data.Text.Lazy.Encoding as TL (decodeUtf8)
import qualified Data.Text.Lazy as TL (Text, pack)
-- transformers
import Control.Monad.Trans.Reader (runReaderT)
-- unliftio
import UnliftIO.STM (STM, newTVarIO)
-- uri-bytestring
import URI.ByteString.QQ (uri)
-- wai-extra
import Network.Wai.Middleware.RequestLogger (logStdoutDev)

import qualified MSGraphAPI.Files.DriveItem as MSDI (listRootChildrenMe)
import qualified MSGraphAPI.Users.Group as MSGU (getMeJoinedTeams)
import qualified MSGraphAPI.Users.User as MSG (getMe, User(..))
import Network.OAuth2.Provider.AzureAD (OAuthCfg(..), azureOAuthADApp, AzureAD)
import MSAuth (applyDotEnv, Tokens, newTokens, tokensToList, withAADUser, loginEndpoint, replyEndpoint, UserSub, Scotty, Action)


main :: IO ()
main = server

server :: MonadIO m => m ()
server = do
  ts <- newTokens
  applyDotEnv (Just ".env")
  mgr <- liftIO $ newManager tlsManagerSettings
  ip <- idpApp
  let
    runR r = runReaderT r ts
  scottyT 3000 runR $ do
    middleware logStdoutDev
    loginEndpoint ip "/oauth/login"
    replyEndpoint ip ts mgr "/oauth/reply"
    allTokensEndpoint ts "/tokens"
    currentUsersEndpoint ts (Just mgr) "/me"
    meFilesEndpoint ts (Just mgr) "/me/files"
    meTeamsEndpoint ts (Just mgr) "/me/teams"

-- currentUserEndpoint :: MonadIO m =>
--                        Tokens UserSub OAuth2Token
--                     -> RoutePattern -> Scotty m ()
-- currentUserEndpoint ts pth = get pth $ withAADUser ts "/oauth/login" $ \oat -> do
--   let
--     t = accessToken oat
--   u <- runReq defaultHttpConfig $ MSG.getMe t
--   let
--     uname = MSG.uDisplayName u
--     h = TL.pack $ unwords ["<html>", "<h1>", T.unpack uname, "</h1>","</html>"]
--   html h

meTeamsEndpoint :: (MonadIO m) =>
                   Tokens a OAuth2Token
                -> Maybe Manager -> RoutePattern -> Scotty m ()
meTeamsEndpoint ts mmgr pth = get pth $ do
    tsl <- tokensToList ts
    let
      f (_, oat) = do
        let
          t = accessToken oat
        item <- runReq defaultHttpConfig{ httpConfigAltManager = mmgr } $ MSGU.getMeJoinedTeams t
        let
          js = A.encodePretty item
        pure js
    rows <- traverse f tsl
    text $ TL.decodeUtf8 $ mconcat rows

meFilesEndpoint :: (MonadIO m) =>
                   Tokens a OAuth2Token
                -> Maybe Manager -> RoutePattern -> Scotty m ()
meFilesEndpoint ts mmgr pth = get pth $ do
    tsl <- tokensToList ts
    let
      f (_, oat) = do
        let
          t = accessToken oat
        item <- runReq defaultHttpConfig{ httpConfigAltManager = mmgr } $ MSDI.listRootChildrenMe  t
        let
          js = A.encodePretty item
        pure js
    rows <- traverse f tsl
    text $ TL.decodeUtf8 $ mconcat rows

currentUsersEndpoint :: (MonadIO m) =>
                        Tokens a OAuth2Token
                     -> Maybe Manager -- ^ if Nothing it uses the default implicit connection manager
                     -> RoutePattern -> Scotty m ()
currentUsersEndpoint ts mmgr pth = get pth $ do
    tsl <- tokensToList ts
    let
      f (_, oat) = do
        let
          t = accessToken oat
        usr <- runReq defaultHttpConfig{ httpConfigAltManager = mmgr } $ MSG.getMe t
        let
          row = unwords ["<tr><td>", show usr, "</td></tr>"]
        pure row
    rows <- traverse f tsl
    let
      h = TL.pack ("<table>" <> mconcat rows <> "</table>")
    html h

allTokensEndpoint :: (MonadIO m, Show a1) =>
                     Tokens a1 OAuth2Token -> RoutePattern -> Scotty m ()
allTokensEndpoint ts pth = get pth $ do
  tsl <- tokensToList ts
  html (table tsl)

table :: (Foldable t, Show a) => t (a, OAuth2Token) -> TL.Text
table mm = TL.pack ("<table>" <> foldMap insf mm <> "</table>")
  where
    insf (k, oat) = unwords ["<tr><td>", show k , "</td><td>", show (accessToken oat), "</td></tr>"]



-- also double check https://stackoverflow.com/a/63929994/2890063 in the AAD app manifest
idpApp :: MonadIO m => m (IdpApplication 'AuthorizationCode AzureAD)
idpApp = azureOAuthADApp (OAuthCfg
                         "ms-graph-api-test"
                         ["profile", "email", "User.Read", "Files.Read", "Team.ReadBasic.All"]
                         "abcd1234"
                         [uri|https://66e7-213-89-187-253.ngrok-free.app/oauth/reply|]
                       )



