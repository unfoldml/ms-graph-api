{-# LANGUAGE DataKinds #-}
{-# language OverloadedStrings #-}
{-# language QuasiQuotes #-}
{-# options_ghc -Wno-unused-imports #-}
module Main (main) where

import Control.Monad.IO.Class (MonadIO(..))
import Data.Maybe (fromMaybe)

-- aeson-pretty
import qualified Data.Aeson.Encode.Pretty as A (encodePretty)
-- bytestring
import qualified Data.ByteString.Lazy.Char8 as LBS (putStrLn, pack)
-- dotenv-micro
import DotEnv.Micro (loadDotEnv)
-- http-client
import Network.HTTP.Client (Manager, newManager)
-- http-client-tls
import Network.HTTP.Client.TLS (tlsManagerSettings)
-- ms-auth
import MSAuth (OAuth2Token(..))
import Network.OAuth2.Provider.AzureAD (OAuthCfg(..), azureOAuthADApp, AzureAD, IdpApplication, GrantTypeFlow(..))
-- req
import Network.HTTP.Req (HttpConfig, runReq, defaultHttpConfig, httpConfigAltManager)
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

import qualified MSGraphAPI as MSG (Collection(..), run, withTLS)
import qualified MSGraphAPI.Files.Drive as MSD (Drive(..), listDrivesGroup)
import qualified MSGraphAPI.Files.DriveItem as MSDI (listRootChildrenMe)
import qualified MSGraphAPI.Users.Group as MSGU (Group(..), listMeJoinedTeams, listGroupsDriveItems)
import qualified MSGraphAPI.Users.User as MSG (getMe, User(..))

import MSAuth (Tokens, newTokens, tokensToList, withAADUser, loginEndpoint, replyEndpoint, UserSub, Scotty, Action)


main :: IO ()
main = server

server :: MonadIO m => m ()
server = do
  ts <- newTokens
  loadDotEnv Nothing
  ip <- idpApp
  MSG.withTLS $ \hc mgr -> do
    let
      runR r = runReaderT r ts
    scottyT 3000 runR $ do
      middleware logStdoutDev
      loginEndpoint ip "/oauth/login"
      replyEndpoint ip ts mgr "/oauth/reply"

      meGroupDrivesEndpoint ts hc "/me/group/drives"
      currentUsersEndpoint ts hc "/me"
      meFilesEndpoint ts hc "/me/files"
      meTeamsEndpoint ts hc "/me/teams"


groupDriveItems t = do
  gs <- MSG.cValue <$> MSGU.listMeJoinedTeams t
  traverse (\g -> MSGU.listGroupsDriveItems (MSGU.gId g) t ) gs

meGroupDrivesEndpoint :: (MonadIO m) =>
                         Tokens a OAuth2Token
                      -> HttpConfig -> RoutePattern -> Scotty m ()
meGroupDrivesEndpoint ts hc pth = get pth $ do
    tsl <- tokensToList ts
    let
      f (_, oat) = do
        let
          t = accessToken oat
        iteme <- MSG.run hc $ groupDriveItems t
        case iteme of
          Right item -> pure $ A.encodePretty item
          Left e -> pure $ LBS.pack $ show e
    rows <- traverse f tsl
    text $ TL.decodeUtf8 $ mconcat rows





meTeamsEndpoint :: (MonadIO m) =>
                   Tokens a OAuth2Token
                -> HttpConfig -> RoutePattern -> Scotty m ()
meTeamsEndpoint ts hc pth = get pth $ do
    tsl <- tokensToList ts
    let
      f (_, oat) = do
        let
          t = accessToken oat
        item <- runReq hc $ MSGU.listMeJoinedTeams t
        let
          js = A.encodePretty item
        pure js
    rows <- traverse f tsl
    text $ TL.decodeUtf8 $ mconcat rows

meFilesEndpoint :: (MonadIO m) =>
                   Tokens a OAuth2Token
                -> HttpConfig -> RoutePattern -> Scotty m ()
meFilesEndpoint ts hc pth = get pth $ do
    tsl <- tokensToList ts
    let
      f (_, oat) = do
        let
          t = accessToken oat
        item <- runReq hc $ MSDI.listRootChildrenMe  t
        let
          js = A.encodePretty item
        pure js
    rows <- traverse f tsl
    text $ TL.decodeUtf8 $ mconcat rows

currentUsersEndpoint :: (MonadIO m) =>
                        Tokens a OAuth2Token
                     -> HttpConfig
                     -> RoutePattern -> Scotty m ()
currentUsersEndpoint ts hc pth = get pth $ do
    tsl <- tokensToList ts
    let
      f (_, oat) = do
        let
          t = accessToken oat
        usr <- runReq hc $ MSG.getMe t
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
                         ["profile", "email", "User.Read", "Files.Read.All", "Team.ReadBasic.All"]
                         "abcd1234"
                         [uri|https://66e7-213-89-187-253.ngrok-free.app/oauth/reply|]
                       )



