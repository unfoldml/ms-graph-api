-- | StorageServices.FileService
--
-- authorize with AD : https://learn.microsoft.com/en-us/rest/api/storageservices/authorize-with-azure-active-directory
--
-- permissions for calling data operations : https://learn.microsoft.com/en-us/rest/api/storageservices/authorize-with-azure-active-directory#permissions-for-calling-data-operations
module MSAzureAPI.StorageServices.FileService (getFile) where

import Control.Monad.IO.Class (MonadIO(..))

-- bytestring
import qualified Data.ByteString.Char8 as BS8 (pack, unpack)
import qualified Data.ByteString.Lazy as LBS (ByteString)
-- hoauth2
-- import Network.OAuth.OAuth2 (OAuth2Token(..))
import Network.OAuth.OAuth2.Internal (AccessToken(..))
-- req
import Network.HTTP.Req (Req, Url, Option, Scheme(..), header)
-- text
import Data.Text (Text, pack, unpack)
-- time
import Data.Time (UTCTime, getCurrentTime)
import Data.Time.Format (FormatTime, formatTime, defaultTimeLocale)
import Data.Time.LocalTime (getZonedTime)

import MSAzureAPI.Internal.Common (APIPlane(..), get, post, getLbs)

{- | Headers:

https://learn.microsoft.com/en-us/rest/api/storageservices/authorize-with-azure-active-directory#call-storage-operations-with-oauth-tokens

Requests that use an OAuth 2.0 token from Azure Active Directory (Azure AD): To authorize a request with Azure AD, pass the

x-ms-version

header on the request with a service version of 2017-11-09 or higher. For more information, see Call storage operations with OAuth tokens in Authorize with Azure Active Directory.

-}

xMsVerHeader :: Option 'Https
xMsVerHeader = header "x-ms-version" "2022-11-02"


-- | x-ms-date header should be formatted as
--
-- %a, %d %b %Y %H:%M:%S GMT
--
-- e.g. Fri, 26 Jun 2015 23:39:12 GMT
xMsDateHeader :: MonadIO m => m (Option 'Https)
xMsDateHeader = do
  zt <- liftIO getZonedTime
  let
    zth = formatTime defaultTimeLocale "%a, %d %b %Y %H:%M:%S %Z" zt
  pure $ header "x-ms-date" (BS8.pack zth)

-- getDateHeader :: MonadIO m => m String
-- getDateHeader = do
--   zt <- liftIO getZonedTime
--   pure $ formatTime defaultTimeLocale "%a, %d %b %Y %H:%M:%S %Z" zt


-- | Configure a StorageService request
-- msStorageReqConfig :: MonadIO m =>
--                       AccessToken -> Text -> [Text] -> m (Url 'Https, Option 'Https)
-- msStorageReqConfig atok uriBase uriRest = do
--   dateHeader <- xMsDateHeader
--   let
--     verHeader = xMsVerHeader
--     (url, os) = msAzureDataReqConfig atok uriBase uriRest
--   pure (url, os <> verHeader <> dateHeader)

msStorageReqHeaders :: MonadIO m => m (Option 'Https)
msStorageReqHeaders = do
  dh <- xMsDateHeader
  let
    vh = xMsVerHeader
  pure (dh <> vh)

-- | get file  https://learn.microsoft.com/en-us/rest/api/storageservices/get-file#request
--
-- @GET https:\/\/myaccount.file.core.windows.net\/myshare\/mydirectorypath\/myfile@
getFile :: Text -- ^ storage account
        -> Text -- ^ file share
        -> Text -- ^ filepath, including directories
        -> AccessToken
        -> Req LBS.ByteString
getFile acct fshare fpath atok = do
  os <- msStorageReqHeaders
  getLbs (APData domain) pth os atok
  where
    domain = acct <> ".file.core.windows.net"
    pth = [fshare, fpath]

-- | list directories and files  https://learn.microsoft.com/en-us/rest/api/storageservices/list-directories-and-files#request 
--
-- GET https://myaccount.file.core.windows.net/myshare/mydirectorypath?restype=directory&comp=list
-- listDirectoryAndFiles

--
-- Path component 	Description
--
-- myaccount 	The name of your storage account.
-- myshare 	The name of your file share.
-- mydirectorypath 	Optional. The path to the directory.
-- myfile 	The name of the file.



