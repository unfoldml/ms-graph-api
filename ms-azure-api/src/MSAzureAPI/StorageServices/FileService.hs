-- | StorageServices.FileService
--
-- authorize with AD : https://learn.microsoft.com/en-us/rest/api/storageservices/authorize-with-azure-active-directory
--
-- permissions for calling data operations : https://learn.microsoft.com/en-us/rest/api/storageservices/authorize-with-azure-active-directory#permissions-for-calling-data-operations
module MSAzureAPI.StorageServices.FileService (
  -- * Files
  getFile
  -- * Directories
  , listDirectoriesAndFiles
  , DirItem(..)
  ) where

import Control.Applicative (Alternative(..))
import Control.Monad.IO.Class (MonadIO(..))
import Data.Foldable (asum)
import Data.Functor (void)
import Data.Maybe (listToMaybe)
import qualified Text.ParserCombinators.ReadP as RP (ReadP, readP_to_S, choice, many, between, char, string, satisfy)

-- bytestring
import qualified Data.ByteString as BS (ByteString)
import qualified Data.ByteString.Char8 as BS8 (pack, unpack)
import qualified Data.ByteString.Lazy as LBS (ByteString)
-- hoauth2
-- import Network.OAuth.OAuth2 (OAuth2Token(..))
import Network.OAuth.OAuth2.Internal (AccessToken(..))
-- req
import Network.HTTP.Req (Req, Url, Option, Scheme(..), header, (=:))
-- text
import Data.Text (Text, pack, unpack)
import qualified Data.Text.Lazy as TL (Text, pack, unpack, toStrict)
-- time
import Data.Time (UTCTime, getCurrentTime)
import Data.Time.Format (FormatTime, formatTime, defaultTimeLocale)
import Data.Time.LocalTime (getZonedTime)
-- xeno
import qualified Xeno.DOM.Robust as X (Node, Content(..), name, contents, children)
-- xmlbf-xeno
import qualified Xmlbf.Xeno as XB (fromRawXml)
-- xmlbf
import qualified Xmlbf as XB (Parser, runParser, pElement, pText)

import MSAzureAPI.Internal.Common (APIPlane(..), (==:), get, getBs, post, getLbs)





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
-- @GET https:\/\/myaccount.file.core.windows.net\/myshare\/mydirectorypath?restype=directory&comp=list@
listDirectoriesAndFiles :: Text -- ^ storage account
                        -> Text -- ^ file share
                        -> Text -- ^ directory path, including directories
                        -> AccessToken
                        -> Req (Either String [DirItem])
listDirectoriesAndFiles acct fshare fpath atok = do
  os <- msStorageReqHeaders
  bs <- getBs (APData domain) pth (os <> "restype" ==: "directory" <> "comp" ==: "list") atok
  pure $ parseXML listDirectoriesP bs
  where
    domain = acct <> ".file.core.windows.net"
    pth = [fshare, fpath]

-- | Directory item, as returned by 'listDirectoriesAndFiles'
data DirItem = DIFile {diId :: Text, diName :: Text}
             | DIDirectory {diId :: Text, diName :: Text}
             deriving (Show)

-- | XML parser for the response body format shown here: https://learn.microsoft.com/en-us/rest/api/storageservices/list-directories-and-files#response-body
listDirectoriesP :: XB.Parser [DirItem]
listDirectoriesP = do
  tag "EnumerationResults" $ do
    enumResultsIgnore
    es <- entries
    selfClosing "NextMarker"
    pure es

enumResultsIgnore :: XB.Parser ()
enumResultsIgnore = ignoreList ["Marker", "Prefix", "MaxResults", "DirectoryId"]

entries :: XB.Parser [DirItem]
entries = tag "Entries" $ many (file <|> directory)

file :: XB.Parser DirItem
file = tag "File" $ do
  fid <- fileId
  fname <- fileName
  properties
  entryFooter
  pure $ DIFile fid fname

directory :: XB.Parser DirItem
directory = tag "Directory" $ do
  fid <- fileId
  fname <- fileName
  properties
  entryFooter
  pure $ DIDirectory fid fname



entryFooter :: XB.Parser ()
entryFooter = ignoreList ["Attributes", "PermissionKey"]

fileId :: XB.Parser Text
fileId = TL.toStrict <$> tag "FileId" anystring

fileName :: XB.Parser Text
fileName = TL.toStrict <$> tag "Name" anystring

properties :: XB.Parser ()
properties = tag "Properties" $
  ignoreList ["Content-Length", "CreationTime", "LastAccessTime", "LastWriteTime", "ChangeTime", "Last-Modified", "Etag"]

ignoreList :: [Text] -> XB.Parser ()
ignoreList ns = void $ many (asum (map (`XB.pElement` XB.pText) ns))

selfClosing :: Text -> XB.Parser ()
selfClosing t = tag t (pure ())


anystring :: XB.Parser TL.Text
anystring = XB.pText
tag :: Text -> XB.Parser a -> XB.Parser a
tag = XB.pElement

parseXML :: XB.Parser b -> BS.ByteString -> Either String b
parseXML p bs = XB.fromRawXml bs >>= XB.runParser p



-- -- t0, t1, tdir, tfile, tentries :: String
-- t0, t1, t1', tfile :: BS.ByteString
-- t0 = "<Properties><CreationTime>datetime</CreationTime><LastAccessTime>datetime</LastAccessTime><LastWriteTime>datetime</LastWriteTime><ChangeTime>datetime</ChangeTime><Last-Modified>datetime</Last-Modified><Etag>etag</Etag></Properties>"

-- t1' = "<?xml version=\"1.0\" encoding=\"utf-8\"?> <EnumerationResults ServiceEndpoint=\"https://myaccount.file.core.windows.net/\" ShareName=\"myshare\" ShareSnapshot=\"date-time\" DirectoryPath=\"directory-path\"> <Marker>string-value</Marker> <Prefix>string-value</Prefix> <MaxResults>int-value</MaxResults> <DirectoryId>directory-id</DirectoryId> <Entries> <File> <FileId>file-id</FileId> <Name>file-name</Name> <Properties> <Content-Length>size-in-bytes</Content-Length> <CreationTime>datetime</CreationTime> <LastAccessTime>datetime</LastAccessTime> <LastWriteTime>datetime</LastWriteTime> <ChangeTime>datetime</ChangeTime> <Last-Modified>datetime</Last-Modified> <Etag>etag</Etag> </Properties> <Attributes>Archive|Hidden|Offline|ReadOnly</Attributes> <PermissionKey>4066528134148476695*1</PermissionKey> </File> <Directory> <FileId>file-id</FileId> <Name>directory-name</Name> <Properties> <CreationTime>datetime</CreationTime> <LastAccessTime>datetime</LastAccessTime> <LastWriteTime>datetime</LastWriteTime> <ChangeTime>datetime</ChangeTime> <Last-Modified>datetime</Last-Modified> <Etag>etag</Etag> </Properties> <Attributes>Archive|Hidden|Offline|ReadOnly</Attributes> <PermissionKey>4066528134148476695*1</PermissionKey> </Directory> </Entries> <NextMarker /> </EnumerationResults>"

-- t1 = "<?xml version=\"1.0\" encoding=\"utf-8\"?><EnumerationResults ServiceEndpoint=\"https://myaccount.file.core.windows.net/\" ShareName=\"myshare\" ShareSnapshot=\"date-time\" DirectoryPath=\"directory-path\"><Marker>string-value</Marker><Prefix>string-value</Prefix><MaxResults>int-value</MaxResults><DirectoryId>directory-id</DirectoryId><Entries><File><FileId>file-id</FileId><Name>file-name</Name><Properties><Content-Length>size-in-bytes</Content-Length><CreationTime>datetime</CreationTime><LastAccessTime>datetime</LastAccessTime><LastWriteTime>datetime</LastWriteTime><ChangeTime>datetime</ChangeTime><Last-Modified>datetime</Last-Modified><Etag>etag</Etag></Properties><Attributes>Archive|Hidden|Offline|ReadOnly</Attributes><PermissionKey>4066528134148476695*1</PermissionKey></File><Directory><FileId>file-id</FileId><Name>directory-name</Name><Properties><CreationTime>datetime</CreationTime><LastAccessTime>datetime</LastAccessTime><LastWriteTime>datetime</LastWriteTime><ChangeTime>datetime</ChangeTime><Last-Modified>datetime</Last-Modified><Etag>etag</Etag></Properties><Attributes>Archive|Hidden|Offline|ReadOnly</Attributes><PermissionKey>4066528134148476695*1</PermissionKey></Directory></Entries><NextMarker /></EnumerationResults>"

-- -- tdir = "<Directory><FileId>file-id</FileId><Name>directory-name</Name><Properties><CreationTime>datetime</CreationTime><LastAccessTime>datetime</LastAccessTime><LastWriteTime>datetime</LastWriteTime><ChangeTime>datetime</ChangeTime><Last-Modified>datetime</Last-Modified><Etag>etag</Etag></Properties><Attributes>Archive|Hidden|Offline|ReadOnly</Attributes><PermissionKey>4066528134148476695*1</PermissionKey></Directory>"

-- tfile = "<File><FileId>file-id</FileId><Name>file-name</Name><Properties><Content-Length>size-in-bytes</Content-Length><CreationTime>datetime</CreationTime><LastAccessTime>datetime</LastAccessTime><LastWriteTime>datetime</LastWriteTime><ChangeTime>datetime</ChangeTime><Last-Modified>datetime</Last-Modified><Etag>etag</Etag></Properties><Attributes>Archive|Hidden|Offline|ReadOnly</Attributes><PermissionKey>4066528134148476695*1</PermissionKey></File>"

-- -- tentries = "<Entries><File><FileId>file-id</FileId><Name>file-name</Name><Properties><Content-Length>size-in-bytes</Content-Length><CreationTime>datetime</CreationTime><LastAccessTime>datetime</LastAccessTime><LastWriteTime>datetime</LastWriteTime><ChangeTime>datetime</ChangeTime><Last-Modified>datetime</Last-Modified><Etag>etag</Etag></Properties><Attributes>Archive|Hidden|Offline|ReadOnly</Attributes><PermissionKey>4066528134148476695*1</PermissionKey></File><Directory><FileId>file-id</FileId><Name>directory-name</Name><Properties><CreationTime>datetime</CreationTime><LastAccessTime>datetime</LastAccessTime><LastWriteTime>datetime</LastWriteTime><ChangeTime>datetime</ChangeTime><Last-Modified>datetime</Last-Modified><Etag>etag</Etag></Properties><Attributes>Archive|Hidden|Offline|ReadOnly</Attributes><PermissionKey>4066528134148476695*1</PermissionKey></Directory></Entries>"