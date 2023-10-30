{-# language OverloadedStrings #-}
{-# language TupleSections #-}
{-# options_ghc -Wno-unused-imports #-}
-- | https://learn.microsoft.com/en-us/rest/api/storageservices/authorize-with-shared-key
module Network.OAuth2.Provider.AzureAD.SharedKey where

import Data.Bifunctor (first)
import Data.Function ((&))
import Data.List (sortOn, intersperse)
import Data.String (IsString(..))

-- base64
import qualified Data.ByteString.Base64 as B64 (encodeBase64, decodeBase64)
-- bytestring
import qualified Data.ByteString as BS (ByteString)
import qualified Data.ByteString.Char8 as BS (pack, unpack)
import qualified Data.ByteString.Lazy as LBS (ByteString)
-- cryptohash-sha256
import qualified Crypto.Hash.SHA256 as H (hmac)
-- http-conduit
import Network.HTTP.Simple (Request, Response, httpBS, httpLBS, defaultRequest, setRequestHost, setRequestPath, setRequestSecure, setRequestMethod, setRequestHeader, setRequestBodySource, setRequestBodyLBS, getResponseStatus, getResponseBody)
-- http-types
import Network.HTTP.Types (RequestHeaders, Header, HeaderName)
-- modern-uri
import Text.URI (mkURI)
-- req
import Network.HTTP.Req (runReq, defaultHttpConfig, req, NoReqBody(..), useHttpsURI, GET(..), BsResponse, bsResponse, header, attachHeader, customAuth, Option)
-- text
import qualified Data.Text as T (Text, pack, unpack)
import qualified Data.Text.Encoding as T (encodeUtf8, decodeUtf8)
-- time
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime, defaultTimeLocale)


-- "Tue, 3 Oct 2023 19:33:08 GMT"
timeString :: IO String
timeString = f <$> getCurrentTime
  where
    f = formatTime defaultTimeLocale "%a, %e %b %Y %H:%M:%S GMT"

xMsDate :: IO (String, String)
xMsDate = ("x-ms-date", ) <$> timeString

canonicalizeHeaders :: [(String, String)] -> [T.Text]
canonicalizeHeaders = map canonicalizeHdr . sortOn fst
  where
    canonicalizeHdr (k, v) = T.pack $ k <> ":" <> v

data ToSignLite = ToSignLite {
  tslVerb :: T.Text -- ^ REST verb
  , tslContentType :: T.Text -- ^ MIME content type
  , tslCanHeaders :: [(String, String)]
  , tslPath :: T.Text -- ^ resource path
  }

{- |
PUT\n\ntext/plain; charset=UTF-8\n\nx-ms-date:Sun, 20 Sep 2009 20:36:40 GMT\nx-ms-meta-m1:v1\nx-ms-meta-m2:v2\n/testaccount1/mycontainer/hello.txt

ctzMq410TV3wS7upTBcunJTDLEJwMAZuFPfr0mrrA08=

-}


signed :: ToSignLite
       -> String -- ^ storage account name
       -> String -- ^ file share
       -> BS.ByteString -- ^ shared key (from Azure portal)
       -> IO (T.Text, Option scheme)
signed (ToSignLite v cty hs pth) acct share key = do
  xdate@(_, datev) <- xMsDate
  let
    hs' = canonicalizeHeaders (xdate : hs) -- ^ https://learn.microsoft.com/en-us/rest/api/storageservices/authorize-with-shared-key#constructing-the-canonicalized-headers-string
    dateHeader = header (BS.pack "x-ms-date") (BS.pack datev)
    res = "/" <> T.pack acct <> "/" <> T.pack share <> "/" <> pth
    appendNewline x = x <> "\n"
    t = mconcat (map appendNewline ([ v, "", cty, ""] <> hs') <> [res])
  case B64.decodeBase64 key of
    Left e -> error $ T.unpack e
    Right dkey -> do
      let
        s = H.hmac dkey (T.encodeUtf8 t)
        s64 = B64.encodeBase64 s
      pure (T.pack acct <> ":" <> s64, dateHeader)


getTest0 :: String -> IO BsResponse
getTest0 k = do
  let
    tsl = ToSignLite "GET" "" [("x-ms-version", "2014-02-14")] "aior/README.md"
    acct = "weuflowsightsa"
    share = "irisity-april4-2023-delivery"
    resource = tslPath tsl
  (s, dateHeader) <- first T.encodeUtf8 <$> signed tsl acct share (BS.pack k)
  let
    host = T.pack ("https://" <> acct <> ".file.core.windows.net/" <> share) <> "/" <> resource
    headers = sklAuthHeader s <>
              header "x-ms-version" "2014-02-14" <>
              -- header "Content-Type" "text/plain; charset=UTF-8" <>
              dateHeader
    um = useHttpsURI =<< mkURI host
  -- putStrLn $ unwords ["Auth header:", BS.unpack s]
  case um of
    Just (u, _) ->
      runReq defaultHttpConfig $ req GET u NoReqBody bsResponse headers
    Nothing -> error $ unwords ["cannot decode", T.unpack host, "as an URI"]

-- putTest0 k = do


sklAuthHeader :: BS.ByteString -> Option scheme
sklAuthHeader token =
  customAuth
    (pure . attachHeader "Authorization" ("SharedKeyLite " <> token))



-- | Shared Key Lite authentication for Storage (Blob, Queue and File services)
--- https://learn.microsoft.com/en-us/rest/api/storageservices/authorize-with-shared-key#blob-queue-and-file-services-shared-key-lite-authorization

-- StringToSign = VERB + "\n" +  
--                Content-MD5 + "\n" +  
--                Content-Type + "\n" +  
--                Date + "\n" +  
--                CanonicalizedHeaders +   
--                CanonicalizedResource;

-- Construct the CanonicalizedResource string in this format as follows (https://learn.microsoft.com/en-us/rest/api/storageservices/authorize-with-shared-key#shared-key-lite-and-table-service-format-for-2009-09-19-and-later):
--
-- 1.Beginning with an empty string (""), append a forward slash (/), followed by the name of the account that owns the resource being accessed.
--
-- 2.Append the resource's encoded URI path. If the request URI addresses a component of the resource, append the appropriate query string. The query string should include the question mark and the comp parameter (for example, ?comp=metadata).
canonicalizedResource :: T.Text -> T.Text -> T.Text
canonicalizedResource ownerAcct res = "/" <> ownerAcct <> "/" <> res

-- example : PUT blob into storage account "myaccount" :
--
-- PUT\n\ntext/plain; charset=UTF-8\n\nx-ms-date:Sun, 20 Sep 2009 20:36:40 GMT\nx-ms-meta-m1:v1\nx-ms-meta-m2:v2\n/myaccount/mycontainer/hello.txt
--
-- 1) utf-8 encode StringToSign
--
-- 2) HMAC-SHA256 sign with base64-decoded Storage Account key accessible from Azure portal
--
-- 3) base64 encode
--
-- (steps 1-3 in symbols:
--
-- Signature=Base64(HMAC-SHA256(UTF8(StringToSign), Base64.decode(<your_azure_storage_account_shared_key>)))
--)
--
-- 4) construct the Authorization header, and add the header to the request :
--
-- Authorization: SharedKeyLite myaccount:ctzMq410TV3wS7upTBcunJTDLEJwMAZuFPfr0mrrA08=
