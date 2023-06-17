{-# LANGUAGE DataKinds #-}
{-# options_ghc -Wno-unused-imports #-}
-- | Common functions for the MS Graph API v1.0
--
-- https://learn.microsoft.com/en-us/graph/api/overview?view=graph-rest-1.0&preserve-view=true
module MSGraphAPI.Internal.Common (
  -- * GET
  get
  , getLbs
  -- * POST
  , post
  -- * running requests
  , runReq
  -- * JSON : aeson helpers
  , Collection(..)
  , aesonOptions
  ) where

import Control.Monad.IO.Class (MonadIO(..))
import Data.Proxy (Proxy)
import GHC.Generics (Generic(..))

import Data.List (sort, sortBy, stripPrefix, uncons)
import Data.Maybe (listToMaybe, fromMaybe)
-- import Data.Ord (comparing)
import Data.Char (toLower)

-- aeson
import qualified Data.Aeson as A (ToJSON(..), FromJSON(..), genericParseJSON, defaultOptions, Options(..), withObject, withText, (.:), (.:?), object, (.=), Key, Value, camelTo2)
-- bytestring
import qualified Data.ByteString as BS (ByteString)
import qualified Data.ByteString.Char8 as BS8 (pack, unpack)
import qualified Data.ByteString.Lazy as LBS (ByteString)
import qualified Data.ByteString.Lazy.Char8 as LBS8 (pack, unpack, putStrLn)
-- hoauth2
import Network.OAuth.OAuth2 (OAuth2Token(..))
import Network.OAuth.OAuth2.Internal (AccessToken(..), ExchangeToken(..), RefreshToken(..), OAuth2Error, IdToken(..))
-- modern-uri
import Text.URI (URI, mkURI)
-- req
import Network.HTTP.Req (Req, runReq, defaultHttpConfig, req, Option, (=:), GET(..), POST(..), Url, Scheme(..), useHttpsURI, https, (/:), ReqBodyJson(..), NoReqBody(..), oAuth2Bearer, HttpResponse(..), jsonResponse, JsonResponse, lbsResponse, LbsResponse, bsResponse, BsResponse, responseBody)
-- text
import Data.Text (Text, pack, unpack)

import Network.OAuth2.Session (Tokens)


-- -- GET, POST 

-- | @POST https:\/\/graph.microsoft.com\/v1.0\/...@
post :: (A.FromJSON b, A.ToJSON a) =>
        [Text] -- ^ URI path segments
     -> Option 'Https
     -> a -- ^ request body
     -> AccessToken
     -> Req b
post paths params bdy tok = responseBody <$> req POST url (ReqBodyJson bdy) jsonResponse opts
  where
    opts = auth <> params
    (url, auth) = msGraphReqConfig tok paths

-- | @GET https:\/\/graph.microsoft.com\/v1.0\/...@
get :: A.FromJSON a =>
       [Text] -- ^ URI path segments
    -> Option 'Https
    -> AccessToken
    -> Req a
get paths params tok = responseBody <$> req GET url NoReqBody jsonResponse opts
  where
    opts = auth <> params
    (url, auth) = msGraphReqConfig tok paths

-- | @GET https:\/\/graph.microsoft.com\/v1.0\/...@
--
-- Returns the response body as a bytestring, e.g. for endpoints that download files or general bytestring payloads
getLbs :: [Text] -- ^ URI path segments
       -> Option 'Https
       -> AccessToken -> Req LBS.ByteString
getLbs paths params tok = responseBody <$> req GET url NoReqBody lbsResponse opts
  where
    opts = auth <> params
    (url, auth) = msGraphReqConfig tok paths

msGraphReqConfig :: AccessToken -> [Text] -> (Url 'Https, Option 'Https)
msGraphReqConfig (AccessToken ttok) uriRest = (url, os)
  where
    url = (https "graph.microsoft.com" /: "v1.0") //: uriRest
    os = oAuth2Bearer $ BS8.pack (unpack ttok)

(//:) :: Url scheme -> [Text] -> Url scheme
(//:) = foldl (/:)


-- * aeson

-- | a collection of items with key @value@
data Collection a = Collection {
  cValue :: [a]
                               } deriving (Eq, Show, Generic)
instance A.FromJSON a => A.FromJSON (Collection a) where
  parseJSON = A.genericParseJSON (aesonOptions "c")

-- | drop the prefix and lowercase first character
--
-- e.g. @userDisplayName@ @->@ @displayName@
aesonOptions :: String -- ^ record prefix
             -> A.Options
aesonOptions pfx = A.defaultOptions { A.fieldLabelModifier = recordName pfx }

-- | drop the prefix and lowercase first character
recordName :: String -- ^ record name prefix
           -> String -- ^ JSON field name
           -> String
recordName pf str = case uncons $ dropPrefix pf str of
  Just (c, cs) -> toLower c : cs
  _ -> error "record name cannot be empty"

-- | Drops the given prefix from a list.
--   It returns the original sequence if the sequence doesn't start with the given prefix.
--
-- > dropPrefix "Mr. " "Mr. Men" == "Men"
-- > dropPrefix "Mr. " "Dr. Men" == "Dr. Men"
dropPrefix :: Eq a => [a] -> [a] -> [a]
dropPrefix a b = fromMaybe b $ stripPrefix a b