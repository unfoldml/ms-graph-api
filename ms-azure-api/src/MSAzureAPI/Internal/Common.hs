{-# LANGUAGE DataKinds #-}
{-# language QuasiQuotes #-}
{-# options_ghc -Wno-unused-imports #-}
-- | Common functions for the MS Azure API
--
module MSAzureAPI.Internal.Common (
  APIPlane(..)
  -- ** PUT
  , put
  -- ** GET
  , get
  , getBs
  , getLbs
  -- ** POST
  , post
  , postRaw
  , postSBMessage
  -- ** DELETE
  , delete
  -- * HTTP(S) connections
  , run
  , withTLS
  -- ** URL parameters
  , (==:)
  -- ** Helpers
  , tryReq
  -- ** Common types
  , Collection
  , collectionValue
  , collectionNextLink
  -- *** Location
  , Location(..)
  , showLocation
  , locationDisplayName
  -- ** JSON co\/dec
  , aesonOptions
  ) where

import Control.Monad.IO.Class (MonadIO(..))
import Data.Proxy (Proxy)
import GHC.Generics (Generic(..))

import Data.List (sort, sortBy, stripPrefix, uncons)
import Data.Maybe (listToMaybe, fromJust, fromMaybe)
-- import Data.Ord (comparing)
import Data.Char (toLower)

-- aeson
import qualified Data.Aeson as A (ToJSON(..), encode, FromJSON(..), genericParseJSON, defaultOptions, Options(..), withObject, withText, (.:), (.:?), object, (.=), Key, Value(..), camelTo2)
-- bytestring
import qualified Data.ByteString as BS (ByteString)
import qualified Data.ByteString.Char8 as BS8 (pack, unpack)
import qualified Data.ByteString.Lazy as LBS (ByteString)
import qualified Data.ByteString.Lazy.Char8 as LBS8 (pack, unpack, putStrLn)
-- http-client
import qualified Network.HTTP.Client as L (RequestBody(..))
-- http-client-tls
import Network.HTTP.Client.TLS (newTlsManager)
-- hoauth2
import Network.OAuth.OAuth2 (OAuth2Token(..))
import Network.OAuth.OAuth2.Internal (AccessToken(..), ExchangeToken(..), RefreshToken(..), OAuth2Error, IdToken(..))
-- modern-uri
import Text.URI (URI, mkURI)
-- req
import Network.HTTP.Req (Req, runReq, HttpBody(..), HttpConfig(..), HttpException(..), defaultHttpConfig, req, Option, (=:), GET(..), POST(..), PUT(..), DELETE(..), Url, Scheme(..), urlQ, useHttpsURI, https, (/:), ReqBodyJson(..), NoReqBody(..), oAuth2Bearer, HttpResponse(..), jsonResponse, JsonResponse, lbsResponse, LbsResponse, bsResponse, BsResponse, responseBody)
-- text
import Data.Text (Text, pack, unpack)
-- unliftio
import UnliftIO (MonadUnliftIO(..))
import UnliftIO.Exception (try)

-- | URL parameters
(==:) :: Text -- ^ key
      -> Text -- ^ value
      -> Option 'Https
(==:) = (=:)

-- | @GET@ a 'LBS.ByteString' e.g. a file
getLbs :: APIPlane
       -> [Text] -- ^ URI path segments
       -> Option 'Https -> AccessToken -> Req LBS.ByteString
getLbs apiplane paths params tok = responseBody <$> req GET url NoReqBody lbsResponse opts
  where
    opts = auth <> params
    (url, auth) = msAzureReqConfig apiplane paths tok

-- | @GET@ a 'BS.ByteString' e.g. a file
getBs :: APIPlane
      -> [Text] -- ^ URI path segments
      -> Option 'Https -> AccessToken -> Req BS.ByteString
getBs apiplane paths params tok = responseBody <$> req GET url NoReqBody bsResponse opts
  where
    opts = auth <> params
    (url, auth) = msAzureReqConfig apiplane paths tok

-- | Create a new TLS manager, which should be reused throughout the program
withTLS :: MonadIO m =>
           (HttpConfig -> m b) -- ^ user program
        -> m b
withTLS act = do
  mgr <- newTlsManager
  let
    hc = defaultHttpConfig { httpConfigAltManager = Just mgr }
  act hc

-- | Run a 'Req' computation
run :: MonadIO m =>
       HttpConfig -> Req a -> m (Either HttpException a)
run hc = runReq hc . tryReq


-- | Specialized version of 'try' to 'HttpException's
--
-- This can be used to catch exceptions of composite 'Req' statements, e.g. around a @do@ block
tryReq :: Req a -> Req (Either HttpException a)
tryReq = try

-- | API control planes
--
-- https://learn.microsoft.com/en-us/azure/azure-resource-manager/management/control-plane-and-data-plane
data APIPlane = APManagement -- ^ Management plane (@management.azure.com@ endpoints)
              | APData Text -- ^ Data plane e.g. FileREST API
              | APServiceBus Text -- ^ Data plane for Service Bus. The parameter is the service name

-- | @PUT@
put :: (A.FromJSON b, A.ToJSON a) =>
       APIPlane
    -> [Text] -- ^ URI path segments
    -> Option 'Https -- ^ request parameters etc.
    -> a -> AccessToken -> Req b
put apiplane paths params bdy tok = responseBody <$> req PUT url (ReqBodyJson bdy) jsonResponse opts
  where
    opts = auth <> params
    (url, auth) = msAzureReqConfig apiplane paths tok

-- | @DELETE@
delete :: (A.FromJSON b, A.ToJSON a) =>
          APIPlane -> [Text] -> Option 'Https -> a -> AccessToken -> Req b
delete apiplane paths params bdy tok = responseBody <$> req DELETE url (ReqBodyJson bdy) jsonResponse opts
  where
    opts = auth <> params
    (url, auth) = msAzureReqConfig apiplane paths tok

-- | @POST@
post :: (A.FromJSON b, A.ToJSON a) =>
        APIPlane
     -> [Text] -- ^ URI path segments
     -> Option 'Https -- ^ request parameters etc.
     -> a -- ^ request body
     -> AccessToken -> Req b
post apiplane paths params bdy tok = responseBody <$> req POST url (ReqBodyJson bdy) jsonResponse opts
  where
    opts = auth <> params
    (url, auth) = msAzureReqConfig apiplane paths tok

-- | @POST@ to a URL
--
-- useful when the base URL is dynamic e.g. comes from an external service
postRaw :: (A.FromJSON b, A.ToJSON a) =>
           Text -- ^ base URL (can contain path and parameters too)
        -> [Text] -- ^ additional URI path segments
        -> Option 'Https
        -> a -> AccessToken -> Req b
postRaw uraw paths params bdy atok = do
  uriBase <- mkURI uraw
  let
    auth = bearerAuth atok
    (u, uparams) = fromJust (useHttpsURI uriBase)
    url = u //: paths
    opts = auth <> params <> uparams -- NB identical keys are not overwritten
  responseBody <$> req POST url (ReqBodyJson bdy) jsonResponse opts




-- | Post a message or batch thereof to the Service Bus
--
-- see example : https://learn.microsoft.com/en-us/rest/api/servicebus/send-message-batch#example
postSBMessage :: (A.FromJSON b, A.ToJSON a) =>
                 Text
              -> [Text]
              -> Option 'Https -> a -> AccessToken -> Req b
postSBMessage servName paths params bdy tok = responseBody <$> req POST url (ReqBodyServiceBusMessage bdy) jsonResponse opts
  where
    opts = auth <> params
    (url, auth) = msAzureReqConfig (APServiceBus servName) paths tok

data ReqBodyServiceBusMessage a = ReqBodyServiceBusMessage a
instance A.ToJSON a => HttpBody (ReqBodyServiceBusMessage a) where
  getRequestBody (ReqBodyServiceBusMessage a) = L.RequestBodyLBS (A.encode a)
  getRequestContentType _ = Just "application/vnd.microsoft.servicebus.json"

-- | @GET@
get :: (A.FromJSON b) =>
       APIPlane
    -> [Text] -- ^ URI path segments
    -> Option 'Https -> AccessToken -> Req b
get apiplane paths params tok = responseBody <$> req GET url NoReqBody jsonResponse opts
  where
    opts = auth <> params
    (url, auth) = msAzureReqConfig apiplane paths tok

msAzureReqConfig :: APIPlane
                 -> [Text] -- ^ URI path segments
                 -> AccessToken
                 -> (Url 'Https, Option 'Https)
msAzureReqConfig apiplane uriRest atok = (url, os)
  where
    url = apiPlaneBaseURL apiplane uriRest
    os = bearerAuth atok

apiPlaneBaseURL :: APIPlane
                -> [Text] -- ^ URI path segments
                -> Url 'Https
apiPlaneBaseURL apiplane uriRest = (https urlBase) //: uriRest
  where
    urlBase = case apiplane of
      APManagement -> "management.azure.com"
      APData ub -> ub
      APServiceBus sn -> sn <> ".servicebus.windows.net"

bearerAuth :: AccessToken -> Option 'Https
bearerAuth (AccessToken ttok) = oAuth2Bearer $ BS8.pack (unpack ttok)


(//:) :: Url scheme -> [Text] -> Url scheme
(//:) = foldl (/:)


-- * common types

-- | Displays the short name, e.g. "westeu"
showLocation :: Location -> Text
showLocation = pack . show

-- | Azure regions
data Location =
  LNorthEU -- ^ "North Europe"
  | LWestEU -- ^ "West Europe"
  deriving (Eq)
instance Show Location where
  show = \case
    LNorthEU -> "northeu"
    LWestEU -> "westeu"
-- | Renders the full name via 'locationDisplayName'
instance A.ToJSON Location where
  toJSON = A.String . locationDisplayName

-- | Displays the full name, e.g. "West Europe"
locationDisplayName :: Location -> Text
locationDisplayName = \case
  LNorthEU -> "North Europe"
  LWestEU -> "West Europe"

-- | a collection of items with key @value@
--
-- NB : results are paginated, and subsequent chunks can be accessed by following the @nextLink@ field
data Collection a = Collection {
  cValue :: [a]
  , cNextLink :: Maybe Text -- ^ The URI to fetch the next page of results
                               } deriving (Eq, Show, Generic)
instance A.FromJSON a => A.FromJSON (Collection a) where
  parseJSON = A.genericParseJSON (aesonOptions "c")
-- | Get the collection items
collectionValue :: Collection a -> [a]
collectionValue = cValue
-- | Get the next link for a 'Collection' of paginated results
collectionNextLink :: Collection a -> Maybe Text
collectionNextLink = cNextLink

-- * aeson

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
