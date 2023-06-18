{-# Language DeriveFunctor #-}
{-# language DeriveGeneric #-}
{-# language DerivingStrategies #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# language OverloadedStrings #-}
module Network.OAuth2.JWT where

import Control.Monad.IO.Class (MonadIO(..))
import qualified Data.List.NonEmpty as NE (NonEmpty(..))
import GHC.Exception (Exception(..))
import GHC.Generics (Generic)
import Data.Maybe (fromMaybe)
import Data.String (IsString(..))
import Data.Typeable

-- aeson
import qualified Data.Aeson as A (FromJSON(..), ToJSON(..), ToJSONKey(..), FromJSON(..), FromJSONKey(..), Value(..))
-- containers
import qualified Data.Map.Strict as M (Map, lookup)
-- jwt
import qualified Web.JWT as J (decode, claims, JWTClaimsSet(..), StringOrURI, NumericDate, ClaimsMap(..))
-- scientific
import Data.Scientific (coefficient)
-- text
import qualified Data.Text as T (Text, unpack)
-- time
import Data.Time (UTCTime(..), NominalDiffTime, getCurrentTime, fromGregorian, addUTCTime, diffUTCTime)
-- validation-micro
import Validation.Micro (Validation(..), failure, validationToEither, maybeToSuccess)


-- | 'sub' field
newtype UserSub = UserSub { userSub :: T.Text }
  deriving (Eq, Ord, Generic, IsString)
  deriving newtype (Show, A.ToJSON, A.FromJSON, A.ToJSONKey, A.FromJSONKey)

newtype UserEmail = UserEmail { userEmail :: T.Text }
  deriving (Eq, Ord, Generic, IsString)
  deriving newtype (Show, A.ToJSON, A.FromJSON, A.ToJSONKey, A.FromJSONKey)

-- | intended audience of the token (== API key ID )
newtype ApiAudience = ApiAudience { apiAudience :: T.Text } deriving (Eq, Ord, Show, Generic, Typeable, IsString)
instance A.ToJSON ApiAudience

jwtClaims :: T.Text -> Maybe J.JWTClaimsSet
jwtClaims t = J.claims <$> J.decode t

-- | decoded claims from the JWT token, valid (at least) for the Google OpenID implementation as of February 2021
--
data JWTClaims =
  JWTClaims {
    jcAud :: T.Text -- "aud"ience field
  , jcExp :: UTCTime -- "exp"iry date
  , jcIat :: UTCTime -- Issued AT
  , jcNbf :: UTCTime -- Not BeFore
  , jcSub :: UserSub -- "sub"ject
  , jcEmail :: UserEmail
            } deriving (Eq, Show)

decValidSub :: J.JWTClaimsSet -> Validation (NE.NonEmpty JWTException) UserSub
decValidSub jc = decSub (J.sub jc)

decValidExp :: Maybe NominalDiffTime
            -> UTCTime
            -> J.JWTClaimsSet
            -> Validation (NE.NonEmpty JWTException) UTCTime
decValidExp nsecs t jc = decExp (J.exp jc) `bindValidation` validateExp nsecs t

decValidNbf :: UTCTime -> J.JWTClaimsSet -> Validation (NE.NonEmpty JWTException) UTCTime
decValidNbf t jc = decNbf (J.nbf jc) `bindValidation` validateNbf t

decValidEmail :: J.JWTClaimsSet -> Validation (NE.NonEmpty JWTException) UserEmail
decValidEmail jc = decEmail (J.unClaimsMap $ J.unregisteredClaims jc)

decValidAud :: ApiAudience -> J.JWTClaimsSet -> Validation (NE.NonEmpty JWTException) T.Text
decValidAud a jc = decAud (J.aud jc) `bindValidation` validateAud a

-- | NB Validation is not a monad though
bindValidation :: Validation e a -> (a -> Validation e b) -> Validation e b
bindValidation v f = case v of
  Failure e -> Failure e
  Success a -> f a


-- | Decode and validate the 'aud', 'exp' and 'nbf' fields of the JWT
decodeValidateJWT :: MonadIO f =>
                     ApiAudience -- ^ intended token audience (its meaning depends on the OAuth identity provider )
                  -> Maybe NominalDiffTime -- ^ buffer period to allow for API roundtrip delays (defaults to 0 if Nothing)
                  -> T.Text -- ^ JWT-encoded string, e.g. the contents of the id_token field
                  -> f (Either (NE.NonEmpty JWTException) JWTClaims)
decodeValidateJWT iaud nsecs jstr = case validationToEither $ decodeJWT jstr of
  Right jwc -> validationToEither <$> validateJWT iaud nsecs jwc
  Left e -> pure $ Left e


-- | Validate the 'aud', 'exp' and 'nbf' fields
validateJWT :: MonadIO m =>
               ApiAudience -- ^ intended token audience (its meaning depends on the OAuth identity provider )
            -> Maybe NominalDiffTime
            -> JWTClaims
            -> m (Validation (NE.NonEmpty JWTException) JWTClaims)
validateJWT a nsecs j = do
  t <- liftIO getCurrentTime
  pure (
    JWTClaims <$>
      validateAud a (jcAud j) <*>
      validateExp nsecs t (jcExp j) <*>
      pure (jcIat j) <*>
      validateNbf t (jcNbf j) <*>
      pure (jcSub j) <*>
      pure (jcEmail j)
       )

-- | Fails if the 'exp'iry field is not at least 'nsecs' seconds in the future
validateExp :: Maybe NominalDiffTime -- ^ defaults to 0 if Nothing
            -> UTCTime -> UTCTime -> Validation (NE.NonEmpty JWTException) UTCTime
validateExp nsecs t texp = do
  if fromMaybe 0 nsecs `addUTCTime` texp > t then
    Success texp
    else failure (JEExpiredToken texp)


-- | Fails if the current time is before the 'nbf' time (= token is not yet valid)
validateNbf :: UTCTime -> UTCTime -> Validation (NE.NonEmpty JWTException) UTCTime
validateNbf t tnbf = do
  if t `diffUTCTime` tnbf > 0 then
    Success tnbf
    else failure (JENotYetValid tnbf)

-- | Fails if the 'aud'ience field is not equal to the supplied ApiAudience
validateAud :: ApiAudience -- ^ intended audience of the token (== API key ID )
            -> T.Text -- ^ decoded from the JWT
            -> Validation (NE.NonEmpty JWTException) T.Text
validateAud aa@(ApiAudience a) audt
  | a == audt = Success audt
  | otherwise = failure $ JEAudienceNotFound aa

decodeJWT :: T.Text
          -> Validation (NE.NonEmpty JWTException) JWTClaims
decodeJWT jwts = case J.claims <$> J.decode jwts of
  Nothing -> failure $ JEMalformedJWT jwts
  Just (J.JWTClaimsSet _ subm audm expm nbfm iatm _ (J.ClaimsMap cms)) ->
    JWTClaims <$>
      decAud audm <*>
      decExp expm <*>
      decIat iatm <*>
      decNbf nbfm <*>
      decSub subm <*>
      decEmail cms

decAud :: Maybe (Either J.StringOrURI [J.StringOrURI])
       -> Validation (NE.NonEmpty JWTException) T.Text
decAud aam = claimNotFound "aud" (fromAud aam)

decExp :: Maybe J.NumericDate -> Validation (NE.NonEmpty JWTException) UTCTime
decExp em = claimNotFound "exp" (fromNumericDate em)

decIat :: Maybe J.NumericDate -> Validation (NE.NonEmpty JWTException) UTCTime
decIat im = claimNotFound "iat" (fromNumericDate im)

decNbf :: Maybe J.NumericDate
       -> Validation (NE.NonEmpty JWTException) UTCTime
decNbf im = claimNotFound "nbf" (fromNumericDate im)

decSub :: A.ToJSON a =>
          Maybe a -> Validation (NE.NonEmpty JWTException) UserSub
decSub sm = claimNotFound "sub" (UserSub <$> fromStringOrUri sm)

decEmail :: (Ord k, IsString k) =>
            M.Map k A.Value -> Validation (NE.NonEmpty JWTException) UserEmail
decEmail cms = claimNotFound "email" (case M.lookup "email" cms of
                                           Just (A.String ems) -> Just $ UserEmail ems
                                           _ -> Nothing)

claimNotFound :: String -> Maybe a -> Validation (NE.NonEmpty JWTException) a
claimNotFound c = maybeToSuccess (JEClaimNotFound c NE.:| [])

-- | Possible exception states of authentication request
data JWTException = JEMalformedJWT T.Text
                   | JEClaimNotFound String
                   | JEAudienceNotFound ApiAudience
                   | JEExpiredToken UTCTime
                   | JENotYetValid UTCTime
                   | JENoToken
                   deriving (Eq, Ord, Generic, Typeable)

instance Show JWTException where
  show = \case
    JEMalformedJWT jt -> unwords ["Cannot decode JWT token :", T.unpack jt]
    JEClaimNotFound c -> unwords ["JWT claim not found :", c]
    JEAudienceNotFound a -> unwords ["audience", show a, "not found"]
    JEExpiredToken t -> unwords ["JWT token expired on", show t]
    JENotYetValid t -> unwords ["JWT token not yet valid:", show t]
    JENoToken -> "No token found"
instance Exception JWTException
instance A.ToJSON JWTException

fromAud :: Maybe (Either J.StringOrURI [J.StringOrURI]) -> Maybe T.Text
fromAud mm = maybe Nothing f mm
  where
    g str = case A.toJSON str of
      A.String s -> Just s
      _ -> Nothing
    f = \case
      Left sou -> g sou
      Right sous -> mconcat <$> traverse g sous

fromNumericDate :: Maybe J.NumericDate -> Maybe UTCTime
fromNumericDate tjm = case A.toJSON tjm of
  A.Number x -> Just $ addUTCTime (fromIntegral $ coefficient x) epoch
  _ -> Nothing

fromStringOrUri :: (A.ToJSON a) => Maybe a -> Maybe T.Text
fromStringOrUri sm = case A.toJSON sm of
  A.String t -> Just t
  _ -> Nothing

epoch :: UTCTime
epoch = UTCTime (fromGregorian 1970 1 1) 0

