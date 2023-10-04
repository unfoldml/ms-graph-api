module Network.OAuth2.Internal.Pkce
  ( mkPkceParam,
    CodeChallenge (..),
    CodeVerifier (..),
    CodeChallengeMethod (..),
    PkceRequestParam (..),
  )
where

import Control.Monad.IO.Class
import qualified Crypto.Hash.SHA256  as H
import qualified Data.ByteString  as BS
import qualified Data.ByteString.Base64.URL  as B64
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import Data.Word
import System.Entropy (getEntropy)

newtype CodeChallenge = CodeChallenge {unCodeChallenge :: Text}

newtype CodeVerifier = CodeVerifier {unCodeVerifier :: Text} deriving (Show)

data CodeChallengeMethod = S256
  deriving (Show)

data PkceRequestParam = PkceRequestParam
  { codeVerifier :: CodeVerifier,
    codeChallenge :: CodeChallenge,
    -- | spec says optional but really it shall be s256 or can be omitted?
    -- https://datatracker.ietf.org/doc/html/rfc7636#section-4.3
    codeChallengeMethod :: CodeChallengeMethod
  }

mkPkceParam :: MonadIO m => m PkceRequestParam
mkPkceParam = do
  codeV <- genCodeVerifier
  pure
    PkceRequestParam
      { codeVerifier = CodeVerifier (T.decodeUtf8 codeV),
        codeChallenge = CodeChallenge (encodeCodeVerifier codeV),
        codeChallengeMethod = S256
      }

encodeCodeVerifier :: BS.ByteString -> Text
encodeCodeVerifier = B64.encodeBase64Unpadded . hashSHA256

genCodeVerifier :: MonadIO m => m BS.ByteString
genCodeVerifier = liftIO $ getBytesInternal BS.empty

cvMaxLen :: Int
cvMaxLen = 128

-- The default 'getRandomBytes' generates bytes out of unreverved characters scope.
-- code-verifier = 43*128unreserved
--   unreserved = ALPHA / DIGIT / "-" / "." / "_" / "~"
--   ALPHA = %x41-5A / %x61-7A
--   DIGIT = %x30-39
getBytesInternal :: BS.ByteString -> IO BS.ByteString
getBytesInternal ba
  | BS.length ba >= cvMaxLen = pure (BS.take cvMaxLen ba)
  | otherwise = do
      bs <- getEntropy cvMaxLen
      let bsUnreserved = ba `BS.append` BS.filter isUnreversed bs
      getBytesInternal bsUnreserved

hashSHA256 :: BS.ByteString -> BS.ByteString
hashSHA256 = H.hash

isUnreversed :: Word8 -> Bool
isUnreversed w = w `BS.elem` unreverseBS

{-
a-z: 97-122
A-Z: 65-90
-: 45
.: 46
_: 95
~: 126
-}
unreverseBS :: BS.ByteString
unreverseBS = BS.pack $ [97 .. 122] ++ [65 .. 90] ++ [45, 46, 95, 126]
