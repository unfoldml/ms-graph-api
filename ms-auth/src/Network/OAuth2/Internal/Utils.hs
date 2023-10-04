module Network.OAuth2.Internal.Utils where

import Data.Bifunctor
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8  as BS8
import Data.Map.Strict (Map)
import qualified Data.Map.Strict  as Map
import qualified Data.Text.Encoding  as TE
import qualified Data.Text.Lazy  as TL

tlToBS :: TL.Text -> ByteString
tlToBS = TE.encodeUtf8 . TL.toStrict

bs8ToLazyText :: BS8.ByteString -> TL.Text
bs8ToLazyText = TL.pack . BS8.unpack

mapsToParams :: [Map TL.Text TL.Text] -> [(ByteString, ByteString)]
mapsToParams =
  map (bimap tlToBS tlToBS)
    . Map.toList
    . Map.unions
