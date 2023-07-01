module DotEnv (applyDotEnv) where

import Control.Monad.IO.Class (MonadIO(..))
import Data.Foldable (traverse_)
import Data.Functor (void)
import Data.List (sortOn)
import Data.Maybe (listToMaybe, fromMaybe)
import Data.Ord (Down(..))
import System.Environment (setEnv)
import qualified Text.ParserCombinators.ReadP as P (ReadP, readP_to_S, char, munch, sepBy1)

-- directory
import System.Directory (doesFileExist)

-- | Load, parse and apply a @.env@ file
--
-- NB : overwrites any preexisting env vars
--
-- NB2 : if the @.env@ file is not found the program continues (i.e. this function is a no-op in that case)
applyDotEnv :: MonadIO m =>
               Maybe FilePath -- ^ defaults to @.env@ if Nothing
            -> m ()
applyDotEnv mfp = liftIO $ do
  let
    fpath = fromMaybe ".env" mfp
  ok <- doesFileExist fpath
  if ok
    then
    do
      mp <- parseDotEnv <$> readFile fpath
      case mp of
        Just es -> setEnvs es
        Nothing -> putStrLn $ unwords ["dotenv: cannot parse", fpath]
    else
    do
      putStrLn $ unwords ["dotenv:", fpath, "not found"]

setEnvs :: MonadIO m => [(String, String)] -> m ()
setEnvs = traverse_ insf
  where
    insf (k, v) = liftIO $ do
      setEnv k v
      putStrLn $ unwords ["dotenv: set", k] -- DEBUG

parseDotEnv :: String -- ^ contents of the @.env@ file
            -> Maybe [(String, String)]
parseDotEnv = parse1 keyValues

keyValues :: P.ReadP [(String, String)]
keyValues = P.sepBy1 keyValue (P.char '\n') <* P.char '\n'

keyValue :: P.ReadP (String, String)
keyValue = do
  k <- keyP
  void $ P.char '='
  v <- valueP
  pure (k, v)

keyP, valueP :: P.ReadP String
keyP = P.munch (/= '=')
valueP = P.munch (/= '\n')

-- parse :: P.ReadP b -> String -> Maybe b
-- parse p str = fst <$> (listToMaybe $ P.readP_to_S p str)

parse1 :: Foldable t => P.ReadP (t a) -> String -> Maybe (t a)
parse1 p str = fmap fst $ listToMaybe $ sortOn (Down . length . fst) (P.readP_to_S p str)
