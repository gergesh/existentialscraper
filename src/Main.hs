module Main where

import Data.Char (toLower)
import Data.Set (fromAscList, notMember)
import qualified Data.ByteString as B (ByteString)
import Control.Monad ((>=>))
import Control.Concurrent.Async (mapConcurrently)
import Text.Printf (printf)
import System.FilePath.Posix (takeExtension, makeValid)
import System.Directory (listDirectory)
import Data.Maybe (catMaybes)
import Text.Read (readMaybe)
import Networking
import Scraper
import Image

lastComic :: IO Int
lastComic = fmap extractNum $ get "http://existentialcomics.com"

getComic :: Int -> IO Comic
getComic = fmap toComic . get . ("http://existentialcomics.com/comic/" ++) . show

nameComic :: Comic -> String
nameComic c = makeValid
              $ printf "%03d_%s%s" (num c)
                                   (map (toLower . illegalToUnderscore) $ title c)
                                   (takeExtension $ head $ images c)
  where illegalToUnderscore = \c -> if c `elem` " :" then '_' else c

saveComic :: Comic -> IO ()
saveComic c = do
    imgStrings <- mapConcurrently getBin (images c)
    let imgArrs = map (readBS (takeExtension $ head $ images c)) imgStrings
        merged  = concatImages imgArrs
    writeImage (nameComic c) merged

main = do
    archived <- fmap (fromAscList . catMaybes . map ((readMaybe :: String -> Maybe Int) . take 3)) $ listDirectory "."
    lastNum <- lastComic
    mapM_ (getComic >=> saveComic) $ filter (`notMember` archived) [1 .. lastNum]
