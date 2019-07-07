module Scraper where

import Data.List (isPrefixOf)
import Data.Char (chr, isDigit)
import Text.Regex.Posix ((=~), getAllTextMatches)

data Comic = Comic { num         :: Int
                   , title       :: String
                   , images      :: [URL]
                   , altText     :: Maybe String
                   -- , explanation :: Maybe String
                   } deriving Show

type URL = String
type Page = String

extractImageURLs :: Page -> [URL]
extractImageURLs = map ("http:" ++ )
                   . tail
                   . getAllTextMatches
                   . (=~ "//static.existentialcomics.com/comics/.+\\.(jpe?g|png)")

replaceCodes :: String -> String
replaceCodes "" = ""
replaceCodes s  = if ("&#" `isPrefixOf` s)
                  then (chr . read . takeWhile isDigit . drop 2) s :
                  (replaceCodes . drop 1 . dropWhile isDigit . drop 2) s
                  else head s : replaceCodes (tail s)

extractAltText :: Page -> Maybe String
extractAltText s = let match = s =~ "<div class=\"altText\">\\s*?(.*)\\s*?</div>"
                   in if null match
                      then Nothing
                      else Just $ replaceCodes $ (!! 1) $ head match

extractTitle :: Page -> String
extractTitle = replaceCodes
               . (!! 1)
               . head
               . (=~ "<h3>(.+)</h3>")

extractNum :: Page -> Int
extractNum = read . drop 52 . (=~ "og:url.*([0-9]+)")

{-
extractExplanation :: Page -> Maybe String
extractExplanation s = let match = s =~ "<div id=\"explanation\">\\s*?(.*)\\s*?</div>"
                       in if null match
                          then Nothing
                          else Just $ replaceCodes $ (!! 1) $ head match
                              -} -- TODO

toComic :: Page -> Comic
toComic = Comic
          <$> extractNum
          <*> extractTitle
          <*> extractImageURLs
          <*> extractAltText
          -- <*> extractExplanation
