{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings, RankNTypes #-}

module Scraper (
         Url
       , openURL
       , convertEncoding
       , writeFileFromUrl
       , parseTagsFromFile
       , hrefs
       ) where

import Control.Applicative
import Network.HTTP
import Codec.Text.IConv
import Data.ByteString.Lazy.Char8 (pack, unpack)
import Codec.Binary.UTF8.String
import Text.HTML.TagSoup

type Url = String

openURL :: Url -> IO String
openURL url = simpleHTTP (getRequest url) >>= getResponseBody

convertEncoding :: EncodingName -> EncodingName -> String -> String
convertEncoding fromEnc toEnc = decodeString . unpack . convert fromEnc toEnc . pack

writeFileFromUrl :: Url -> FilePath
                 -> EncodingName -> EncodingName -> IO ()
writeFileFromUrl url fileName fromEnc toEnc = do
  page <- openURL url
  writeFile fileName $ convertEncoding fromEnc toEnc page

parseTagsFromFile :: FilePath -> IO [Tag String]
parseTagsFromFile fileName = do
  page <- readFile fileName
  return $ parseTags page

-- hrefs :: forall a. (Eq a, Data.String.IsString a) => [Tag a] -> [a]
hrefs [] = []
hrefs ((TagOpen "a" [("href", url)]) : ts) = url : hrefs ts
hrefs ((TagOpen "a" [("href", url), _]) : ts) = url : hrefs ts
hrefs (_:ts) = hrefs ts

-- main :: IO ()
-- main = do
--      tags <- readFile "archive.html"
--      return $ archiveDateLinks tags
--   page <- openURL "http://d.hatena.ne.jp/kazu-yamamoto/archive"
--   writeFile "archive.html" $ convertEncoding "EUC-JP" "UTF-8" page
--   page <- readFile "archive.html"
--   return $ parseTags page

