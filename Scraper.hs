{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings, RankNTypes #-}

module Scraper (
         Url
       , openURL
       , convertEncoding
       , writeFileFromUrl
       , parseTagsFromFile
       , hrefs
       , subTree
       ) where

import Control.Applicative
import Network.HTTP
import Codec.Text.IConv
import Data.ByteString.Lazy.Char8 (pack, unpack)
import Codec.Binary.UTF8.String
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Tree
import Debug.Trace

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

-- subTree :: Eq t => (t, Maybe [Attribute t]) -> [TagTree t] -> [TagTree t]
subTree _ _ [] = []
subTree (Just tagStr) Nothing (t:ts) = subTreeByTag tagStr (t:ts)
  where
    subTreeByTag _ [] = []
    subTreeByTag tagStr ((TagBranch tagStr' attrs' subT):ts)
      | tagStr == tagStr' = subT ++ subTreeByTag tagStr ts
      | otherwise = subTreeByTag tagStr subT ++ subTreeByTag tagStr ts
    subTreeByTag tagStr ((TagLeaf t'):ts) = subTreeByTag tagStr ts
subTree Nothing (Just attrs) (t:ts) = subTreeByAttrs attrs (t:ts)
  where
    subTreeByAttrs _ [] = []
    subTreeByAttrs attr ((TagBranch tagStr' attrs' subT):ts)
      | attrs == attrs' = subT ++ subTreeByAttrs attrs ts
      | otherwise = subTreeByAttrs attrs subT ++ subTreeByAttrs attrs ts
    subTreeByAttrs attrs ((TagLeaf t'):ts) = subTreeByAttrs attrs ts
subTree (Just tagStr) (Just attrs) (t:ts) = subTreeByTagAttrs tagStr attrs (t:ts)
  where
    subTreeByAttrs _ _ [] = []
    subTreeByTagAttrs tagStr attrs ((TagBranch tagStr' attrs' subT):ts)
      | (tagStr, attrs) == (tagStr', attrs') = subT ++ subTreeByTagAttrs tagStr attrs ts
      | otherwise = subTreeByTagAttrs tagStr attrs subT ++ subTreeByTagAttrs tagStr attrs ts
    subTreeByTagAttrs tagStr attrs ((TagLeaf t'):ts) = subTreeByTagAttrs tagStr attrs ts

-- subTree (tagStr, Nothing) ((TagBranch tagStr' attrs' subT):ts)
--   | tagStr == tagStr' = subT ++ subTree (tagStr, Nothing) ts
--   | otherwise = subTree (tagStr, Nothing) subT ++ subTree (tagStr, Nothing) ts
-- subTree (tagStr, Nothing) ((TagLeaf _):ts) = subTree (tagStr, Nothing) ts
-- subTree (tagStr, Just attrs) ((TagBranch tagStr' attrs' subT):ts)
--   | (tagStr, attrs) == (tagStr', attrs') = subT ++ subTree (tagStr, Just attrs) ts
--   | otherwise = subTree (tagStr, Just attrs) subT ++ subTree (tagStr, Just attrs) ts
-- subTree (tagStr, Just attrs) ((TagLeaf _):ts) = subTree (tagStr, Just attrs) ts
