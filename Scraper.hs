{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings, RankNTypes #-}

module Scraper (
         Url
       , openURL
       , convertEncoding
       , writeFileFromUrl
       , contentFromUrl
       , parseTagsFromFile
       , subTree
       , links
       ) where

-- import Control.Applicative
import Network.HTTP
import Codec.Text.IConv
import Data.ByteString.Lazy.Char8 (pack, unpack)
import Codec.Binary.UTF8.String
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Tree
-- import Debug.Trace

type Url = String

openURL :: Url -> IO String
openURL url = simpleHTTP (getRequest url) >>= getResponseBody

convertEncoding :: EncodingName -> EncodingName -> String -> String
convertEncoding fromEnc toEnc = decodeString . unpack . convert fromEnc toEnc . pack

writeFileFromUrl :: Url -> FilePath -> EncodingName -> EncodingName -> IO ()
writeFileFromUrl url fileName fromEnc toEnc = do
  page <- openURL url
  writeFile fileName $ convertEncoding fromEnc toEnc page

-- contentFromUrl :: Url -> EncodingName -> EncodingName -> IO ()
contentFromUrl url fromEnc toEnc = do
  page <- openURL url
  return $ convertEncoding fromEnc toEnc page

parseTagsFromFile :: FilePath -> IO [Tag String]
parseTagsFromFile fileName = do
  page <- readFile fileName
  return $ parseTags page

-- | subTree
-- >>> let htmlTree = tagTree $ parseTags "<!DOCTYPE html><html><head><meta charset=\"utf-8\" /><link href=\"css/bootstrap.min.css\" rel=\"stylesheet\" /><link href=\"css/bootstrap-responsive.css\" rel=\"stylesheet\" /></head><body><div id=\"container\"><div id=\"content\"><div class=\"alert alert-success\" id=\"notification\" style=\"display:none\"> ... </div><h1><a href=\"/\">GYMMEMO</a></h1><table class=\"table\"><tr><th>日時</th><th>名前</th><th>スコア</th></tr><tr><td>hoho</td><td class=\"date\">2012-09-09</td><td>ooo</td></tr></table></div></div><script type=\"text/javascript\" src=\"js/jquery.js\"></script><script type=\"text/javascript\" src=\"js/bootstrap-tab.js\"></script><script type=\"text/javascript\" src=\"js/bootstrap-button.js\"></script><script type=\"text/javascript\" src=\"js/websql.js\"></script></body></html>"
-- >>> subTree (Just "tr") Nothing htmlTree
-- [TagBranch "tr" [] [TagBranch "th" [] [TagLeaf (TagText "\26085\26178")],TagBranch "th" [] [TagLeaf (TagText "\21517\21069")],TagBranch "th" [] [TagLeaf (TagText "\12473\12467\12450")]],TagBranch "tr" [] [TagBranch "td" [] [TagLeaf (TagText "hoho")],TagBranch "td" [("class","date")] [TagLeaf (TagText "2012-09-09")],TagBranch "td" [] [TagLeaf (TagText "ooo")]]]
-- 
-- >>> subTree (Just "td") Nothing htmlTree
-- [TagBranch "td" [] [TagLeaf (TagText "hoho")],TagBranch "td" [("class","date")] [TagLeaf (TagText "2012-09-09")],TagBranch "td" [] [TagLeaf (TagText "ooo")]]

subTree :: Eq a => Maybe a -> Maybe [Attribute a] -> [TagTree a] -> [TagTree a]
subTree _ _ [] = []
subTree (Just tagString) Nothing trees = subTreeByTag tagString trees
  where
    subTreeByTag _ [] = []
    subTreeByTag tagStr ((TagBranch tagStr' attr subT):ts)
      | tagStr == tagStr' = [TagBranch tagStr' attr subT] ++ subTreeByTag tagStr ts
      | otherwise = subTreeByTag tagStr subT ++ subTreeByTag tagStr ts
    subTreeByTag tagStr ((TagLeaf _):ts) = subTreeByTag tagStr ts
subTree Nothing (Just attributes) trees = subTreeByAttrs attributes trees
  where
    subTreeByAttrs _ [] = []
    subTreeByAttrs attrs ((TagBranch tagStr attrs' subT):ts)
      | attrs == attrs' = [TagBranch tagStr attrs' subT] ++ subTreeByAttrs attrs ts
      | otherwise = subTreeByAttrs attrs subT ++ subTreeByAttrs attrs ts
    subTreeByAttrs attrs ((TagLeaf _):ts) = subTreeByAttrs attrs ts
subTree (Just tagString) (Just attributes) trees = subTreeByTagAttrs tagString attributes trees
  where
    subTreeByTagAttrs _ _ [] = []
    subTreeByTagAttrs tagStr attrs ((TagBranch tagStr' attrs' subT):ts)
      | (tagStr, attrs) == (tagStr', attrs')
        = [TagBranch tagStr' attrs' subT] ++ subTreeByTagAttrs tagStr attrs ts
      | otherwise = subTreeByTagAttrs tagStr attrs subT ++ subTreeByTagAttrs tagStr attrs ts
    subTreeByTagAttrs tagStr attrs ((TagLeaf _):ts) = subTreeByTagAttrs tagStr attrs ts
subTree Nothing Nothing ts = ts

-- | links
-- >>> let tree = tagTree $ parseTags "<!DOCTYPE html><html><head><meta charset=\"utf-8\" /><link href=\"css/bootstrap.min.css\" rel=\"stylesheet\" /><link href=\"css/bootstrap-responsive.css\" rel=\"stylesheet\" /></head><body><div id=\"container\"><div id=\"content\"><div class=\"alert alert-success\" id=\"notification\" style=\"display:none\"> ... </div><h1><a href=\"/\">GYMMEMO</a></h1><table class=\"table\"><tr><th>日時</th><th>名前</th><th>スコア</th></tr><tr><td>hoho</td><td class=\"date\">2012-09-09</td><td><a href=\"./ooo.html\">ooo</a></td></tr></table></div><p class=\"pp\">yukku<a href=\"http://example.com\">AAA</a>oorahk</div><script type=\"text/javascript\" src=\"js/jquery.js\"></script><script type=\"text/javascript\" src=\"js/bootstrap-tab.js\"></script><script type=\"text/javascript\" src=\"js/bootstrap-button.js\"></script><script type=\"text/javascript\" src=\"js/websql.js\"></script></body></html>"
-- >>> links tree
-- [("/","GYMMEMO"),("./ooo.html","ooo"),("http://example.com","AAA")]

-- TagTreeのリストのなかにあるリンクのURLとテキストをタプルのリストで返す
links :: [TagTree String] -> [(Url, String)]
links [] = []
links tree = links' $ subTree (Just "a") Nothing tree
  where
    links' :: [TagTree String] -> [(Url, String)]
    links' [] = []
    links' ((TagBranch "a" [("href", url)] [TagLeaf (TagText title)]):ts) = (url, title): links' ts
    links' (_:ts) = links' ts
