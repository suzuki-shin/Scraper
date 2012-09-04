{-# OPTIONS -Wall #-}
{-# LANGUAGE RankNTypes #-}

module Scraper.Hatena (
    entryUrlTitleDaysOf
  , entryUrlTitlesOf
  , entryUrlsOf
  , getEntryFromUrl
  , blogTitleOf
  ) where

import Scraper
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Tree
-- import Data.String
import Control.Applicative
import Codec.Binary.UTF8.String
import Data.Time.Calendar
-- import Data.List.Split
import Text.Regex.Posix
-- import Debug.Trace

type UserName = String

baseUrl :: Url
baseUrl = "http://d.hatena.ne.jp/"

-- 指定したはてなユーザーのentryのURLとタイトルと書かれた日のタプルのリストを返す
entryUrlTitleDaysOf :: UserName -> IO [(Url, String, Maybe Day)]
entryUrlTitleDaysOf user = fst <$> entryUrlTitleDaysOf' 0 []
  where
    entryUrlTitleDaysOf' :: Int -> [(Url, String, Maybe Day)] -> IO ([(Url, String, Maybe Day)], Int)
    entryUrlTitleDaysOf' fromNum urlTitleDays = do
      (lts, num) <- entryUrlTitleDaysOf'' fromNum
      case lts of
        [] -> return (urlTitleDays, num)
        _ -> entryUrlTitleDaysOf' (num + 50) (urlTitleDays ++ lts)
    entryUrlTitleDaysOf'' :: Int -> IO ([(Url, String, Maybe Day)], Int)
    entryUrlTitleDaysOf'' fromNum = do
      tags <- parsedArchivePageOf user fromNum
      return $ (entryUrlTitleDays tags, fromNum)

-- 指定したはてなユーザーのentryのタイトルとリンクのタプルのリストを返す
entryUrlTitlesOf :: UserName -> IO [(Url, String)]
entryUrlTitlesOf user = do
  utds <- entryUrlTitleDaysOf user
  return $ map (\(url, title, _) -> (url, title)) utds

-- 指定したはてなユーザーのentryのリンクをリストで返す
entryUrlsOf :: UserName -> IO [Url]
entryUrlsOf user = do
  urlTitles <- entryUrlTitleDaysOf user
  return $ map (\(url, _, _) -> url) urlTitles

archivePageOf :: UserName -> Int -> IO String
archivePageOf user fromNum = openURL $ baseUrl ++ user ++ "/archive?of=" ++ show fromNum

parsedArchivePageOf :: UserName -> Int -> IO [Tag String]
parsedArchivePageOf user fromNum = do
  page <- archivePageOf user fromNum
  return $ parseTags $ convertEncoding "EUC-JP" "UTF-8" page

-- | archiveSections
-- >>> let trees = tagTree $ parseTags "<body><h1>hoge</h1><div class=\"fuga\">FUgya!<ul id=\"archive\"><li class=\"archive-section\">not</li><li class=\"archive archive-section\"><a href=\"hoho\">OK</a>OK</li><li>mumu</li><li class=\"archive archive-section\">1233</li></ul><p><li class=\"archive archive-section\"></li></p></body>"
-- >>> archiveSections trees
-- [TagBranch "li" [("class","archive archive-section")] [TagBranch "a" [("href","hoho")] [TagLeaf (TagText "OK")],TagLeaf (TagText "OK")],TagBranch "li" [("class","archive archive-section")] [TagLeaf (TagText "1233")],TagBranch "li" [("class","archive archive-section")] []]
archiveSections :: [TagTree String] -> [TagTree String]
archiveSections = subTree (Just "li") (Just [("class", "archive archive-section")])

-- | entryUrlTitles
-- >>> let tags = parseTags "<body><h1>hoge</h1><div class=\"fuga\">FUgya!<ul id=\"archive\"><li class=\"archive-section\">not</li><li class=\"archive archive-section\"><a href=\"hoho\">OK</a>OK</li><li>mumu</li><li class=\"archive archive-section\">1233</li></ul><p><li class=\"archive archive-section\"></li></p></body>"
-- >>> entryUrlTitles tags
-- [("hoho","OK")]
entryUrlTitles :: [Tag String] -> [(Url, String)]
entryUrlTitles = links . archiveSections . tagTree

-- | entryUrlTitleDays
-- >>> let tags = parseTags "<body><h1>hoge</h1><div class=\"fuga\">FUgya!<ul id=\"archive\"><li class=\"archive-section\">not</li><li class=\"archive archive-section\"><a href=\"http://d.hatena.ne.jp/hoho/20120808#12345566\">OK</a>OK</li><li>mumu</li><li class=\"archive archive-section\">1233</li></ul><p><li class=\"archive archive-section\"></li></p></body>"
-- >>> entryUrlTitleDays tags
-- [("http://d.hatena.ne.jp/hoho/20120808#12345566","OK",Just 2012-08-08)]
entryUrlTitleDays :: [Tag String] -> [(Url, String, Maybe Day)]
entryUrlTitleDays = map (\(url, title) -> (url, title, dayFromUrl url)) . entryUrlTitles

-- | dayFromUrl
-- >>> dayFromUrl ""
-- Nothing
-- >>> dayFromUrl "http://hoge.fuga"
-- Nothing
-- >>> dayFromUrl "http://d.hatena.ne.jp/hoge/20120812#1234325"
-- Just 2012-08-12
-- >>> dayFromUrl "http://d.hatena.ne.jp/hoge/2012112#1234325"
-- Nothing
-- >>> dayFromUrl "http://d.hatena.ne.jp/suzuki-shin/archive?word=&of=0"
-- Nothing
dayFromUrl :: Url -> Maybe Day
dayFromUrl url = let ymd = getYmd url
                 in case length ymd == 4 of
                   True -> Just $ fromGregorian ((read (ymd!!1)) :: Integer)
                                                ((read (ymd!!2)) :: Int)
                                                ((read (ymd!!3)) :: Int)
                   _ -> Nothing
  where
    getYmd :: Url -> [String]
    getYmd [] = []
    getYmd url' = concat $ (url' =~ (baseUrl ++ "[^/]+/([0-9]{4})([0-9]{2})([0-9]{2})") :: [[String]])

-- 指定したエントリのページを取得する
getEntryFromUrl :: Url -> IO String
getEntryFromUrl url = decodeString <$> openURL url

-- | blogTitle
-- >>> let tags = parseTags "<html><head><title>aaabbbccc</title></head><body><h1>hoge</h1></body></html>"
-- >>> blogTitle tags
-- "aaabbbccc"
blogTitle :: [Tag String] -> String
blogTitle = titleText . flattenTree . subTree (Just "title") Nothing . tagTree
  where
    titleText :: [Tag String] -> String
    titleText (TagText title:_) = title
    titleText (_:ts) = titleText ts
    titleText _ = ""

blogTitleOf :: UserName -> IO String
blogTitleOf user = do
  page <- openURL blogUrl
  return $ blogTitle $ parseTags $ convertEncoding "EUC-JP" "UTF-8" page
  where
    blogUrl = baseUrl ++ user
