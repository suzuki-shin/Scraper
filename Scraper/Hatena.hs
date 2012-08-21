{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings, RankNTypes #-}

module Scraper.Hatena (
    entryUrlTitleDaysOf
  , entryUrlTitlesOf
  , entryUrlsOf
  , parsedArchivePageOf
  , getEntryFromUrl
  ) where

import Scraper
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Tree
import Data.String
import Control.Applicative
import Codec.Binary.UTF8.String
import Data.Time.Calendar
import Data.List.Split
-- import Debug.Trace

type UserName = String

-- 指定したはてなユーザーのentryのURLとタイトルと書かれた日のタプルのリストを返す
entryUrlTitleDaysOf :: UserName -> IO [(Url, String, Day)]
entryUrlTitleDaysOf user = fst <$> entryUrlTitleDaysOf' 0 []
  where
    entryUrlTitleDaysOf' :: Int -> [(Url, String, Day)] -> IO ([(Url, String, Day)], Int)
    entryUrlTitleDaysOf' fromNum urlTitleDays = do
      (lts, num) <- entryUrlTitleDaysOf'' fromNum
      case lts of
        [] -> return (urlTitleDays, num)
        _ -> entryUrlTitleDaysOf' (num + 50) (urlTitleDays ++ lts)
    entryUrlTitleDaysOf'' :: Int -> IO ([(Url, String, Day)], Int)
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
archivePageOf user fromNum = openURL $ "http://d.hatena.ne.jp/" ++ user ++ "/archive?of=" ++ show fromNum

parsedArchivePageOf :: UserName -> Int -> IO [Tag String]
parsedArchivePageOf user fromNum = do
  page <- archivePageOf user fromNum
  return $ parseTags $ convertEncoding "EUC-JP" "UTF-8" page

-- http://d.hatena.ne.jp/<user>/archive をパースしたtagからentryのリンクを抜き出す
-- entryUrls :: forall a. (Eq a, Data.String.IsString a) => [Tag a] -> [a]
-- entryUrls [] = []
-- entryUrls ((TagOpen "li" [("class", "archive archive-section")]) : (TagOpen "a" [("href", url)]) : ts)
--                  = url : entryUrls ts
-- entryUrls ((TagOpen "li" [("class", "archive archive-section")]) : (TagOpen "a" [("href", url), _]) : ts)
--                  = url : entryUrls ts
-- entryUrls (_:ts) = entryUrls ts

archiveSections :: (Eq t, IsString t) => [TagTree t] -> [TagTree t]
archiveSections tts = concat $ _archiveSections tts
  where
    _archiveSections [] = []
    _archiveSections ((TagBranch "li" [("class", "archive archive-section")] tree):ts)
                     = tree : _archiveSections ts
    _archiveSections ((TagBranch _ _ tree):ts) = (concat $ _archiveSections tree) : _archiveSections ts
    _archiveSections (_:ts) = _archiveSections ts

-- archiveSectionsのデバッグ用
-- as1 [] = []
-- as1 ((TagBranch "li" [("class", "archive archive-section")] tree):ts)
--     = trace ("### archive-section ###\n--- tree ---(" ++ show (length tree) ++ ")\n" ++ show tree ++ "\n--- ts ---" ++ show (length ts) ++ ")\n" ++ show ts ++ "\n")
--             (tree : (as1 ts))
-- as1 ((TagBranch _ _ tree):ts) = ((concat $ as1 tree) : as1 ts)
-- as1 (_:ts) = (as1 ts)
-- as1 ((TagBranch _ _ tree):ts) = trace ("### TagBranch _ _ ###\n") ((concat $ as1 tree) : (as1 ts))
-- as1 (_:ts) = trace ("### _:ts ###\n") (as1 ts)


links :: [TagTree String] -> [(Url, String)]
links [] = []
links ((TagBranch "a" [("href", url)] [(TagLeaf (TagText title))]):ts) = (url, title): links ts
links (_:ts) = links ts

entryUrlTitles :: [Tag String] -> [(Url, String)]
entryUrlTitles = links . archiveSections . tagTree

entryUrlTitleDays :: [Tag String] -> [(Url, String, Day)]
entryUrlTitleDays = map (\(url, title) -> (url, title, dayFromUrl url)) . entryUrlTitles

dayFromUrl :: Url -> Day
dayFromUrl url = fromGregorian ((read yyyy) :: Integer)
                               ((read mm) :: Int)
                               ((read dd) :: Int)
                   where
                     yyyymmdd = (sepByOneOf "/#" ((splitOn "http://d.hatena.ne.jp/" url)!!1))!!1
                     [yyyy, mm, dd] = splitPlaces [(4::Int),2,2] yyyymmdd

-- 指定したエントリのページを取得する
getEntryFromUrl :: Url -> IO String
getEntryFromUrl url = decodeString <$> openURL url
