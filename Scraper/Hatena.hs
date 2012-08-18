{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings, RankNTypes #-}

module Scraper.Hatena where

import Scraper
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Tree
import Data.String
import Control.Applicative
import Codec.Binary.UTF8.String
-- import Debug.Trace

type UserName = String

-- 指定したはてなユーザーのentryのタイトルとリンクのタプルのリストを返す
entryUrlTitlesOf :: UserName -> IO [(Url, String)]
entryUrlTitlesOf user = fst <$> entryUrlTitlesOf' 0 []
  where
    entryUrlTitlesOf' :: Int -> [(Url, String)] -> IO ([(Url, String)], Int)
    entryUrlTitlesOf' fromNum urlTitles = do
      (lts, num) <- entryUrlTitlesOf'' fromNum
      case lts of
        [] -> return (urlTitles, num)
        _ -> entryUrlTitlesOf' (num + 50) (urlTitles ++ lts)
    entryUrlTitlesOf'' :: Int -> IO ([(Url, String)], Int)
    entryUrlTitlesOf'' fromNum = do
      tags <- parsedArchivePageOf user fromNum
      return $ (entryUrlTitles tags, fromNum)

-- 指定したはてなユーザーのentryのリンクをリストで返す
entryUrlsOf :: UserName -> IO [Url]
entryUrlsOf user = fst <$> entryUrlsOf' 0 []
  where
    entryUrlsOf' :: Int -> [Url] -> IO ([Url], Int)
    entryUrlsOf' fromNum urls = do
      (ls, num) <- entryUrlsOf'' fromNum
    --   print ls
      case ls of
        [] -> return (urls ++ [], num)
        _ -> entryUrlsOf' (num + 50) (urls ++ ls)

    entryUrlsOf'' :: Int -> IO ([Url], Int)
    entryUrlsOf'' fromNum = do
      tags <- parsedArchivePageOf user fromNum
      return $ (entryUrls tags, fromNum)


archivePageOf :: UserName -> Int -> IO String
archivePageOf user fromNum = openURL $ "http://d.hatena.ne.jp/" ++ user ++ "/archive?of=" ++ show fromNum

parsedArchivePageOf :: UserName -> Int -> IO [Tag String]
parsedArchivePageOf user fromNum = do
  page <- archivePageOf user fromNum
  return $ parseTags $ convertEncoding "EUC-JP" "UTF-8" page

-- http://d.hatena.ne.jp/<user>/archive をパースしたtagからentryのリンクを抜き出す
entryUrls :: forall a. (Eq a, Data.String.IsString a) => [Tag a] -> [a]
entryUrls [] = []
entryUrls ((TagOpen "li" [("class", "archive archive-section")]) : (TagOpen "a" [("href", url)]) : ts)
                 = url : entryUrls ts
entryUrls ((TagOpen "li" [("class", "archive archive-section")]) : (TagOpen "a" [("href", url), _]) : ts)
                 = url : entryUrls ts
entryUrls (_:ts) = entryUrls ts

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

-- 指定したエントリのページを取得する
getEntryFromUrl :: Url -> IO String
getEntryFromUrl url = decodeString <$> openURL url
