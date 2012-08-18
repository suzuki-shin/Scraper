{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings, RankNTypes #-}

module Scraper.Hatena (entryLinksOf, getEntryFromUrl) where

import Scraper
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Tree
import Data.String
import Control.Applicative
import Codec.Binary.UTF8.String
import Debug.Trace

type UserName = String

-- 指定したはてなユーザーのentryのタイトルとリンクのタプルのリストを返す
-- entryTitleLinksOf :: UserName -> IO [(String, Url)]

-- 指定したはてなユーザーのentryのリンクをリストで返す
entryLinksOf :: UserName -> IO [Url]
entryLinksOf user = fst <$> entryLinksOf' user 0 []
  where
    entryLinksOf' :: UserName -> Int -> [Url] -> IO ([Url], Int)
    entryLinksOf' user fromNum links = do
      (ls, num) <- entryLinksOf'' user fromNum
    --   print ls
      case ls of
        [] -> return (links ++ [], num)
        _ -> entryLinksOf' user (num + 50) (links ++ ls)

    entryLinksOf'' :: UserName -> Int -> IO ([Url], Int)
    entryLinksOf'' user fromNum = do
      tags <- parsedArchivePageOf user fromNum
      return $ (entryLinks tags, fromNum)


archivePageOf :: UserName -> Int -> IO String
archivePageOf user fromNum = openURL $ "http://d.hatena.ne.jp/" ++ user ++ "/archive?of=" ++ show fromNum

parsedArchivePageOf :: UserName -> Int -> IO [Tag String]
parsedArchivePageOf user fromNum = do
  page <- archivePageOf user fromNum
  return $ parseTags $ convertEncoding "EUC-JP" "UTF-8" page

-- http://d.hatena.ne.jp/<user>/archive をパースしたtagからentryのリンクを抜き出す
entryLinks :: forall a. (Eq a, Data.String.IsString a) => [Tag a] -> [a]
entryLinks [] = []
entryLinks ((TagOpen "li" [("class", "archive archive-section")]) : (TagOpen "a" [("href", url)]) : ts)
                 = url : entryLinks ts
entryLinks ((TagOpen "li" [("class", "archive archive-section")]) : (TagOpen "a" [("href", url), _]) : ts)
                 = url : entryLinks ts
entryLinks (_:ts) = entryLinks ts

-- archiveSections :: [TagTree] -> [[TagTree]]
archiveSections tts = concat $ _archiveSections tts
_archiveSections [] = []
_archiveSections ((TagBranch "li" [("class", "archive archive-section")] tree):ts) = tree : _archiveSections ts
_archiveSections ((TagBranch _ _ tree):ts) = (concat $ _archiveSections tree) : _archiveSections ts
_archiveSections (_:ts) = _archiveSections ts

-- archiveSectionsのデバッグ用
as1 [] = []
as1 ((TagBranch "li" [("class", "archive archive-section")] tree):ts)
    = trace ("### archive-section ###\n--- tree ---(" ++ show (length tree) ++ ")\n" ++ show tree ++ "\n--- ts ---" ++ show (length ts) ++ ")\n" ++ show ts ++ "\n")
            (tree : (as1 ts))
as1 ((TagBranch _ _ tree):ts) = ((concat $ as1 tree) : as1 ts)
as1 (_:ts) = (as1 ts)
-- as1 ((TagBranch _ _ tree):ts) = trace ("### TagBranch _ _ ###\n") ((concat $ as1 tree) : (as1 ts))
-- as1 (_:ts) = trace ("### _:ts ###\n") (as1 ts)


links [] = []
links ((TagBranch "a" [("href", url)] [(TagLeaf (TagText title))]):ts) = (url, title): links ts
links (_:ts) = links ts

entryLinksTitles :: [Tag String] -> [(Url, String)]
entryLinksTitles = links . archiveSections . tagTree

-- 指定したエントリのページを取得する
getEntryFromUrl :: Url -> IO String
getEntryFromUrl url = decodeString <$> openURL url
