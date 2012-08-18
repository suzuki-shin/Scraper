{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings, RankNTypes #-}

module Scraper.Hatena (entryLinksOf, getEntryFromUrl) where

import Scraper
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Tree
import Data.String
import Control.Applicative
import Codec.Binary.UTF8.String
-- import Network.HTTP
-- import Codec.Text.IConv
-- import Data.ByteString.Lazy.Char8 (pack, unpack)

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
archiveSections [] = []
archiveSections ((TagBranch "li" [("class", "archive archive-section")] tree):ts) = tree : archiveSections ts
archiveSections ((TagBranch _ _ tree):ts) = (concat $ archiveSections tree) : archiveSections ts
archiveSections (_:ts) = archiveSections ts

links [] = []
links ((TagBranch "a" [("href", url)] [(TagLeaf (TagText title))]):ts) = (url, title): links ts
links (_:ts) = links ts

entryLinksTitles :: [Tag String] -> [(Url, String)]
entryLinksTitles tags = links ((archiveSections $ tagTree tags)!!0)

-- ("http://d.hatena.ne.jp/suzuki-shin/20120717#1342533311",
--  [TagLeaf (TagText " [haskell]spot\12434\21205\12363\12375\12390\12415\12424\12358\12392\12375\12383\12392\12365\12398\12513\12514")])


-- 指定したエントリのページを取得する
getEntryFromUrl :: Url -> IO String
getEntryFromUrl url = decodeString <$> openURL url
