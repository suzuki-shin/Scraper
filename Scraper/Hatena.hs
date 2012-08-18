{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings, RankNTypes #-}

module Scraper.Hatena (entryLinksOf, getEntryFromUrl) where

import Scraper
import Text.HTML.TagSoup
import Data.String
import Control.Applicative
import Codec.Binary.UTF8.String
-- import Network.HTTP
-- import Codec.Text.IConv
-- import Data.ByteString.Lazy.Char8 (pack, unpack)

type UserName = String

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
      page <- openURL $ "http://d.hatena.ne.jp/" ++ user ++ "/archive?of=" ++ show fromNum
      let tags = parseTags $ convertEncoding "EUC-JP" "UTF-8" page
      return $ (entryLinks tags, fromNum)

    -- http://d.hatena.ne.jp/<user>/archive からentryのリンクを抜き出す
    entryLinks :: forall a. (Eq a, Data.String.IsString a) => [Tag a] -> [a]
    entryLinks [] = []
    entryLinks ((TagOpen "li" [("class", "archive archive-date")]) : (TagOpen "a" [("href", url)]) : ts)
                     = url : entryLinks ts
    entryLinks ((TagOpen "li" [("class", "archive archive-date")]) : (TagOpen "a" [("href", url), _]) : ts)
                     = url : entryLinks ts
    entryLinks (_:ts) = entryLinks ts

getEntryFromUrl :: Url -> IO String
getEntryFromUrl url = decodeString <$> openURL url
