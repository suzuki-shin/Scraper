{-# OPTIONS -Wall #-}
{-# LANGUAGE RankNTypes #-}

module Scraper.Jugem (
    lastEid
  , blogUrl
  , entryUrlsOf
  ) where

import Scraper
-- import Text.HTML.TagSoup
-- import Text.HTML.TagSoup.Tree
-- import Data.String
-- import Control.Applicative
-- import Codec.Binary.UTF8.String
-- import Data.Time.Calendar
-- import Data.List.Split
import Text.Regex.Posix
import qualified Data.List as L
-- import Data.Text as Text
-- import Debug.Trace

type UserName = String

blogUrl :: UserName -> Url
blogUrl user = "http://" ++ user ++ ".jugem.jp/"


-- | lastEid
-- >>> lastEid "eid=123\"><a href=\"./eid=429"
-- Just 429
-- >>> lastEid "eid=123\"><a href=\"./eid=E429"
-- Just 123
-- >>> lastEid ""
-- Nothing
-- >>> lastEid "ed=123\"><a href=\"./id=E429"
-- Nothing
lastEid :: String -> Maybe Int
lastEid page = case length eids >= 1 of
  True -> Just $ L.maximum (map (read . (!!1)) eids :: [Int])
  _ -> Nothing
  where
    eids = page =~ "eid=([0-9]+)" :: [[String]]

entryUrlsOf :: UserName -> IO [Url]
entryUrlsOf user = do
  let url = blogUrl user
  page <- openURL url
  case lastEid page of
    Nothing       -> return []
    Just lastEid' -> return $ map ((\eid -> url ++ "?eid=" ++ eid) . show) [1..lastEid']
