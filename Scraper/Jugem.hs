{-# OPTIONS -Wall #-}
{-# LANGUAGE RankNTypes #-}

module Scraper.Jugem (
  lastEid
  ) where

import Scraper
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Tree
import Data.String
import Control.Applicative
import Codec.Binary.UTF8.String
import Data.Time.Calendar
import Data.List.Split
import Text.Regex.Posix
import qualified Data.List as L
-- import Data.Text as Text
-- import Debug.Trace

type UserName = String

blogUrl :: UserName -> Url
blogUrl user = "http://" ++ user ++ ".jugem.ne.jp/"

lastEid :: Url -> IO Int
lastEid url = do
  page <- openURL url
  let eids = page =~ "eid=([0-9]+)" :: [[String]]
  return $ L.maximum (map (read . (!!1)) eids :: [Int])
