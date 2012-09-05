{-# OPTIONS -Wall #-}
{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies, OverloadedStrings, GeneralizedNewtypeDeriving, GADTs, FlexibleContexts, RankNTypes #-}
module Main where

import Scraper.Hatena
-- import Control.Applicative
-- import Network.HTTP
-- import Codec.Text.IConv
-- import Data.ByteString.Lazy.Char8 (pack, unpack)
-- import Codec.Binary.UTF8.String
-- import Text.HTML.TagSoup
-- import Text.HTML.TagSoup.Tree
-- import Debug.Trace
import System.Environment
import Database.Persist
import Database.Persist.Store
import Database.Persist.Sqlite
import Database.Persist.TH
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Trans
import Data.Time (Day)
import Data.Text (Text)

-- data BlogType = HatenaDaiary | JugemBlog

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persist|
Blog
    title String
    url String
    author String
    status Bool default=True
--     blogType BlogType
    deriving Show
BlogEntry
    title String
    url String
    body String
    blog BlogId
    postedAt Day
    updatedAt Day
    status Bool default=True
    deriving Show
|]

dbpath :: Text
dbpath = "scraper.sqlite3"

runDB action = withSqliteConn dbpath $ runSqlConn $ do
    runMigration migrateAll
    action

addBlog :: String -> String -> String -> IO ()
addBlog title url author = runDB $ do
  blog <- insert $ Blog title url author True
  return ()

-- addBlogEntry :: [String] -> IO ()
addBlogEntry title url body blog updatedAt postedAt = runDB $ do
  _ <- insert $ BlogEntry title url body blog updatedAt postedAt True
  return ()

addBlogEntries :: [(String, String, String, BlogId, Day)] -> IO ()
addBlogEntries ((title, url, body, blog, postedAt):xs) = do
    addBlogEntry title url body blog postedAt postedAt
    addBlogEntries xs
    return ()

main = do
  hateDa "suzuki"
--   args <- getArgs
--   addBlog args

hateDa :: String -> IO ()
hateDa user = do
  title <- blogTitleOf user
  utds <- entryUrlTitleDaysOf user
  runDB $ do
    blog <- selectFirst [BlogAuthor ==. user] [LimitTo 1]
    case blog of
      Nothing -> do
        _ <- insert $ Blog title (blogUrlOf user) user True
        return ()
      Just blog -> return ()
--   liftIO $ print (blog :: Maybe (Entity Blog))
--   return ()