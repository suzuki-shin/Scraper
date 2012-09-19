{-# OPTIONS -Wall #-}
{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies, OverloadedStrings, GeneralizedNewtypeDeriving, GADTs, FlexibleContexts, RankNTypes #-}

module Main where

import Scraper
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
import Data.Maybe
import Control.Monad.Trans
-- import Control.Monad.Logger
import Data.Time.Calendar

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
    html String
    blog BlogId
    postedAt Day Maybe
    updatedAt Day Maybe
    status Bool default=True
    deriving Show
|]

dbpath :: Text
dbpath = "scraper.sqlite3"

-- runDB :: (MonadIO m, Control.Monad.Trans.Control.MonadBaseControl IO m, Control.Monad.Logger.MonadLogger m)
--          => SqlPersist m a -> m a
runDB action = withSqliteConn dbpath $ runSqlConn $ do
    runMigration migrateAll
    action

addBlog title url author = runDB $ insert $ Blog title url author True

addBlogEntry title url html blogId updatedAt postedAt
  = insert $ BlogEntry title url html blogId updatedAt postedAt True
-- addBlogEntry title url html blogId updatedAt postedAt
--   = runDB $ insert $ BlogEntry title url html blogId updatedAt postedAt True

addBlogOf :: (MonadIO (backend m), PersistStore backend m) => String -> backend m (Key backend (BlogGeneric backend1))
addBlogOf user = do
  title <- liftIO $ blogTitleOf user
  insert $ Blog title ("http://d.hatena.ne.jp/" ++ user) user True

addBlogEntries user blogId ((url, title, day):utds) = do
  html <- liftIO $ getEntryFromUrl url
  addBlogEntry title url html blogId day day
  addBlogEntries user blogId utds
  return ()
addBlogEntries _ _ [] = return ()

main =
  hateDa "natsu-mi-kan"
--   hoge "Tokyo-Kuni"

hoge user =
--   blogId <- selectFirst [BlogAuthor ==. user] [LimitTo 1]
-- --   blogs <- selectList [BlogAuthor ==. user] [LimitTo 5]
--   liftIO $ print blogId
--   let bid = entityKey $ fromJust blogId
--   liftIO $ print bid
  addBlogEntry "title A" "http://example.com" "<html><body></body></html>" (Key (PersistInt64 1)) Nothing Nothing
--   addBlogEntry "title A" "http://example.com" "<html><body></body></html>" bid Nothing Nothing

hateDa :: String -> IO ()
hateDa user = runDB $ do
  blogId <- blogIdOf user
--   liftIO $ print blogId
  utds <- liftIO $ entryUrlTitleDaysOf user
--   liftIO $ putStrLn "97"
--   liftIO $ print utds
  addBlogEntries user blogId utds
    where
      blogIdOf user = do
        b <- selectFirst [BlogAuthor ==. user] [LimitTo 1]
        case b of
          Nothing -> do
            liftIO $ putStrLn "Nothing"
            blogId <- addBlogOf user
            return blogId
          Just blog -> do
            liftIO $ putStrLn "Just"
            liftIO $ print blog
            return $ entityKey blog


--       utds <- liftIO $ entryUrlTitleDaysOf user
--       -- BlogEntryを検索し存在しないもしくはupdatedAtが更新されていればhtml(html全体)を取得して保存する
--       insertBlogEntries $ unsavedUtds utds (unKey . entityKey blog)
--       return ()
--     where
--       insertBlog :: String -> IO Blog
--       insertBlog user = do
--         title <- liftIO $ blogTitleOf user
--         blog <- insert $ Blog title ("http://d.hatena.ne.jp/" ++ user) user True
--         return blog
--       unsavedUtds :: [(Url, String, Day)] -> [(Url, String, Day)]
--       unsavedUtds utds = snd $ filter (\(unsavedUrl, (url, title, day)) -> unsavedUrl == url) $ zip  (unsavedUrls (map fst utds)) utds
--       insertBlogEntries ((url,title,day):utds) blogId
--         = ((insertBlogEntry url title day blogId):(insertBlogEntries utds))

-- 渡したurlsのなかからBlogEntryテーブルのurlに存在しないもののみを返す
unsavedUrls urls = do
  savedBlogs <- savedIn urls
  let savedUrls = map (blogUrl . entityVal) savedBlogs
      unsavedUrls' = filter (`notElem` savedUrls) urls
  return unsavedUrls'

-- 渡したurlsのなかでBlogEntryテーブルのurlに存在するもののみを返す
savedIn urls = runDB $ selectList [BlogUrl <-. urls] []

insertBlogEntry url title day blogId = do
  html <- getEntryFromUrl url
  runDB $ addBlogEntry title url html blogId day day
  return ()
