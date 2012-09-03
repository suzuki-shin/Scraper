{-# OPTIONS -Wall #-}
{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies, OverloadedStrings, GeneralizedNewtypeDeriving, GADTs, FlexibleContexts, RankNTypes #-}

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

addBlog :: [String] -> IO ()
addBlog [title, url, author] = runDB $ do
  blog <- insert $ Blog title url author True
  return ()
addBlog _ = error "bad args"

-- addBlogEntry :: [String] -> IO ()
addBlogEntry title url body blog updatedAt postedAt = runDB $ do
  _ <- insert $ BlogEntry title url body blog updatedAt postedAt True
  return ()
addBlogEntry _ _ _ _ _ _ = error "bad args"

main = do
  args <- getArgs
  addBlog args
