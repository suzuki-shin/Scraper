{-# OPTIONS -Wall #-}
{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies, OverloadedStrings, GeneralizedNewtypeDeriving, GADTs, FlexibleContexts, RankNTypes #-}

module Main where

import Scraper
import Scraper.Jugem as J
-- import Debug.Trace
-- import System.Environment
import Database.Persist
-- import Database.Persist.Store
import Database.Persist.Sqlite
import Database.Persist.TH
-- import Control.Monad.IO.Class (liftIO, MonadIO)
-- import Data.Time (Day)
import Data.Text (Text)
-- import Data.Maybe
-- import Control.Monad.Trans
-- import Control.Monad.Logger
import Data.Time.Calendar
import System.FilePath.Posix
import Data.List.Utils

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
runDB action = withSqliteConn dbpath $ runSqlConn $ action

-- addBlogOf :: (Control.Monad.IO.Class.MonadIO m, Control.Monad.Trans.Control.MonadBaseControl IO m, Control.Monad.Logger.MonadLogger m, Control.Monad.Trans.Resource.MonadUnsafeIO m, Control.Monad.Trans.Resource.MonadThrow m) => String -> m (Key SqlPersist (BlogGeneric backend))
addBlogOf user = runDB $ do
  let title = user
  insert $ Blog title (J.blogUrl user) user True

main :: IO ()
main = do
  putStrLn "Input Blog owner"
  user <- getLine
  urls <- entryUrlsOf user
  mapM_ print urls
  _ <- addBlogOf user
  writeEntryToFile user urls
    where
      fileName user' url' = "entries" </> user' ++ "__" ++ (last (split "?" url'))
      writeEntryToFile user (url:urls) = do
        writeFileFromUrl url (fileName user url) "EUC-JP" "UTF-8"
        writeEntryToFile user urls
      writeEntryToFile _ [] = return ()