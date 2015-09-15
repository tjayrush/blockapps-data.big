{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Blockchain.DBM (
  DBs(..),
  DBsLite(..),
  DBMLite,
  openDBs,
  openDBsLite
  ) where


import Control.Monad.IO.Class
import Control.Monad.State
import Control.Monad.Trans.Resource
import System.Directory
import System.FilePath

import           Control.Monad.Logger    (runNoLoggingT)
import qualified Database.Persist.Postgresql as SQL

import Blockchain.Constants

import Blockchain.Data.DataDefs
import Blockchain.DB.SQLDB

--import Debug.Trace

data DBs =
  DBs {
    sqlDB'::SQLDB
    }

data DBsLite =
  DBsLite {
     sqlDBLite :: SQLDB
     }

type DBMLite = StateT DBsLite (ResourceT IO)

connStr::SQL.ConnectionString
connStr = "host=localhost dbname=eth user=postgres password=api port=5432"

openDBs::(MonadResource m, MonadBaseControl IO m)=>m DBs
openDBs = do
  sqldb <-   runNoLoggingT  $ SQL.createPostgresqlPool connStr 20
  SQL.runSqlPool (SQL.runMigration migrateAll) sqldb
  return $ DBs
      sqldb

openDBsLite :: (MonadResource m, MonadBaseControl IO m) => SQL.ConnectionString -> m DBsLite
openDBsLite connectionString = do
  sqldb <- runNoLoggingT  $ SQL.createPostgresqlPool connectionString 20
  SQL.runSqlPool (SQL.runMigration migrateAll) sqldb
  return $ DBsLite sqldb
