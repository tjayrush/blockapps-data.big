{-# LANGUAGE FlexibleContexts #-}

module Blockchain.Data.TransactionResult (
     putTransactionResult
    ) where

import Database.Persist hiding (get)
import Database.Persist.Types
import Database.Persist.TH
import qualified Database.Persist.Postgresql as SQL
import qualified Database.Esqueleto as E

import Control.Monad.State
import Control.Monad.Trans.Resource


import Blockchain.Data.DataDefs
import Blockchain.DBM
import Blockchain.DB.SQLDB

putTransactionResult::(HasSQLDB m, MonadIO m, MonadBaseControl IO m)=>
                      TransactionResult->m (Key TransactionResult)
putTransactionResult tr = do
  pool <- getSQLDB
  runResourceT $ SQL.runSqlPool (SQL.insert tr) pool
