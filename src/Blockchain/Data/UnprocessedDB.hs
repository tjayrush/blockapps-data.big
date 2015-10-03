{-# LANGUAGE OverloadedStrings, ForeignFunctionInterface #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Blockchain.Data.UnprocessedDB (
  Block(..),
  putUnprocessed,
  deleteUnprocessed
) where 

import qualified Database.Esqueleto as E
import Database.Persist hiding (get)
import qualified Database.Persist.Postgresql as SQL

import Blockchain.DB.SQLDB
import Blockchain.Data.DataDefs

import Control.Monad.State
import Control.Monad.Trans.Resource

putUnprocessed :: (HasSQLDB m, MonadIO m)=>
               [Unprocessed]->m [Key Unprocessed]
putUnprocessed ps = do
  db <- getSQLDB
  runResourceT $ flip SQL.runSqlPool db $
    forM ps $ \p -> SQL.insert p

deleteUnprocessed::(HasSQLDB m, MonadIO m)=>
               [E.Key Block]->m ()
deleteUnprocessed blockIds = do
  db <- getSQLDB
  runResourceT $ flip SQL.runSqlPool db $
    E.delete $
    E.from $ \p ->
    E.where_ (p E.^. UnprocessedBlockId `E.in_` E.valList blockIds)

--instance Format Unprocessed where
--  format Unprocessed{unprocessedBlockId=blockId} = CL.yellow $ format blockId
