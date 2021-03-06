{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances, FlexibleContexts #-}

module Blockchain.Setup (
  oneTimeSetup
  ) where

import Control.Monad.IO.Class
import Control.Monad.Logger (runNoLoggingT)
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Resource
import Control.Monad.Trans.State
import qualified Data.ByteString as B
import qualified Database.Esqueleto as E
import qualified Database.LevelDB as DB
import Database.Persist.Postgresql hiding (get)
import System.Directory
import System.FilePath

import qualified Blockchain.Colors as CL
import qualified Blockchain.Database.MerklePatricia as MP
import Blockchain.Data.UnprocessedDB
import Blockchain.Data.DataDefs
import Blockchain.Data.GenesisBlock
import Blockchain.DB.CodeDB
import Blockchain.DB.HashDB
import Blockchain.DB.StateDB
import Blockchain.DB.SQLDB
import Blockchain.Constants

data SetupDBs =
  SetupDBs {
    stateDB::StateDB,
    hashDB::HashDB,
    codeDB::CodeDB,
    sqlDB::SQLDB
    }

type SetupDBM = StateT SetupDBs (ResourceT IO)
instance HasStateDB SetupDBM where
  getStateDB = do
    cxt <- get
    return $ stateDB cxt
  setStateDBStateRoot sr = do
    cxt <- get
    put cxt{stateDB=(stateDB cxt){MP.stateRoot=sr}}

{-instance HasStorageDB SetupDBM where
  getStorageDB = do
    cxt <- get
    return $ MPDB.ldb $ setupDBStateDB cxt --storage and states use the same database!-}

instance HasHashDB SetupDBM where
  getHashDB = fmap hashDB get

instance HasCodeDB SetupDBM where
  getCodeDB = fmap codeDB get

instance HasSQLDB SetupDBM where
  getSQLDB = fmap sqlDB get


connStr::ConnectionString
connStr = "host=localhost dbname=eth user=postgres password=api port=5432"

oneTimeSetup::IO ()
oneTimeSetup = do
  runNoLoggingT $ withPostgresqlConn connStr $ runReaderT $ do
    liftIO $ putStrLn $ CL.yellow ">>>> Migrating SQL DB"
    runMigration migrateAll

    liftIO $ putStrLn $ CL.yellow ">>>> Creating SQL Indexes"
    rawExecute "CREATE INDEX CONCURRENTLY ON block_data_ref (block_id);" []
    rawExecute "CREATE INDEX CONCURRENTLY ON block_data_ref (number);" []
    rawExecute "CREATE INDEX CONCURRENTLY ON block_data_ref (hash);" []
    rawExecute "CREATE INDEX CONCURRENTLY ON block_data_ref (parent_hash);" []
    rawExecute "CREATE INDEX CONCURRENTLY ON block_data_ref (coinbase);" []
    rawExecute "CREATE INDEX CONCURRENTLY ON block_data_ref (total_difficulty);" []

    rawExecute "CREATE INDEX CONCURRENTLY ON address_state_ref (address);" []

    rawExecute "CREATE INDEX CONCURRENTLY ON raw_transaction (from_address);" []
    rawExecute "CREATE INDEX CONCURRENTLY ON raw_transaction (to_address);" []
    rawExecute "CREATE INDEX CONCURRENTLY ON raw_transaction (block_id);" []
    rawExecute "CREATE INDEX CONCURRENTLY ON raw_transaction (block_number);" [] 
    rawExecute "CREATE INDEX CONCURRENTLY ON raw_transaction (tx_hash);" [] 

    rawExecute "CREATE INDEX CONCURRENTLY ON storage (key);" []
    
    rawExecute "CREATE INDEX CONCURRENTLY ON unprocessed (block_id);" []

  _ <-
    runResourceT $ do
      liftIO $ putStrLn $ CL.yellow ">>>> Setting UP DB handles"

      homeDir <- liftIO getHomeDirectory                     

      liftIO $ createDirectoryIfMissing False $ homeDir </> dbDir "h"
      sdb <- DB.open (homeDir </> dbDir "h" ++ stateDBPath)
             DB.defaultOptions{DB.createIfMissing=True, DB.cacheSize=1024}
      hdb <- DB.open (homeDir </> dbDir "h" ++ hashDBPath)
             DB.defaultOptions{DB.createIfMissing=True, DB.cacheSize=1024}
      cdb <- DB.open (homeDir </> dbDir "h" ++ codeDBPath)
             DB.defaultOptions{DB.createIfMissing=True, DB.cacheSize=1024}
      let smpdb = MP.MPDB{MP.ldb=sdb, MP.stateRoot=error "stateRoot not defined in oneTimeSetup"}
          
      pool <- runNoLoggingT $ createPostgresqlPool connStr 20

      flip runStateT (SetupDBs smpdb hdb cdb pool) $ do
        addCode B.empty --blank code is the default for Accounts, but gets added nowhere else.
        liftIO $ putStrLn $ CL.yellow ">>>> Initializing Genesis Block"
        _ <- initializeGenesisBlock
        genesisBlockId <- getGenesisBlockId
        putUnprocessed [Unprocessed genesisBlockId]

  return ()

getGenesisBlockId::HasSQLDB m=>
                   m (E.Key Block)
getGenesisBlockId = do
  ret <- sqlQuery $
         E.select $
         E.from $ \(bdRef `E.InnerJoin` block) -> do
           E.on ( bdRef E.^. BlockDataRefBlockId E.==. block E.^. BlockId )
           E.where_ (bdRef E.^. BlockDataRefNumber E.==. E.val 0 )
           return $ block E.^. BlockId

  case ret of
    [] -> error "called getBlockIdFromBlock on a block that wasn't in the DB"
    [blockId] -> return (E.unValue blockId)
    _ -> error "called getBlockIdFromBlock on a block that appears more than once in the DB"

