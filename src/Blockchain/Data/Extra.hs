
module Blockchain.Data.Extra (
     getBestProcessedStateRoot,
     putBestProcessedStateRoot
    ) where

import Data.Maybe
import qualified Database.Esqueleto as E
import qualified Database.Persist.Sql as SQL

import Blockchain.Data.DataDefs
import qualified Blockchain.Database.MerklePatricia as MP
import Blockchain.DB.SQLDB

getBestProcessedStateRoot::HasSQLDB m=>
                           m (MP.SHAPtr, Integer)
getBestProcessedStateRoot = do
  ret <- sqlQuery $ 
         E.select $ E.from $ \kv -> do
             E.where_ (kv E.^. ExtraKey E.==. E.val "bestBlockNumber")
             return $ kv E.^. ExtraValue

  case ret of
    [x] -> return $ read $ E.unValue x
    _ -> error "Ethereum DBs are blank, you need to set them up by running 'ethereum-setup'"


putBestProcessedStateRoot::HasSQLDB m=>
                           MP.SHAPtr->Integer->m ()
putBestProcessedStateRoot stateRoot bestNumber = do
  sqlQuery $ SQL.upsert (Extra "bestBlockNumber" $ show (stateRoot, bestNumber)) []
  return ()
