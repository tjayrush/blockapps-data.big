
module Blockchain.Data.Extra (
     getBestProcessedStateRoot,
     putBestProcessedStateRoot
    ) where

import qualified Database.Persist.Sql as SQL

import Blockchain.Data.DataDefs
import qualified Blockchain.Database.MerklePatricia as MP
import Blockchain.DB.SQLDB

getBestProcessedStateRoot::HasSQLDB m=>
                           m (MP.SHAPtr, Integer)
getBestProcessedStateRoot = do
  ret <- sqlQuery $ SQL.getJust (ExtraKey "bestBlockNumber")
  return $ read $ extraValue ret


putBestProcessedStateRoot::HasSQLDB m=>
                           MP.SHAPtr->Integer->m ()
putBestProcessedStateRoot stateRoot bestNumber = do
  _ <- sqlQuery $ SQL.upsert (Extra "bestBlockNumber" $ show (stateRoot, bestNumber)) []
  return ()
