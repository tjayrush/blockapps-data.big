
module Blockchain.Data.Extra (
     getBestProcessedStateRoot,
     putBestProcessedStateRoot
    ) where

import qualified Database.Esqueleto as E

import Blockchain.Data.DataDefs
import qualified Blockchain.Database.MerklePatricia as MP
import Blockchain.DB.SQLDB

getBestProcessedStateRoot::HasSQLDB m=>
                           m (MP.SHAPtr, Integer)
getBestProcessedStateRoot = do
  ret <-
      sqlQuery $
      E.select $ 
       E.from $ \kv -> do
         E.where_ (kv E.^. ExtraKey E.==. E.val "bestBlockNumber")
         return $ kv E.^. ExtraValue

  case ret of
    [x] -> return undefined -- $ read . E.unValue . head $ x
    [] -> error "Ethereum DBs are blank, you need to set them up by running 'ethereum-setup'"
    _ -> error "getBestBlockHash can't handle a tie yet, yet that is what we have."


putBestProcessedStateRoot::HasSQLDB m=>
                           MP.SHAPtr->Integer->m ()
putBestProcessedStateRoot = undefined

