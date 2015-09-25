{-# LANGUAGE OverloadedStrings, TupleSections #-}

module Blockchain.Data.GenesisBlock (
                      initializeGenesisBlock,
                      initializeStateDB
) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BLC
import Data.Maybe

import Blockchain.Database.MerklePatricia

import Blockchain.Data.Address
import Blockchain.Data.AddressStateDB
import Blockchain.Data.BlockDB
import Blockchain.Data.GenesisInfo
import Blockchain.Data.StablenetGenesis
import qualified Blockchain.Data.TestnetGenesis as Testnet
import Blockchain.Data.DiffDB
import Blockchain.DB.CodeDB
import Blockchain.DB.HashDB
import Blockchain.DB.StateDB
import Blockchain.DB.SQLDB
import Blockchain.ExtWord

--import Debug.Trace

initializeBlankStateDB::HasStateDB m=>
                        m ()
initializeBlankStateDB = do
  db <- getStateDB
  liftIO $ runResourceT $
         initializeBlank db
  setStateDBStateRoot emptyTriePtr

initializeStateDB::(HasStateDB m, HasHashDB m)=>
                   [(Address, Integer)]->m ()
initializeStateDB addressInfo = do
  initializeBlankStateDB
  
  forM_ addressInfo $ \(address, balance) ->
    putAddressState address blankAddressState{addressStateBalance=balance}




genesisInfoToGenesisBlock::(HasStateDB m, HasHashDB m)=>
                           GenesisInfo->m Block
genesisInfoToGenesisBlock gi = do
  initializeStateDB $ genesisInfoAccountInfo gi
  db <- getStateDB
  return $
    Block {
      blockBlockData =
         BlockData {
           blockDataParentHash = genesisInfoParentHash gi,
           blockDataUnclesHash = genesisInfoUnclesHash gi, 
           blockDataCoinbase = genesisInfoCoinbase gi,
           blockDataStateRoot = stateRoot db, 
           blockDataTransactionsRoot = genesisInfoTransactionsRoot gi, 
           blockDataReceiptsRoot = genesisInfoReceiptsRoot gi, 
           blockDataLogBloom = genesisInfoLogBloom gi, 
           blockDataDifficulty = genesisInfoDifficulty gi, 
           blockDataNumber = genesisInfoNumber gi, 
           blockDataGasLimit = genesisInfoGasLimit gi, 
           blockDataGasUsed = genesisInfoGasUsed gi, 
           blockDataTimestamp = genesisInfoTimestamp gi, 
           blockDataExtraData = genesisInfoExtraData gi, 
           blockDataMixHash = genesisInfoMixHash gi, 
           blockDataNonce = genesisInfoNonce gi
           },
      blockReceiptTransactions=[],
      blockBlockUncles=[]
      }
         


initializeGenesisBlock::(HasStateDB m, HasCodeDB m, HasSQLDB m, HasHashDB m)=>
                        String->m Block
initializeGenesisBlock genesisName = do
  --liftIO $ putStrLn $ BLC.unpack $ encode $ toJSON canonicalGenesisInfo
  theJSONString <- liftIO $ BLC.readFile "genesis.json"

  let theJSON =
        case eitherDecode theJSONString of
          Left x -> error x
          Right x -> x
  
  genesisBlock <-
      genesisInfoToGenesisBlock theJSON
  
      
{-  genesisBlock <-
      genesisInfoToGenesisBlock
      (
       case genesisName of
         "stablenet" -> stablenetGenesisInfo
         "canonical" -> canonicalGenesisInfo
         "testnet" -> Testnet.genesisInfo
         _ -> error $ "Unknown genesis block name: " ++ genesisName
      ) -}
  (_, genBlkId) <- putBlock genesisBlock
  genAddrStates <- getAllAddressStates
  let diffFromPair (addr', addrS) = CreateAddr addr' addrS
  commitSqlDiffs genBlkId 0 $ map diffFromPair genAddrStates

  return genesisBlock




