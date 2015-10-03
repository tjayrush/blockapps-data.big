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
{-# OPTIONS_GHC -fno-warn-orphans       #-}


module Blockchain.Data.BlockDB (
  Block(..),
  BlockData(..),
  blockHash,
  getBlock,
  putBlocks,
  rawTX2TX,
  tx2RawTX,
  tx2RawTX'
) where 

import Database.Persist hiding (get)
import qualified Database.Persist.Postgresql as SQL
import qualified Database.Esqueleto as E


import qualified Data.ByteString as B

import Data.Functor
import Data.List
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S

import Data.Time.Clock.POSIX

import Numeric
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

import Blockchain.Data.Address
import qualified Blockchain.Colors as CL

import Blockchain.DB.SQLDB

import Blockchain.ExtWord
import Blockchain.Format
import Blockchain.Data.RLP
import Blockchain.SHA
import Blockchain.Util
import Blockchain.Data.RawTransaction
import Blockchain.Data.Transaction
import Blockchain.Data.DataDefs
import Blockchain.Data.Code

import Control.Monad.State
import Control.Monad.Trans.Resource

import Debug.Trace

rawTX2TX :: RawTransaction -> Transaction
rawTX2TX (RawTransaction _ nonce gp gl (Just to) val dat r s v _ _ _) = (MessageTX nonce gp gl to val dat r s v)
rawTX2TX (RawTransaction _ nonce gp gl Nothing val init' r s v _ _ _) = (ContractCreationTX nonce gp gl val (Code init') r s v)

tx2RawTX :: Transaction -> (Key Block) -> Integer ->  RawTransaction
tx2RawTX tx blkId blkNum =
  case tx of
    (MessageTX nonce gp gl to val dat r s v) -> (RawTransaction signer nonce gp gl (Just to) val dat r s v blkId (fromIntegral $ blkNum) (hash $ rlpSerialize $ rlpEncode tx))
    (ContractCreationTX nonce gp gl val (Code init') r s v) ->  (RawTransaction signer nonce gp gl Nothing val init' r s v blkId (fromIntegral $ blkNum) (hash $ rlpSerialize $ rlpEncode tx))
  where
    signer = fromMaybe (Address (-1)) $ whoSignedThisTransaction tx


tx2RawTX' :: Transaction -> RawTransaction
tx2RawTX' tx = tx2RawTX tx (E.toSqlKey 1) (-1)

calcTotalDifficulty :: (HasSQLDB m, MonadResource m, MonadBaseControl IO m, MonadThrow m)=>
                       Block -> BlockId -> m Integer
calcTotalDifficulty b _ = do
  db <- getSQLDB
  let bd = blockBlockData b

  parent <- runResourceT $
     SQL.runSqlPool (getParent (blockDataParentHash bd)) db
  case parent of
    Nothing ->
      case (blockDataNumber bd) of
        0 -> return (blockDataDifficulty bd)
        _ ->  error $ "couldn't find parent to calculate difficulty, parent hash is " ++ format (blockDataParentHash bd)
    Just p -> return $ (blockDataRefTotalDifficulty . entityVal $ p) + (blockDataDifficulty bd)
     
  where getParent h = do
          SQL.selectFirst [ BlockDataRefHash SQL.==. h ] []

blk2BlkDataRef :: (HasSQLDB m, MonadResource m) =>
                  M.Map SHA Integer->(Block, SHA)->BlockId->m BlockDataRef
blk2BlkDataRef dm (b, hash) blkId = do
  let difficulty = fromMaybe (error $ "missing value in difficulty map: " ++ format hash) $
                   M.lookup hash dm --  <- calcTotalDifficulty b blkId
  return (BlockDataRef pH uH cB sR tR rR lB d n gL gU t eD nc mH blkId hash True True difficulty) --- Horrible! Apparently I need to learn the Lens library, yesterday
  where
      bd = (blockBlockData b)
      pH = blockDataParentHash bd
      uH = blockDataUnclesHash bd
      cB = blockDataCoinbase bd
      sR = blockDataStateRoot bd
      tR = blockDataTransactionsRoot bd
      rR = blockDataReceiptsRoot bd
      lB = blockDataLogBloom bd
      n =  blockDataNumber bd
      d  = blockDataDifficulty bd
      gL = blockDataGasLimit bd
      gU = blockDataGasUsed bd
      t  = blockDataTimestamp bd
      eD = blockDataExtraData bd
      nc = blockDataNonce bd
      mH = blockDataMixHash bd
      
getBlock::(HasSQLDB m, MonadResource m, MonadBaseControl IO m)=>
          SHA->m (Maybe Block)
getBlock h = do
  db <- getSQLDB
  entBlkL <- runResourceT $
    SQL.runSqlPool actions db

  case entBlkL of
    [] -> return Nothing
    lst -> return $ Just . entityVal . head $ lst
  where actions = E.select $ E.from $ \(bdRef, block) -> do
                                   E.where_ ( (bdRef E.^. BlockDataRefHash E.==. E.val h ) E.&&. ( bdRef E.^. BlockDataRefBlockId E.==. block E.^. BlockId ))
                                   return block                        

getDifficulties::HasSQLDB m=>[SHA]->m [(SHA, Integer)]
getDifficulties hashes = do
  db <- getSQLDB
  blocks <-
    runResourceT $
    flip SQL.runSqlPool db $ 
    E.select $
    E.from $ \bd -> do
      E.where_ ((bd E.^. BlockDataRefHash) `E.in_` E.valList hashes)
      return (bd E.^. BlockDataRefHash, bd E.^. BlockDataRefTotalDifficulty)
      
  return $ map f blocks

  where
    f::(E.Value SHA, E.Value Integer)->(SHA, Integer)
    f (h, a) = (E.unValue h, E.unValue a)

addDifficulties::M.Map SHA Integer->[(SHA, Integer, SHA)]->M.Map SHA Integer
addDifficulties dm [] = dm
addDifficulties dm ((hash, blockDifficulty, parentHash):rest) = 
  let parentDifficulty = fromMaybe (error $ "missing hash in difficulty map in addDifficulties: " ++ format parentHash ++ ", hash=" ++ format hash) $ M.lookup parentHash dm
      dm' = M.insert hash (parentDifficulty + blockDifficulty) dm
  in addDifficulties dm' rest

getDifficultyMap::HasSQLDB m=>
                  [(Block, SHA)]->m (M.Map SHA Integer)
getDifficultyMap blocksAndHashes = do
  let hashes = S.fromList $ map snd blocksAndHashes
      parents = S.fromList $ map (blockDataParentHash . blockBlockData . fst) blocksAndHashes

  dm' <- fmap (M.fromList . ((SHA 0, 0):)) $ getDifficulties (S.toList $ parents S.\\ hashes)

  return $ addDifficulties dm'
    (map (\(x, y) ->
           (y,
            blockDataDifficulty $ blockBlockData x,
            blockDataParentHash $ blockBlockData x)
         ) blocksAndHashes)


putBlocks::(HasSQLDB m, MonadResource m, MonadBaseControl IO m, MonadThrow m)=>
           [Block]->m [(Key Block, Key BlockDataRef)]
putBlocks blocks = do
  let blocksAndHashes = map (\b -> (b, blockHash b)) blocks
  dm <- getDifficultyMap blocksAndHashes
  db <- getSQLDB
  runResourceT $
    flip SQL.runSqlPool db $
    forM blocksAndHashes $ actions dm
      
  where actions dm (b, hash) = do
          blkId <- SQL.insert $ b                      
          toInsert <- lift $ lift $ blk2BlkDataRef dm (b, hash) blkId
          mapM_ (insertOrUpdate b blkId) ((map (\tx -> tx2RawTX tx blkId (blockDataNumber (blockBlockData b)))  (blockReceiptTransactions b)))
          blkDataRefId <- SQL.insert $ toInsert
          _ <- SQL.insert $ Unprocessed blkId
          return $ (blkId, blkDataRefId)

        insertOrUpdate b blkid rawTX  = do
            (txId :: [Entity RawTransaction])
                 <- SQL.selectList [ RawTransactionTxHash SQL.==. (rawTransactionTxHash rawTX )]
                                   [ ]
            case txId of
                [] -> do
                      _ <- SQL.insert rawTX
                      return ()
                lst -> mapM_ (\t -> SQL.update (SQL.entityKey t)
                                              [ RawTransactionBlockId SQL.=. blkid, 
                                                RawTransactionBlockNumber SQL.=. (fromIntegral $ blockDataNumber (blockBlockData b)) ])
                             lst

instance Format Block where
  format b@Block{blockBlockData=bd, blockReceiptTransactions=receipts, blockBlockUncles=uncles} =
    CL.blue ("Block #" ++ show (blockDataNumber bd)) ++ " " ++
    tab (format (blockHash b) ++ "\n" ++
         format bd ++
         (if null receipts
          then "        (no transactions)\n"
          else tab (intercalate "\n    " (format <$> receipts))) ++
         (if null uncles
          then "        (no uncles)"
          else tab ("Uncles:" ++ tab ("\n" ++ intercalate "\n    " (format <$> uncles)))))
              
instance RLPSerializable Block where
  rlpDecode (RLPArray [bd, RLPArray transactionReceipts, RLPArray uncles]) =
    Block (rlpDecode bd) (rlpDecode <$> transactionReceipts) (rlpDecode <$> uncles)
  rlpDecode (RLPArray arr) = error ("rlpDecode for Block called on object with wrong amount of data, length arr = " ++ show arr)
  rlpDecode x = error ("rlpDecode for Block called on non block object: " ++ show x)

  rlpEncode Block{blockBlockData=bd, blockReceiptTransactions=receipts, blockBlockUncles=uncles} =
    RLPArray [rlpEncode bd, RLPArray (rlpEncode <$> receipts), RLPArray $ rlpEncode <$> uncles]

instance RLPSerializable BlockData where
  rlpDecode (RLPArray [v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15]) =
    BlockData {
      blockDataParentHash = rlpDecode v1,
      blockDataUnclesHash = rlpDecode v2,
      blockDataCoinbase = rlpDecode v3,
      blockDataStateRoot = rlpDecode v4,
      blockDataTransactionsRoot = rlpDecode v5,
      blockDataReceiptsRoot = rlpDecode v6,
      blockDataLogBloom = rlpDecode v7,
      blockDataDifficulty = rlpDecode v8,
      blockDataNumber = rlpDecode v9,
      blockDataGasLimit = rlpDecode v10,
      blockDataGasUsed = rlpDecode v11,
      blockDataTimestamp = posixSecondsToUTCTime $ fromInteger $ rlpDecode v12,
      blockDataExtraData = rlpDecode v13,
      blockDataMixHash = rlpDecode v14,
      blockDataNonce = bytesToWord64 $ B.unpack $ rlpDecode v15
      }  
  rlpDecode (RLPArray arr) = error ("Error in rlpDecode for Block: wrong number of items, expected 15, got " ++ show (length arr) ++ ", arr = " ++ show (pretty arr))
  rlpDecode x = error ("rlp2BlockData called on non block object: " ++ show x)


  rlpEncode bd =
    RLPArray [
      rlpEncode $ blockDataParentHash bd,
      rlpEncode $ blockDataUnclesHash bd,
      rlpEncode $ blockDataCoinbase bd,
      rlpEncode $ blockDataStateRoot bd,
      rlpEncode $ blockDataTransactionsRoot bd,
      rlpEncode $ blockDataReceiptsRoot bd,
      rlpEncode $ blockDataLogBloom bd,
      rlpEncode $ blockDataDifficulty bd,
      rlpEncode $ blockDataNumber bd,
      rlpEncode $ blockDataGasLimit bd,
      rlpEncode $ blockDataGasUsed bd,
      rlpEncode (round $ utcTimeToPOSIXSeconds $ blockDataTimestamp bd::Integer),
      rlpEncode $ blockDataExtraData bd,
      rlpEncode $ blockDataMixHash bd,
      rlpEncode $ B.pack $ word64ToBytes $ blockDataNonce bd
      ]

blockHash::Block->SHA
blockHash (Block info _ _) = hash . rlpSerialize . rlpEncode $ info

instance Format BlockData where
  format b = 
    "parentHash: " ++ format (blockDataParentHash b) ++ "\n" ++
    "unclesHash: " ++ format (blockDataUnclesHash b) ++ 
    (if blockDataUnclesHash b == hash (B.pack [0xc0]) then " (the empty array)\n" else "\n") ++
    "coinbase: " ++ show (pretty $ blockDataCoinbase b) ++ "\n" ++
    "stateRoot: " ++ format (blockDataStateRoot b) ++ "\n" ++
    "transactionsRoot: " ++ format (blockDataTransactionsRoot b) ++ "\n" ++
    "receiptsRoot: " ++ format (blockDataReceiptsRoot b) ++ "\n" ++
    "difficulty: " ++ show (blockDataDifficulty b) ++ "\n" ++
    "gasLimit: " ++ show (blockDataGasLimit b) ++ "\n" ++
    "gasUsed: " ++ show (blockDataGasUsed b) ++ "\n" ++
    "timestamp: " ++ show (blockDataTimestamp b) ++ "\n" ++
    "extraData: " ++ show (pretty $ blockDataExtraData b) ++ "\n" ++
    "nonce: " ++ showHex (blockDataNonce b) "" ++ "\n"

