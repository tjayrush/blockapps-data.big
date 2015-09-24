{-# LANGUAGE OverloadedStrings, TupleSections, TypeSynonymInstances, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Blockchain.Data.GenesisInfo (
  GenesisInfo(..)
  ) where

import Control.Applicative
import Data.Aeson
import qualified Data.ByteString as B
import Data.Time
import Data.Word

import Blockchain.Data.Address
import Blockchain.MiscJSON
import Blockchain.SHA
import Blockchain.Database.MerklePatricia

data GenesisInfo =
  GenesisInfo {
    genesisInfoParentHash::SHA,
    genesisInfoUnclesHash::SHA,
    genesisInfoCoinbase::Address,
    genesisInfoAccountInfo::[(Address, Integer)],
    genesisInfoTransactionsRoot::SHAPtr,
    genesisInfoReceiptsRoot::SHAPtr,
    genesisInfoLogBloom::B.ByteString,
    genesisInfoDifficulty::Integer,
    genesisInfoNumber::Integer,
    genesisInfoGasLimit::Integer,
    genesisInfoGasUsed::Integer,
    genesisInfoTimestamp::UTCTime,
    genesisInfoExtraData::Integer,
    genesisInfoMixHash::SHA,
    genesisInfoNonce::Word64
} deriving (Show)

instance FromJSON GenesisInfo where
  parseJSON (Object o) =
    GenesisInfo <$>
    o .: "parentHash" <*>
    (return $ SHA 0xc0) <*> --UnclesHash"
    o .: "coinbase" <*>
    o .: "AccountInfo" <*>
    (return emptyTriePtr) <*> --TransactionsRoot
    (return emptyTriePtr) <*> --ReceiptsRoot
    (return $ B.replicate 256 0) <*> --LogBloom
    o .: "difficulty" <*>
    (return 0) <*> --number
    o .: "gasLimit" <*>
    (return 0) <*> --gasUsed
    o .: "timestamp" <*>
    o .: "extraData" <*>
    o .: "mixhash" <*>
    o .: "nonce"
  parseJSON _ = undefined
  
instance ToJSON GenesisInfo where
  toJSON x =
    object [
      "parentHash" .= genesisInfoParentHash x,
      "unclesHash" .= genesisInfoUnclesHash x,
      "coinbase" .= genesisInfoCoinbase x,
      "accountInfo" .= genesisInfoAccountInfo x,
      "transactionRoot" .= genesisInfoTransactionsRoot x,
      "receiptsRoot" .= genesisInfoReceiptsRoot x,
      "logBloom" .= genesisInfoLogBloom x,
      "difficulty" .= genesisInfoDifficulty x,
      "number" .= genesisInfoNumber x,
      "gasLimit" .= genesisInfoGasLimit x,
      "gasUsed" .= genesisInfoGasUsed x,
      "timestamp" .= genesisInfoTimestamp x,
      "extraData" .= genesisInfoExtraData x,
      "mixHash" .= genesisInfoMixHash x,
      "nonce" .= genesisInfoNonce x
      ]
