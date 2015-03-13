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

module Blockchain.Data.AddressStateDB (
  blankAddressState,
  getAddressState,
  getAllAddressStates,
  putAddressState,
  deleteAddressState,
  addressStateExists
) where 

import Database.Persist
import Database.Persist.Types
import Database.Persist.TH


import Blockchain.DBM
import Blockchain.Data.Address
import qualified Blockchain.Colors as CL

import Blockchain.ExtDBs
import Blockchain.Format
import Blockchain.Data.RLP
import Blockchain.SHA
import Blockchain.Data.SignedTransaction
import Blockchain.Util
import Blockchain.Data.Block
import Blockchain.Data.BlockDB
import Blockchain.Data.AddressState

import Data.Binary
import qualified Data.ByteString.Lazy as BL
import Data.Functor
import Data.List
import Numeric
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))


import qualified Data.NibbleString as N


blankAddressState::AddressState
blankAddressState = AddressState { addressStateNonce=0, balance=0, contractRoot=emptyTriePtr, codeHash=hash "" }


instance Format AddressState where
  format a = CL.blue "AddressState" ++
             tab("\nnonce: " ++ showHex (addressStateNonce a) "" ++
                 "\nbalance: " ++ show (toInteger $ balance a) ++
                 "\ncontractRoot: " ++ show (pretty $ contractRoot a) ++
                 "\ncodeHash: " ++ show (pretty $ codeHash a))
  
instance RLPSerializable AddressState where
  --rlpEncode a | balance a < 0 = rlpEncode a{balance = - balance a}
  rlpEncode a | balance a < 0 = error $ "Error in cal to rlpEncode for AddressState: AddressState has negative balance: " ++ format a
  rlpEncode a = RLPArray [
    rlpEncode $ toInteger $ addressStateNonce a,
    rlpEncode $ toInteger $ balance a,
    rlpEncode $ contractRoot a,
    rlpEncode $ codeHash a
                ]

  rlpDecode (RLPArray [n, b, cr, ch]) =
    AddressState {
      addressStateNonce=fromInteger $ rlpDecode n,
      balance=fromInteger $ rlpDecode b,
      contractRoot=rlpDecode cr,
      codeHash=rlpDecode ch
      } 
  rlpDecode x = error $ "Missing case in rlpDecode for AddressState: " ++ show (pretty x)

addressAsNibbleString::Address->N.NibbleString
addressAsNibbleString (Address s) = N.EvenNibbleString $ BL.toStrict $ encode s

getBlock::SHA->DBM (Maybe Block)
getBlock h = 
  fmap (rlpDecode . rlpDeserialize) <$> blockDBGet (BL.toStrict $ encode h)

putBlock::Block->DBM ()
putBlock b = do
  let bytes = rlpSerialize $ rlpEncode b
  blockDBPut (BL.toStrict $ encode $ blockHash b) bytes

getAddressState::Address->DBM AddressState
getAddressState address = do
  states <- getKeyVals $ addressAsNibbleString address
  case states of
    [] -> do
      putAddressState address blankAddressState
      return blankAddressState
    [state] -> return $ rlpDecode $ rlpDeserialize $ rlpDecode $ snd state
    _ -> error ("getAddressStates found multiple states for: " ++ show (pretty address) ++ "\n" ++ intercalate "\n" (show . pretty <$> states))
  

getAllAddressStates::DBM [(N.NibbleString, AddressState)]
getAllAddressStates = do
  states <- getKeyVals ""
  return $ fmap (rlpDecode . rlpDeserialize . rlpDecode) <$> states

putAddressState::Address->AddressState->DBM ()
putAddressState address newState = 
  putKeyVal (addressAsNibbleString address) $ rlpEncode $ rlpSerialize $ rlpEncode newState

deleteAddressState::Address->DBM ()
deleteAddressState address = 
  deleteKey (addressAsNibbleString address)

addressStateExists::Address->DBM Bool
addressStateExists address = 
  keyExists (addressAsNibbleString address)

