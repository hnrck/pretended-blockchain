{-|
Module : Blockchain
Description : Implementation of the blockchain
Copyright : (c) Henrick Deschamps, 2018
License : MIT
Maintainer: henrick.deschamps@gmail.com
Stability : experimental

This module is used to implement the blockchain in Haskell using StateT.
-}
module Blockchain
  ( runBlockchain
  ) where

import           Control.Monad.State          (StateT (..), evalStateT, forever,
                                               get, lift, put)
import           Data.Maybe                   (Maybe (..), fromJust, isNothing)
import           System.IO                    (BufferMode (NoBuffering),
                                               hSetBuffering, stdout)
import           Text.Read                    (readMaybe)

import           Blockchain.Block             (Block (..), genesisBlock,
                                               mineBlock)
import           Blockchain.Block.Transaction (Transaction (..))

-- | The blockchain, simply a 'chain' of Block.
type Blockchain = [Block]

-- | Append a block to the blockchain. Might take a transaction or not.
appendBlock :: Monad m => Maybe Transaction -> StateT Blockchain m Blockchain
appendBlock tx = do
  bc <- get
  -- Mine a new block, and append it to the blockchain.
  let bc' = mineBlock (head bc) tx : bc
  put bc'
  return bc'

-- | The blockchain routine. Asks the user for information in order to build a
-- transaction. If the information are compromised, the build block use nothing.
updateBlockchain :: StateT Blockchain IO ()
updateBlockchain =
  forever $
    -- Get information for a transaction.
   do
    lift $ putStrLn "Mining new block..."
    lift $ putStrLn "Creating transaction"
    lift $ putStr "\tfrom:   "
    lineFrom <- lift getLine
    let f = lineFrom
    lift $ putStr "\tto:     "
    lineTo <- lift getLine
    let t = lineTo
    lift $ putStr "\tamount: "
    lineAmount <- lift getLine
    let a = readMaybe lineAmount :: Maybe Float
    -- build a transaction. If the information get are compromised, build
    -- nothing.
    let tx =
          if isNothing a
            then Nothing
            else Just $ Transaction f t (fromJust a)
    -- Append a new block to the blockchain.
    bc <- appendBlock tx
    lift . putStrLn $ show bc
    return ()

-- | Launch the blockchain routine, building a genesis block with a first
-- transaction, or nothing.
runBlockchain :: Maybe Transaction -> IO ()
runBlockchain tx = do
  hSetBuffering stdout NoBuffering -- Do not remove, might break the output
  evalStateT updateBlockchain [genesisBlock tx]
