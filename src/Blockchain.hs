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

import           Blockchain.Block.Transaction (Transaction (..))

-- | Launch the blockchain routine, building a genesis block with a first
-- transaction, or nothing.
runBlockchain :: Maybe Transaction -> IO ()
runBlockchain = undefined
