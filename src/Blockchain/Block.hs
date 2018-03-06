{-|
Module : Blockchain.Block
Description : Implementation of the blockchain's blocks
Copyright : (c) Henrick Deschamps, 2018
License : MIT
Maintainer: henrick.deschamps@gmail.com
Stability : experimental

This module is used to implement the blockchain's block in Haskell. For now, a
block is comprised of an header, and maybe a transaction.
The block hash, in the header, is computed at creation.
-}
module Blockchain.Block
  ( Block(..)
  , genesisBlock
  , mineBlock
  ) where

import           Data.Maybe                   (Maybe (..))

import           Blockchain.Block.Header      (Header (..))
import           Blockchain.Block.Transaction (Transaction (..))

-- | The blockchain's block. A block can be printed or tested for equality. 
-- The header is computed at creation, and the transaction is not mandatory.
data Block = Block
  { header      :: Header 
  , transaction :: Maybe Transaction
  } deriving (Show, Eq)

-- | Generate the first block of a blockchain. The transaction is not mandatory
-- and can be left to nothing.
genesisBlock :: Maybe Transaction -> Block
genesisBlock = undefined

-- | Mine a new block using a previous one. The transaction is not mandatory
-- and can be left to nothing.
mineBlock :: Block -> Maybe Transaction -> Block
mineBlock = undefined
