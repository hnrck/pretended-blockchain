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

import qualified Crypto.Hash.SHA256           as SHA256
import           Data.ByteString              (ByteString)
import           Data.Maybe                   (Maybe (..))

import           Blockchain.Block.Header      (Header (..), genesisHeader,
                                               headerContext, newHeader,
                                               setHash)
import           Blockchain.Block.Transaction (Transaction (..),
                                               transactionContext)

-- | The blockchain's block. A block can be printed or tested for equality. 
-- The header is computed at creation, and the transaction is not mandatory.
data Block = Block
  { header      :: Header 
  , transaction :: Maybe Transaction
  } deriving (Show, Eq)

-- | Generate the context of the block, from it header and transaction's
-- contexts.
blockContext :: Block -> [ByteString]
blockContext (Block hdr tx) = headerContext hdr ++ transactionContext tx

-- | Compute the block hash from its context.
computeHash :: Block -> ByteString
computeHash = SHA256.finalize . SHA256.updates SHA256.init . blockContext

-- | Hash a block.
hashBlock :: Block -> Block
hashBlock (Block hdr tx) = Block (setHash hdr (computeHash (Block hdr tx))) tx

-- | Generate the first block of a blockchain. The transaction is not mandatory
-- and can be left to nothing.
genesisBlock :: Maybe Transaction -> Block
genesisBlock = hashBlock . Block genesisHeader

-- | Mine a new block using a previous one. The transaction is not mandatory
-- and can be left to nothing.
mineBlock :: Block -> Maybe Transaction -> Block
mineBlock (Block (Header _ ts h) _) = hashBlock . Block (newHeader h (ts + 1))
