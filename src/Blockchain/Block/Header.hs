{-|
Module : Blockchain.Block.Header
Description : Implementation of the header in a block of the blockchain.
Copyright : (c) Henrick Deschamps, 2018
License : MIT
Maintainer: henrick.deschamps@gmail.com
Stability : experimental

This module is used to implement the blockchain's block header in Haskell.
-}
module Blockchain.Block.Header
  ( Header(..)
  ) where

import           Data.ByteString       (ByteString)

-- | The header, containing the previous hash, timestamp, and current hash,
-- computed at block creation.
data Header = Header
  { previousHash :: ByteString
  , timestamp    :: !LogicalTime
  , hash         :: ByteString
  } deriving (Show, Eq)

-- | Headers are timestamped with logical time.
type LogicalTime = Int
