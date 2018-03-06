{-|
Module : Blockchain.Block.Transaction
Description : Implementation of the blockchain's block transaction.
Copyright : (c) Henrick Deschamps, 2018
License : MIT
Maintainer: henrick.deschamps@gmail.com
Stability : experimental

This module is used to implement the transaction that can be packed in a
blockchain's block, in Haskell.
-}
module Blockchain.Block.Transaction
  ( Transaction(..)
  , transactionContext
  ) where

import           Data.ByteString       (ByteString)
import           Data.ByteString.Char8 (pack)
import           Data.Maybe            (Maybe (..))

-- | A transaction is a contract from someone to someone else, of a certain
-- amount.
data Transaction = Transaction
  { from   :: String
  , to     :: String
  , amount :: Float
  } deriving (Show, Eq)

-- | Generate the transaction context for hash.
transactionContext :: Maybe Transaction -> [ByteString]
transactionContext Nothing = []
transactionContext (Just (Transaction f t a)) = [pack f, pack t, pack $ show a]
