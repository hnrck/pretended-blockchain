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
  , Transactions
  , addTransaction
  , transactionContext
  , transactionsContext
  ) where

import           Data.ByteString       (ByteString)
import           Data.ByteString.Char8 (pack)
import           Data.Maybe            (Maybe (..))

-- | Transactions is a list of transction, potentially empty.
type Transactions = [Transaction]

-- | A transaction is a contract from someone to someone else, of a certain
-- amount.
data Transaction = Transaction
  { from   :: String
  , to     :: String
  , amount :: Float
  } deriving (Show, Eq)

-- | Add a transaction to transactions
addTransaction :: Transaction -> Transactions -> Transactions
addTransaction t ts = t : ts

-- | Generate the transaction context for hash.
-- | TODO(hnrck) Removing the Maybe when no more used.
transactionContext :: Maybe Transaction -> [ByteString]
transactionContext Nothing = []
transactionContext (Just (Transaction f t a)) = [pack f, pack t, pack $ show a]

-- | Generate the transactions context for hash.
transactionsContext :: Transactions -> [ByteString]
transactionsContext []  = []
transactionsContext txs = foldr ((++) . transactionContext . Just) [] txs
