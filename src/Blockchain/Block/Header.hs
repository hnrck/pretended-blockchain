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
  , newHeader
  , setHash
  , headerContext
  , genesisHeader
  ) where

import           Data.ByteString       (ByteString)
import           Data.ByteString.Char8 (pack)

-- | The header, containing the previous hash, timestamp, and current hash,
-- computed at block creation.
data Header = Header
  { previousHash :: ByteString
  , timestamp    :: !LogicalTime
  , hash         :: ByteString
  } deriving (Show, Eq)

-- | Headers are timestamped with logical time.
type LogicalTime = Int

-- | generate a new header with a generic hash before hash computation.
newHeader :: ByteString -> LogicalTime -> Header
newHeader h t = Header h t (pack "")

-- | Set the hash of an header. Hash has to be computed beforehand.
setHash :: Header -> ByteString -> Header
setHash (Header h t _) = Header h t

-- | Generate the header's context for hash generation.
headerContext :: Header -> [ByteString]
headerContext (Header h t _) = [h, pack $ show t]

-- | Default hash, used for genesis block.
defaultHash :: ByteString
defaultHash = pack "00000000000000000000000000000000"

-- | The header of the first blockchain's block. Previous hash is the default
-- one  and logical time is set to 0.
genesisHeader :: Header
genesisHeader = newHeader defaultHash 0
