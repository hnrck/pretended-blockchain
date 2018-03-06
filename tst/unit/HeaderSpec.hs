module HeaderSpec where

import           Data.ByteString.Char8   (pack, unpack)
import           Test.Hspec              (Spec (..), describe, it, shouldBe)
import           Test.QuickCheck         (property)

import           Blockchain.Block.Header (Header (..), genesisHeader,
                                          headerContext, newHeader, setHash)

spec :: Spec
spec =
  describe "Blockchain.Block.Header" $ -- do
  it "Static check genesis header" $ -- do
  genesisHeader `shouldBe`
  Header (pack "00000000000000000000000000000000") 0 (pack "")
