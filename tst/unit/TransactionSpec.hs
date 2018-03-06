module TransactionSpec where

import           Data.ByteString.Char8        (pack, unpack)
import           Test.Hspec                   (Spec (..), describe, it,
                                               shouldBe)
import           Test.QuickCheck              (property)

import           Blockchain.Block.Transaction (Transaction (..),
                                               transactionContext)

spec :: Spec
spec =
  describe "Blockchain.Block.Transaction" $ do
    it "Static check transaction context of nothing" $ -- do
      transactionContext Nothing `shouldBe` []
    it "Transaction context is determinist (modulo String packing)" $ -- do
      property $ \f t a ->
        (f /= unpack (pack f)) ||
        (t /= unpack (pack t)) ||
        fmap unpack (transactionContext (Just (Transaction f t a))) ==
        [f, t, show a]
