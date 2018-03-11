module TransactionSpec where

import           Data.ByteString.Char8        (pack, unpack)
import           Test.Hspec                   (Spec (..), describe, it,
                                               shouldBe)
import           Test.QuickCheck              (property)

import           Blockchain.Block.Transaction (Transaction (..), Transactions,
                                               addTransaction,
                                               transactionContext,
                                               transactionsContext)

spec :: Spec
spec =
  describe "Blockchain.Block.Transaction" $ do
    it "Static check transaction context of nothing" $ -- do
      transactionContext Nothing `shouldBe` []
    it "Empty transactions context is empty" $ -- do
      transactionsContext [] `shouldBe` []
    it "Transaction context is determinist (modulo String packing)" $ -- do
      property $ \f t a ->
        (f /= unpack (pack f)) ||
        (t /= unpack (pack t)) ||
        fmap unpack (transactionContext (Just (Transaction f t a))) ==
        [f, t, show a]
    it "Adding a transaction to empty transactions makes it not empty" $ -- do
      property $ \f t a -> addTransaction (Transaction f t a) [] /= []
    it
      "Transactions context of two transactions is the concatenation of their contexts" $ -- do
      property $ \f1 f2 t1 t2 a1 a2 ->
        transactionsContext
          (addTransaction
             (Transaction f1 t1 a1)
             (addTransaction (Transaction f2 t2 a2) [])) ==
        transactionContext (Just (Transaction f1 t1 a1)) ++
        transactionContext (Just (Transaction f2 t2 a2))
