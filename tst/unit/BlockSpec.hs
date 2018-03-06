module BlockSpec where

import           Data.ByteString.Char8        (unpack)
import           Test.Hspec                   (Spec (..), describe, it,
                                               shouldBe)
import           Test.QuickCheck              (property)

import           Blockchain.Block             (Block (..), genesisBlock,
                                               mineBlock)
import           Blockchain.Block.Header      (Header (..))
import           Blockchain.Block.Transaction (Transaction (..))

blockHashToString :: Block -> String
blockHashToString (Block (Header _ _ h) _) = unpack h

spec :: Spec
spec =
  describe "Blockchain.Block" $ do
    it "Static check hash of genesisBlock Nothing" $ -- do
      blockHashToString (genesisBlock Nothing) `shouldBe`
      "T\240Z\135\245\184\129x\f\220@\227\253\223\235\247.;\167\229\246T\ENQ\171\DC2\FS\DEL\"\217\132\154\180"
    it "Static check hash of mineBlock Nothing on genesisBlock Nothing" $ -- do
      blockHashToString (mineBlock (genesisBlock Nothing) Nothing) `shouldBe`
      "\235D\198D\SYN=b>9\206\GS\253\191\201\&2\DEL0\207?6n\238\159\191\210\161\190\ACK\186!\172\159"
    it "Hash is determinist" $ -- do
      property $ \f t a ->
        genesisBlock (Just (Transaction f t a)) ==
        genesisBlock (Just (Transaction f t a))
    it "Blocks with different amounts have different hashes" $ -- do
      property $ \f t a x ->
        a == x ||
        genesisBlock (Just (Transaction f t a)) /=
        genesisBlock (Just (Transaction f t x))
    it "Blocks with different senders have different hashes" $ -- do
      property $ \f t a x ->
        f == x ||
        genesisBlock (Just (Transaction f t a)) /=
        genesisBlock (Just (Transaction x t a))
    it "Blocks with different receivers have different hashes" $ -- do
      property $ \f t a x ->
        t == x ||
        genesisBlock (Just (Transaction f t a)) /=
        genesisBlock (Just (Transaction f x a))
