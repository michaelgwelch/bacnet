module BACnet.WriterSpec where

import Test.Hspec
import Test.QuickCheck
import BACnet.Writer

spec :: Spec
spec =
  do
    describe "writeNullAP" $
      it "writes [0x00]" $
        runW writeNullAP `shouldBe` [0x00]

    describe "writeBoolAP" $ do
      it "returns [0x10] for Bool input False" $
        runW (writeBoolAP False) `shouldBe` [0x10]

      it "returns [0x11] for Bool input True" $
        runW (writeBoolAP True) `shouldBe` [0x11]
