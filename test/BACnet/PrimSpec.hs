module BACnet.PrimSpec where

import Test.Hspec
import Test.QuickCheck
import BACnet.Prim
import Control.Exception (evaluate)

spec =
  do
    describe "testBit" $ do
      context "when given (BitString 0 [0xF0])" $ do
        let bs = bitString 0 [0xF0]

        it "returns True for bit position 0" $
          testBit bs 0 `shouldBe` True

        it "returns True for bit position 3" $
          testBit bs 3 `shouldBe` True

        it "returns False for bit position 4" $
          testBit bs 4 `shouldBe` False

        it "returns False for bit position 7" $
          testBit bs 7 `shouldBe` False

      context "when given empty" $
        it "gives an error for any bit position" $
          property $ \n -> evaluate (testBit empty n) `shouldThrow` anyErrorCall

      context "when given (BitString 3 [0x00, 0x88])" $ do
        let bs = bitString 3 [0x00, 0x88]

        it "gives an error for pit position 13" $
          evaluate (testBit bs 13) `shouldThrow` anyErrorCall

        it "gives an error for bit position > 12" $
          property $ forAll
            (choose (13, maxBound))
            (\n -> evaluate (testBit bs n) `shouldThrow` anyErrorCall)
