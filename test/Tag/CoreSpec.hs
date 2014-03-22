module Tag.CoreSpec where

import Test.Hspec
import Test.QuickCheck hiding ((.&.))
import Data.Bits
import Tag.Core

spec :: Spec
spec = do
  describe "isAP" $ do
    it "returns True for 0x00" $
      isAP 0x00 `shouldBe` True
    it "returns False for 0x08" $
      isAP 0x08 `shouldBe` False
    it "returns True when 3rd bit of a byte is not set" $
      property $
      forAll (choose (0,255) `suchThat` (\b -> not $ testBit b 3))
      (\b -> isAP b `shouldBe` True)
    it "returns False when 3rd bit of a byte is set" $
      property $
      forAll (choose (0,255) `suchThat` (`testBit` 3))
      (\b -> isAP b `shouldBe` False)

  describe "isCS" $
    it "is equivalent to not . isAP" $
      property (\b -> isCS b `shouldBe` not (isAP b))

  describe "lvt" $ do
    it "returns 0 with input 0xF0" $ lvt 0xF0 `shouldBe` 0
    it "returns 1 with input 0x01" $ lvt 0x01 `shouldBe` 1
    it "returns 2 with input 0xa2" $ lvt 0xa2 `shouldBe` 2
    it "returns 4 with input 0xbc" $ lvt 0xbc `shouldBe` 4
    it "returns 7 with input 0xcf" $ lvt 0xcf `shouldBe` 7

    it "is equivalent to (.&. 0x07)" $
      property (\b -> lvt b `shouldBe` (b .&. 0x07))

  describe "isOpen" $
    it "returns the same value as isCS b && (lvt b == openType)" $
      property (\b -> isOpen b `shouldBe` (isCS b && lvt b == openType))

  describe "isClose" $
    it "returns the same value as isCS b && (lvt b == closeType)" $
      property (\b -> isClose b `shouldBe` (isCS b && lvt b == closeType))

  describe "isExtendedLength" $
    it "returns True if lvt == 5, else False" $
      property (\b -> isExtendedLength b `shouldBe` (lvt b == 5))
