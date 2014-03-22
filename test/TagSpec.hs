module TagSpec where

import Test.Hspec
import Test.QuickCheck
import Data.Bits
import Tag

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
    it "returns the same value as not . isAP" $
      property (\b -> isCS b `shouldBe` not (isAP b))
