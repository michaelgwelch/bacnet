module TagSpec where

import Test.Hspec
import Test.QuickCheck
import Data.Bits
import Tag

spec :: Spec
spec = describe "isAP" $ do
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
    forAll (choose (0,255) `suchThat` (\b -> testBit b 3))
    (\b -> isAP b `shouldBe` False)
