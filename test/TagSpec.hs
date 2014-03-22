module TagSpec where

import Tag
import Test.QuickCheck
import Test.Hspec

spec :: Spec
spec = do
  describe "readTag" $ do
    it "returns Just(apNullTag, bs) on input: 0x00 : bs" $
      property $ (\bs -> runReader readTag (0x00 : bs) `shouldBe`
      Just(apNullTag, bs))
    it "returns Nothing for input that is empty" $
      runReader readTag [] `shouldBe` Nothing
    it "returns Just(apTrueTag, bs) on input: 0x11 : bs" $
      property $ (\bs -> runReader readTag (0x11 : bs) `shouldBe`
      Just(apTrueTag, bs))
