module BACnet.ReaderSpec where

import Test.HUnit
import Test.Hspec
import Test.QuickCheck
import BACnet.Reader
import Data.Word


spec :: Spec
spec = do
  describe "readNullAP" $
    it "reads [0x00] and returns Just((), [])" $ do
      runReader readNullAP [0x00] `shouldBe` Just((),[])

  describe "readBoolAP" $
    it "reads [0x10] and returns Just(False, [])" $
      runReader readBoolAP [0x10] `shouldBe` Just(False, [])

  describe "readBoolAP" $
    it "reads [0x11] and returns Just(True, [])" $
      runReader readBoolAP [0x11] `shouldBe` Just(True, [])

  describe "readUnsignedAP" $ do
    it "reads [0x21, 0x00] and returns Just(0, [])" $
      runReader readUnsignedAP [0x21, 0x00] `shouldBe` Just(0, [])

    it "reads [0x22, 0x01, 0x00] and returns Just(256, [])" $
      runReader readUnsignedAP [0x22, 0x01, 0x00] `shouldBe` Just(256, [])

    it "reads [0x22, 0xFF, 0xFF] and returns Just(65535, [])" $
      runReader readUnsignedAP [0x22, 0xFF, 0xFF] `shouldBe` Just(65535, [])

    it "reads [0x23, 0x01, 0x00, 0x00] and returns Just(65536, [])" $
      runReader readUnsignedAP [0x23, 0x01, 0x00, 0x00] `shouldBe`
        Just(65536, [])

    it "reads [0x23, 0xFF, 0xFF, 0xFF] and returns Just(16646655, [])" $
      runReader readUnsignedAP [0x23, 0xFF, 0xFF, 0xFF] `shouldBe`
        Just(16777215, [])
