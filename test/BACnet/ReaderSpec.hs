module BACnet.ReaderSpec where

import Test.HUnit
import Test.Hspec
import Test.QuickCheck
import BACnet.Reader
import Data.Word


spec :: Spec
spec = do
  describe "readNullAP" $
    it "reads [0x00] and returns Just((), [])" $
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

    it "reads [0x25, 0x05, 0x01, 0x00, 0x00, 0x00, 0x00, x0ff] and returns Just(4294967296, [0xff])" $
      runReader readUnsignedAP [0x25, 0x05, 0x01, 0x00, 0x00, 0x00, 0x00, 0xff] `shouldBe`
        Just(4294967296, [0xff])

  describe "readSignedAP" $ do
    it "reads [0x31, 0x00] and returns Just(0, [])" $
      runReader readSignedAP [0x31, 0x00] `shouldBe` Just(0, [])

    it "reads [0x31, 0xFF] and returns Just(-1, [])" $
      runReader readSignedAP [0x31, 0xFF] `shouldBe` Just(-1, [])

    it "reads [0x31, 0xFE] and returns Just(-2, [])" $
      runReader readSignedAP [0x31, 0xFE] `shouldBe` Just(-2, [])

  describe "readFloatAP" $
    it "reads [0x44, 0x00, 0x00, 0x00, 0x01] and returns Just(1.4e-45, [])" $
      runReader readRealAP [0x44, 0x00, 0x00, 0x00, 0x01] `shouldBe`
        Just(1.4e-45, [])


  describe "readOctetStringAP" $
    it ("reads [0x65, 0x08, 0x01, 0x02, 0xa0, 0xb0, 0xc0, 0xd0, 0xe0, 0xff, 0xee] and " ++
       "returns Just([0x01, 0x02, 0xa0, 0xb0, 0xc0, 0xd0, 0xe0, 0xf0], [0xee])") $
       runReader readOctetStringAP [0x65, 0x08, 0x01, 0x02, 0xa0, 0xb0, 0xc0, 0xd0, 0xe0, 0xff, 0xee]
       `shouldBe` Just([0x01, 0x02, 0xa0, 0xb0, 0xc0, 0xd0, 0xe0, 0xff], [0xee])
