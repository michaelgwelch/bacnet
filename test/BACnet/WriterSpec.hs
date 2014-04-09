module BACnet.WriterSpec where

import Test.Hspec
import Test.QuickCheck
import BACnet.Prim
import BACnet.Writer
import Numeric.Limits

spec :: Spec
spec =
  do
    describe "writeNullAP" $
      it "writes [0x00]" $
        runW writeNullAP `shouldBe` [0x00]

    describe "writeBoolAP" $ do
      it "writes [0x10] for Bool input False" $
        runW (writeBoolAP False) `shouldBe` [0x10]

      it "writes [0x11] for Bool input True" $
        runW (writeBoolAP True) `shouldBe` [0x11]

    describe "writeUnsignedAP" $ do
      it "writes [0x21, 0x00] for input 0" $
        runW (writeUnsignedAP 0) `shouldBe` [0x21, 0x00]

      it "writes [0x21, 0xFF] for input 255" $
        runW (writeUnsignedAP 255) `shouldBe` [0x21, 0xFF]

      it "writes [0x22, 0x01, 0x00] for input 256" $
        runW (writeUnsignedAP 256) `shouldBe` [0x22, 0x01, 0x00]

      it "writes [0x22, 0xFF, 0xFF] for input 65535" $
        runW (writeUnsignedAP 65535) `shouldBe` [0x22, 0xFF, 0xFF]

      it "writes [0x23, 0x01, 0x00, 0x00] for input 65536" $
        runW (writeUnsignedAP 65536) `shouldBe` [0x23, 0x01, 0x00, 0x00]

      it "writes [0x23, 0xFF, 0xFF, 0xFF] for input 16777215" $
        runW (writeUnsignedAP 16777215) `shouldBe` [0x23, 0xFF, 0xFF, 0xFF]

      it "writes [0x24, 0x01, 0x00, 0x00, 0x00] for input 16777216" $
        runW (writeUnsignedAP 16777216) `shouldBe` [0x24, 0x01, 0x00, 0x00, 0x00]

    describe "writeSignedAP" $ do
      it "writes [0x31, 0x00] for input 0" $
        runW (writeSignedAP 0) `shouldBe` [0x31, 0x00]

      it "writes [0x31, 0x7F] for input 127" $
        runW (writeSignedAP 127) `shouldBe` [0x31, 0x7F]

      it "writes [0x32, 0x00, 0x80] for input 128" $
        runW (writeSignedAP 128) `shouldBe` [0x32, 0x00, 0x80]

      it "writes [0x32, 0x7F, 0xFF] for input 32767" $
        runW (writeSignedAP 32767) `shouldBe` [0x32, 0x7F, 0xFF]

      it "writes [0x31, 0xFF] for input -1" $
        runW (writeSignedAP (-1)) `shouldBe` [0x31, 0xFF]

      it "writes [0x31, 0x80] for input -128" $
        runW (writeSignedAP (-128)) `shouldBe` [0x31, 0x80]

      it "writes [0x32, 0xFF, 0x7F] for input -129" $
        runW (writeSignedAP (-129)) `shouldBe` [0x32, 0xFF, 0x7F]

      it "writes [0x32, 0x80, 0x00] for input -32768" $
        runW (writeSignedAP (-32768)) `shouldBe` [0x32, 0x80, 0x00]

      it "writes [0x33, 0xFF, 0x7F, 0xFF] for input -32769" $
        runW (writeSignedAP (-32769)) `shouldBe` [0x33, 0xFF, 0x7F, 0xFF]

      it "writes [0x33, 0x80, 0x00, 0x00] for input -8388608" $
        runW (writeSignedAP (-8388608)) `shouldBe` [0x33, 0x80, 0x00, 0x00]

      it "writes [0x34, 0xFF, 0x7F, 0xFf, 0xFF] for input -8388609" $
        runW (writeSignedAP (-8388609)) `shouldBe` [0x34, 0xFF, 0x7F, 0xFF, 0xFF]

      it "writes [0x34, 0x80, 0x00, 0x00, 0x00] for input -2147483648" $
        runW (writeSignedAP (-2147483648)) `shouldBe`
          [0x34, 0x80, 0x00, 0x00, 0x00]

    describe "writeRealAP" $ do
      it "writes [0x44, 0x00, 0x00, 0x00, 0x01] for input  1.4e-45" $
        runW (writeRealAP  1.4e-45) `shouldBe` [0x44, 0x00, 0x00, 0x00, 0x01]

      it "writes [0x44, 0x7F, 0x7F, 0xFF, 0xFF] for input 3.4028235E38" $
        runW (writeRealAP 3.4028235E38) `shouldBe` [0x44, 0x7F, 0x7F, 0xFF, 0xFF]

      it "writes [0x44, 0xFF, 0x80, 0x00, 0x00] for input -infinity" $
        runW (writeRealAP (-1.0/0.0)) `shouldBe` [0x44, 0xFF, 0x80, 0x00, 0x00]

    describe "writeDoubleAP" $ do
      it "writes [0x55, 0x08, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01] for input 4.9E-324" $
        runW (writeDoubleAP 4.9E-324) `shouldBe` [0x55, 0x08, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01]

      it "writes [0x55, 0x08, 0x7F, 0xF0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00] for infinity" $
        runW (writeDoubleAP (1.0/0.0)) `shouldBe` [0x55, 0x08, 0x7F, 0xF0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]

    describe "writeOctetStringAP" $ do
      it "writes [0x60] for empty Octet string" $
        runW (writeOctetStringAP []) `shouldBe` [0x60]

      it "writes [0x61, 0xAA] for singleton string [0xAA]" $
        runW (writeOctetStringAP [0xAA]) `shouldBe` [0x61, 0xAA]

      it "writes [0x65, 0x06, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06] for string [0x01, 0x02, 0x03, 0x04, 0x05, 0x06]" $
        runW (writeOctetStringAP [0x01, 0x02, 0x03, 0x04, 0x05, 0x06]) `shouldBe`
          [0x65, 0x06, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06]

      it "writes [0x65, 0xFD] ++ bs for input bs where bs is a string of length 253" $
        runW (writeOctetStringAP (replicate 253 17)) `shouldBe`
          ([0x65, 0xFD] ++ replicate 253 17)

      it "writes [0x65, 0xFE, 0x00, 0xFE] ++ bs for input bs where bs is a string of length 254" $
        runW (writeOctetStringAP (replicate 254 18)) `shouldBe`
          ([0x65, 0xFE, 0x00, 0xFE] ++ replicate 254 18)

      it "writes [0x65, 0xFF, 0x00, 0x01, 0x00, 0x00] as the tag for a string of length 65536" $
        runW (writeOctetStringAP (replicate 65536 19)) `shouldBe`
          ([0x65, 0xFF, 0x00, 0x01, 0x00, 0x00] ++ replicate 65536 19)

    describe "writeStringAP" $ do
      it "writes [0x71, 0x00] for empty string" $
        runW (writeStringAP "") `shouldBe` [0x71, 0x00]

      it "writes [0x73, 0x00, 0x48, 0x49] for \"HI\"" $
        runW (writeStringAP "HI") `shouldBe` [0x73, 0x00, 0x48, 0x49]

    describe "writeEnumeratedAP" $ do
      it "writes [0x91, 0x00] for 0" $
        runW (writeEnumeratedAP $ Enumerated 0) `shouldBe` [0x91, 0x00]

    describe "writeDateAP" $
      it "writes [0xA4, 0x72, 0x04, 0x06, 0xFF] for Date 114 4 6 255" $
        runW (writeDateAP $ Date 114 4 6 255) `shouldBe` [0xA4, 0x72, 0x04, 0x06, 0xFF]

    describe "writeTimeAP" $
      it "writes [0xB4, 0x09, 0x1A, 0x13, 0xFF] for Time 9 26 19 255" $
        runW (writeTimeAP $ Time 9 26 19 255) `shouldBe` [0xB4, 0x09, 0x1A, 0x13, 0xFF]

    describe "writeObjectIdentifierAP" $
      it "writes [0xC4, 0x01, 0x00, 0x00, 0xFF] for ObjectIdentifier 4 255" $
        runW (writeObjectIdentifierAP $ objectIdentifier 4 255) `shouldBe`
          [0xC4, 0x01, 0x00, 0x00, 0xFF]
