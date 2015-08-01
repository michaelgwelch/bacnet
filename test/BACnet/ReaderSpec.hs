module BACnet.ReaderSpec where

import Data.Int (Int32)
import Test.Hspec
import Test.QuickCheck
import BACnet.Reader
import BACnet.Prim
import Data.Maybe (fromJust)
import Control.Exception (Exception, evaluate)
import BACnet.EncodableSpec ()
import Numeric.Limits (maxValue)

isPosInfinite :: RealFloat a => a -> Bool
isPosInfinite f = (f > 0) && isInfinite f

isNegInfinite :: RealFloat a => a -> Bool
isNegInfinite f = (f < 0) && isInfinite f

posInfinity :: RealFloat a => a
posInfinity = 1/0

negInfinity :: RealFloat a => a
negInfinity = -posInfinity



shouldThrow' :: Exception e => a -> Selector e -> Expectation
shouldThrow' a s = evaluate a `shouldThrow` s

eval :: a -> IO ()
eval a = evaluate a >> return ()

spec :: Spec
spec = do
  describe "readNullAP" $ do
    it "reads [0x00] and returns ())" $
      run readNullAP [0x00] `shouldBe` ()

    it "fails on any other 1 byte input" $
      property $
      forAll (choose (1,255))
      (\b -> eval (run readNullAP [b]) `shouldThrow` anyErrorCall)

  describe "readNullCS" $ do
    it "run (readNullCS 4) [0x48] returns ()" $
      run (readNullCS 4)  [0x48] `shouldBe` ()

    it "run (readNullCS 254) [0xF8, 0xFE] returns ()" $
      run (readNullCS 254) [0xF8, 0xFE] `shouldBe` ()

  describe "readBoolAP" $ do
    it "reads [0x10] and returns False" $
      run readBoolAP [0x10] `shouldBe` False

    it "reads [0x11] and returns True" $
      run readBoolAP [0x11] `shouldBe` True

    it "fails on any other 1 byte input" $
      property $
      forAll (choose (0,255) `suchThat` (\b -> b /= 0x10 && b /= 0x11))
      (\b -> eval (run readBoolAP [b]) `shouldThrow` anyErrorCall)

  describe "readBoolCS" $ do
    it "run (readBoolCS 32) [0xF9, 0x20, 0x00] returns False" $
      run (readBoolCS 32) [0xF9, 0x20, 0x00] `shouldBe` False

    it "run (readBoolCS 254) [0xF9, 0xFE, 0x01] returns True" $
      run (readBoolCS 254) [0xF9, 0xFE, 0x01] `shouldBe` True

    it "fails on any other value for the last byte" $
      property $
      forAll (choose (2,255))
      (\b -> eval (run (readBoolCS 7) [0x79, b]) `shouldThrow` anyErrorCall)

  describe "readUnsignedAP" $ do
    it "reads [0x21, 0x00] and returns 0" $
      run readUnsignedAP [0x21, 0x00] `shouldBe` 0

    it "reads [0x22, 0x01, 0x00] and returns 256" $
      run readUnsignedAP [0x22, 0x01, 0x00] `shouldBe` 256

    it "reads [0x22, 0xFF, 0xFF] and returns 65535" $
      run readUnsignedAP [0x22, 0xFF, 0xFF] `shouldBe` 65535

    it "reads [0x23, 0x01, 0x00, 0x00] and returns 65536" $
      run readUnsignedAP [0x23, 0x01, 0x00, 0x00] `shouldBe`
        65536

    it "reads [0x23, 0xFF, 0xFF, 0xFF] and returns 16777215" $
      run readUnsignedAP [0x23, 0xFF, 0xFF, 0xFF] `shouldBe`
        16777215

    it "reads [0x24, 0x01, 0x00, 0x00, 0x00] and returns 16777216" $
      run readUnsignedAP [0x24, 0x01, 0x00, 0x00, 0x00, 0x00]
      `shouldBe` 16777216

    it "reads [0x24, 0xFF, 0xFF, 0xFF, 0xFF] and returns 4294967295" $
      run readUnsignedAP [0x24, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF]
      `shouldBe` 4294967295

  describe "readUnsignedCS" $
    it "run (readUnsignedCS 0) [0x0C, 0x11, 0x22, 0x33, 0x44] returns 0x11223344" $
      run (readUnsignedCS 0) [0x0C, 0x11, 0x22, 0x33, 0x44] `shouldBe` 0x11223344

  describe "readSignedAP" $ do
    it "reads [0x31, 0x00] and returns 0" $
      run readSignedAP [0x31, 0x00] `shouldBe` 0

    it "reads [0x32, 0x00, 0x80] and returns 128" $
      run readSignedAP [0x32, 0x00, 0x80] `shouldBe` 128

    it "reads [0x32, 0x7F, 0xFF] and returns 32767" $
      run readSignedAP [0x32, 0x7F, 0xFF] `shouldBe` 32767

    it "reads [0x33, 0x00, 0x80, 0x00] and returns 32768" $
      run readSignedAP [0x33, 0x00, 0x80, 0x00] `shouldBe` 32768

    it "reads [0x33, 0x7F, 0xFF, 0xFF] and returns 8388607" $
      run readSignedAP [0x33, 0x7F, 0xFF, 0xFF] `shouldBe` 8388607

    it "reads [0x34, 0x00, 0x80, 0x00, 0x00] and returns 8388608" $
      run readSignedAP [0x34, 0x00, 0x80, 0x00, 0x00] `shouldBe` 8388608

    it "reads [0x34, 0x7F, 0xFF, 0xFF, 0xFF] and returns 2147483647" $
      run readSignedAP [0x34, 0x7F, 0xFF, 0xFF, 0xFF] `shouldBe` 2147483647

    it "reads [0x31, 0xFF] and returns -1" $
      run readSignedAP [0x31, 0xFF] `shouldBe` -1

    it "reads [0x31, 0xFE] and returns -2" $
      run readSignedAP [0x31, 0xFE] `shouldBe` -2

    it "reads [0x31, 0x80] and returns -128" $
      run readSignedAP [0x31, 0x80] `shouldBe` -128

    it "reads [0x32, 0xFF, 0x7F] and returns -129" $
      run readSignedAP [0x32, 0xFF, 0x7F] `shouldBe` -129

    it "reads [0x32, 0x80, 0x00] and returns -32768" $
      run readSignedAP [0x32, 0x80, 0x00] `shouldBe` -32768

    it "reads [0x33, 0xFF, 0x7F, 0xFF] and returns -32769" $
      run readSignedAP [0x33, 0xFF, 0x7F, 0xFF] `shouldBe` -32769

    it "reads [0x33, 0x80, 0x00, 0x00] and returns -8388608" $
      run readSignedAP [0x33, 0x80, 0x00, 0x00] `shouldBe` (-8388608)

    it "reads [0x34, 0xFF, 0x7F, 0xFF, 0xFF] and returns -8388609" $
      run readSignedAP [0x34, 0xFF, 0x7F, 0xFF, 0xFF] `shouldBe` -8388609

    it "reads [x034, 0x80, 0x00, 0x00, 0x00] and returns -2147483648" $
      run readSignedAP [0x34, 0x80, 0x00, 0x00, 0x00] `shouldBe` 
          (minBound :: Int32)


  describe "readRealAP" $ do
    it "reads [0x44, 0x00, 0x00, 0x00, 0x01] and returns 1.4e-45" $
      run readRealAP [0x44, 0x00, 0x00, 0x00, 0x01] `shouldBe` 1.4e-45

    it "reads [0x44, 0x7F, 0x7F, 0xFF, 0xFF] and returns maxValue :: Float" $
      run readRealAP [0x44, 0x7F, 0x7F, 0xFF, 0xFF] `shouldBe` (maxValue :: Float)

    it "reads [0x44, 0xFF, 0x7F, 0xFF, 0xFF] and returns -maxValue :: Float" $
      run readRealAP [0x44, 0xFF, 0x7F, 0xFF, 0xFF] `shouldBe` (-maxValue :: Float)

    it "reads [0x44, 0xFF, 0xC0, 0x00, 0x00] and returns a NaN value" $
      isNaN (run readRealAP [0x44, 0xFF, 0xC0, 0x00, 0x00]) `shouldBe` True

    it "reads [0x44, 0x7F, 0x80, 0x00, 0x00] and returns Infinity" $
      run readRealAP [0x44, 0x7F, 0x80, 0x00, 0x00] `shouldBe` posInfinity

    it "reads [0x44, 0xFF, 0x80, 0x00, 0x00] and returns -Infinity" $
      run readRealAP [0x44, 0xFF, 0x80, 0x00, 0x00] `shouldBe` negInfinity



  describe "readDoubleAP" $ do
    it "reads [0x55, 0x08, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01, 0xaa] and returns 4.9e-324" $
      run readDoubleAP [0x55, 0x08, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01, 0xaa]
        `shouldBe` 4.9e-324

    it "reads [0x55, 0x08, 0x7F, 0xEF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF] and returns maxValue :: Double" $
      run readDoubleAP [0x55, 0x08, 0x7F, 0xEF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF]
        `shouldBe` (maxValue :: Double)

    it "reads [0x55, 0x08, 0xFF, 0xEF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF] and returns -maxValue :: Double" $
      run readDoubleAP [0x55, 0x08, 0xFF, 0xEF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF]
        `shouldBe` (-maxValue :: Double)

    it "reads [0x55, 0x08, 0xFF, 0xF8, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00] and returns a NaN Value" $
      isNaN (run readDoubleAP [0x55, 0x08, 0xFF, 0xF8, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00])
        `shouldBe` True

    it "reads [0x55, 0x08, 0xFF, 0xF0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00] and returns -Infinity" $
      run readDoubleAP [0x55, 0x08, 0xFF, 0xF0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]
        `shouldBe` negInfinity

    it "reads [0x55, 0x08, 0x7F, 0xF0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00] and returns Infinity" $
      run readDoubleAP [0x55, 0x08, 0x7F, 0xF0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]
        `shouldBe` posInfinity


  describe "readOctetStringAP" $ do
    it "reads [0x60] and returns []" $
      run readOctetStringAP [0x60] `shouldBe` []

    it "reads [0x61 0x22] and returns [0x22]" $
      run readOctetStringAP [0x61, 0x22] `shouldBe` [0x22]

    it ("reads [0x65, 0x08, 0x01, 0x02, 0xa0, 0xb0, 0xc0, 0xd0, 0xe0, 0xff, 0xee] and " ++
       "returns [0x01, 0x02, 0xa0, 0xb0, 0xc0, 0xd0, 0xe0, 0xf0]") $
       run readOctetStringAP [0x65, 0x08, 0x01, 0x02, 0xa0, 0xb0, 0xc0, 0xd0, 0xe0, 0xff, 0xee]
       `shouldBe` [0x01, 0x02, 0xa0, 0xb0, 0xc0, 0xd0, 0xe0, 0xff]

  describe "readStringAP" $ do
    it "reads [0x71, 0x00] and returns \"\"" $
      run readStringAP [0x71, 0x00] `shouldBe` ""

    it "reads [0x72, 0x00, 0x41] and returns \"A\"" $
      run readStringAP [0x72, 0x00, 0x41] `shouldBe` "A"

    it "reads [0x75, 0x06, 0x00, 0x48, 0x45, 0x4C, 0x4C, 0x4F] and returns \"HELLO\"" $
      run readStringAP [0x075, 0x06, 0x00, 0x48, 0x45, 0x4C, 0x4C, 0x4F] `shouldBe` "HELLO"

  describe "readBitStringAP" $ do
    it "reads [0x80] and throws an error. Must be at least length 1" $
      run readBitStringAP [0x80] `shouldThrow'` anyErrorCall

    it "reads [0x82 0x03 0xF7] and returns 'bitString 3 [0xF7]'" $
      run readBitStringAP [0x82, 0x03, 0xF7] `shouldBe` fromJust (bitString 3 [0xF7])

  describe "readEnumeratedAP" $ do
    it "reads [0x91, 0x00] and returns 0" $
      run readEnumeratedAP [0x91, 0x00] `shouldBe` Enumerated 0

    it "reads [0x92, 0x01, 0x00] and returns 256" $
      run readEnumeratedAP [0x92, 0x01, 0x00] `shouldBe` Enumerated 256

    it "reads [0x92, 0xFF, 0xFF] and returns 65535" $
      run readEnumeratedAP [0x92, 0xFF, 0xFF] `shouldBe` Enumerated 65535

    it "reads [0x93, 0x01, 0x00, 0x00] and returns 65536" $
      run readEnumeratedAP [0x93, 0x01, 0x00, 0x00] `shouldBe`
        Enumerated 65536

  describe "readDateAP" $
    it "reads [0xA4, 0x72, 0x04, 0x06, 0xFF] and returns Date 114 4 6 255" $
      run readDateAP [0xa4, 0x72, 0x04, 0x06, 0xFF] `shouldBe` Date 114 4 6 255

  describe "readTimeAP" $
    it "reads [0xB4, 0x0A, 0x26, 0x22, 0xFF] and returns Time 10 38 34 255" $
      run readTimeAP [0xb4, 0x0a, 0x26, 0x22, 0xFF] `shouldBe` Time 10 38 34 255

  describe "readObjectIdentifierAP" $
    it "reads [0xC4, 0x00, 0xC0, 0x00, 0x0F] and returns objectIdentifier 3 15" $
      run readObjectIdentifierAP [0xc4, 0x00, 0xc0, 0x00, 0x0F] `shouldBe`
        fromJust (objectIdentifier 3 15)

  describe "readAnyAP" $ do
    it "reads NullAP [0x00]" $
      run readAnyAP [0x00] `shouldBe` NullAP

    context "it can read Boolean Ap" $ do
      it "reads [0x10]" $
        run readAnyAP [0x10] `shouldBe` BooleanAP False

      it "reads [0x11]" $
        run readAnyAP [0x11] `shouldBe` BooleanAP True

    it "reads UnsignedAP" $
      run readAnyAP [0x24, 0x11, 0x11, 0x22, 0xff] `shouldBe` UnsignedAP 0x111122ff

    it "reads SignedAP" $
      run readAnyAP [0x34, 0x22, 0x11, 0xff, 0x34] `shouldBe` SignedAP 0x2211ff34

    it "reads RealAP" $
      run readAnyAP [0x44, 0x00, 0x00, 0x00, 0x01] `shouldBe` RealAP 1.4e-45

    it "reads DoubleAP" $
      run readAnyAP [85,8,62,149,18,122,154,191,163,50]
        `shouldBe` DoubleAP 3.14e-7

    it "reads OctetStringAP" $
      run readAnyAP [0x65,6,1,2,3,4,5,6] `shouldBe` OctetStringAP [1,2,3,4,5,6]

    it "reads CharacterStringAP" $
      run readAnyAP [0x75,6,0,72,69,76,76,79] `shouldBe` CharacterStringAP "HELLO"

    it "reads BitStringAP" $
      run readAnyAP [0x85,0x03,0x02,0xcd,0x00]
        `shouldBe` (BitStringAP . fromJust $ bitString 2 [0xcd, 0x00])

    it "reads EnumeratedAP" $
      run readAnyAP [0x92, 0xfe, 0xdc] `shouldBe` EnumeratedAP (Enumerated 0xfedc)

    it "reads DateAP" $
      run readAnyAP [0xa4, 100, 2, 3, 4] `shouldBe` DateAP (Date 100 2 3 4)

    it "reads TimeAP" $
      run readAnyAP [0xb4, 11, 12, 13, 0] `shouldBe` TimeAP (Time 11 12 13 0)

    it "reads ObjectIdentifierAP" $
      run readAnyAP [0xc4, 0x00, 0x80, 0x00, 0x05] `shouldBe`
        ObjectIdentifierAP (fromJust $ objectIdentifier 2 5)
