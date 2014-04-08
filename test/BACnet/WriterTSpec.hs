module BACnet.WriterTSpec where

import Test.Hspec
import BACnet.Writer
import BACnet.WriterT

spec :: Spec
spec =
  do
    describe "writeUnsignedAP2" $
      it "works" $
        runW' writeUnsignedAP2 0x23 `shouldBe` [0x21, 0x23]

    describe "writeEvenAP" $
      it "writes [0x21, 0x0a] for input 5" $
        runW' writeEvenAP 5 `shouldBe` [0x21, 0x0a]

    describe "writeNumSat" $
      context " with predicte (==0)" $ do
        let p = (==0)
        it "writes [0x10] (which is False) with input 35" $
          runW' (writeNumSat p) 35 `shouldBe` [0x10]

        it "writes [0x11] (which is True) with input 0" $
          runW' (writeNumSat p) 0 `shouldBe` [0x11]

    describe "writeStringLength" $
      it "writes [0x31, 0x0a] for input \"0123456789\"" $
        runW' writeStringLength "0123456789" `shouldBe` [0x31, 0x0a]

    describe "writeBoolIfStringIsNotEmpty" $ do
      it "writes [0x11] for input \"a\"" $
        runW' writeBoolIfStringIsNotEmpty "a" `shouldBe` [0x11]

      it "writes [0x10] for input \"\"" $
        runW' writeBoolIfStringIsNotEmpty "" `shouldBe` [0x10]
