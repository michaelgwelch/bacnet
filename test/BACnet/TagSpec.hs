module BACnet.TagSpec where

import BACnet.Tag.Core
import BACnet.Tag.Reader
import Test.Hspec
import BACnet.Reader

-- TODO: Remove the readAPTag tests. Remove the function as well.
spec :: Spec
spec =
  describe "readNullAPTag" $
    it "returns () for input [0x00]" $
      run readNullAPTag [0x00] `shouldBe` NullAP
