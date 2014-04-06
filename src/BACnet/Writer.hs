module BACnet.Writer
  (
    writeNullAP,
    writeBoolAP,
    writeUnsignedAP,
    runW
  ) where

import BACnet.Writer.Core
import BACnet.Tag
import Data.Word
import Prelude hiding (null)

writeNullAP :: Writer
writeNullAP = null

writeBoolAP :: Bool -> Writer
writeBoolAP False = unsigned8 0x10
writeBoolAP True = unsigned8 0x11

writeUnsignedAP :: Word32 -> Writer
writeUnsignedAP 0 = unsigned8 0x21 <> unsigned8 0x00
