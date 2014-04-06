module BACnet.Writer
  (
    writeNullAP,
    writeBoolAP,
    runW
  ) where

import BACnet.Writer.Core
import BACnet.Tag
import Prelude hiding (null)

writeNullAP :: Writer
writeNullAP = null

writeBoolAP :: Bool -> Writer
writeBoolAP False = unsigned8 0x10
writeBoolAP True = unsigned8 0x11
