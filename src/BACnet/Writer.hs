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
writeNullAP = writeNullAPTag

writeBoolAP :: Bool -> Writer
writeBoolAP = writeBoolAPTag

writeUnsignedAP :: Word32 -> Writer
writeUnsignedAP n =
  let (len, bs) = unfoldNum n
  in writeUnsignedAPTag len <> bytes bs
