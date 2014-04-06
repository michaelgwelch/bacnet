module BACnet.Writer
  (
    writeNullAP,
    writeBoolAP,
    writeUnsignedAP,
    writeSignedAP,
    runW
  ) where

import BACnet.Writer.Core
import BACnet.Tag
import Data.Word
import Data.Int
import Prelude hiding (null)

writeNullAP :: Writer
writeNullAP = writeNullAPTag

writeBoolAP :: Bool -> Writer
writeBoolAP = writeBoolAPTag

writeUnsignedAP :: Word32 -> Writer
writeUnsignedAP n =
  let (len, bs) = unfoldNum n
  in writeUnsignedAPTag len <> bytes bs

writeSignedAP :: Int32 -> Writer
writeSignedAP n =
  let (len, bs) = unfoldNum n
  in writeSignedAPTag len <> bytes bs
