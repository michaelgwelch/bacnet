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
import Data.Bits
import Data.Word
import Data.Int
import Prelude hiding (null)

writeNullAP :: Writer
writeNullAP = writeNullAPTag

writeBoolAP :: Bool -> Writer
writeBoolAP = writeBoolAPTag

writeUnsignedAP :: Word32 -> Writer
writeUnsignedAP = writeIntegral writeUnsignedAPTag

writeSignedAP :: Int32 -> Writer
writeSignedAP = writeIntegral writeSignedAPTag

writeIntegral :: (Num a, Unfoldable a, Ord a, Bits a, Integral a)
  => (Word32 -> Writer) -> a -> Writer
writeIntegral tagWriter n =
  let (len, bs) = unfoldNum n
  in tagWriter len <> bytes bs
