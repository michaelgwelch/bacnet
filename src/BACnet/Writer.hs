module BACnet.Writer
  (
    writeNullAP,
    writeBoolAP,
    writeUnsignedAP,
    writeSignedAP,
    writeRealAP,
    writeDoubleAP,
    writeDateAP,
    writeTimeAP,
    runW
  ) where

import BACnet.Prim
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

writeRealAP :: Float -> Writer
writeRealAP f = writeRealAPTag <> real f

writeDoubleAP :: Double -> Writer
writeDoubleAP d = writeDoubleAPTag <> double d

writeDateAP :: Date -> Writer
writeDateAP (Date y m dm dw) =
  writeDateAPTag <> unsigned8 y <> unsigned8 m <> unsigned8 dm <> unsigned8 dw

writeTimeAP :: Time -> Writer
writeTimeAP (Time h m s hs) =
  writeTimeAPTag <> unsigned8 h <> unsigned8 m <> unsigned8 s <> unsigned8 hs
