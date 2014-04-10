-- | Defines writers of BACnet data.
--   Writers are instances of 'Monoid' therefore the Monoid equalities hold.
--   Here are some examples using (writeUnsignedAP b) as an example.
--
-- prop> mempty <> (writeUnsignedAP b) == writeUnsignedAP b
-- prop> writeUnsignedAP b <> mempty == writeUnsignedAP b
-- prop> let w = writeUnsignedAP in (w x <> w y) <> w z == w x <> (w y <> w z)
module BACnet.Writer
  (
    Writer,
    writeNullAP,
    writeBoolAP,
    writeUnsignedAP,
    writeSignedAP,
    writeRealAP,
    writeDoubleAP,
    writeOctetStringAP,
    writeStringAP,
    writeBitStringAP,
    writeEnumeratedAP,
    writeDateAP,
    writeTimeAP,
    writeObjectIdentifierAP,
    runW
  ) where

import BACnet.Prim
import BACnet.Writer.Core
import BACnet.Tag
import Data.Bits
import Data.Word
import Data.Int
import Prelude hiding (null)
import qualified Data.ByteString.Lazy.UTF8 as UTF8
import Data.Monoid (mempty, Monoid)

-- | Writes an application encoded null value which is always @0x00@.
--
-- >>> runW writeNullAP
-- [0]
writeNullAP :: Writer
writeNullAP = writeNullAPTag

-- | Writes an application encoded boolean value which is either 0x10 (False),
--   or 0x11 (True).
--
-- >>> runW $ writeBoolAP True
-- [17]
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

writeOctetStringAP :: [Word8] -> Writer
writeOctetStringAP o = writeOctetStringAPTag (fromIntegral $ length o) <> bytes o

writeStringAP :: String -> Writer
writeStringAP s = writeStringAPTag (fromIntegral $ length s + 1) <>
                  unsigned8 0x00 <> bytestring (UTF8.fromString s)

writeBitStringAP :: BitString -> Writer
writeBitStringAP s = writeBitStringAPTag (fromIntegral $ bitStringLength s) <>
                     unsigned8 (bitStringUnusedBits s) <> bytes (bitStringBytes s)

writeEnumeratedAP :: Enumerated -> Writer
writeEnumeratedAP = writeIntegral writeEnumeratedAPTag . getEnumValue

writeDateAP :: Date -> Writer
writeDateAP (Date y m dm dw) =
  writeDateAPTag <> unsigned8 y <> unsigned8 m <> unsigned8 dm <> unsigned8 dw

writeTimeAP :: Time -> Writer
writeTimeAP (Time h m s hs) =
  writeTimeAPTag <> unsigned8 h <> unsigned8 m <> unsigned8 s <> unsigned8 hs

writeObjectIdentifierAP :: ObjectIdentifier -> Writer
writeObjectIdentifierAP = (writeObjectIdentifierAPTag <>) . unsigned32 . getRawValue
