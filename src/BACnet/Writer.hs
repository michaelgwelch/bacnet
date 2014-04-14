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
    runW,
    writeAnyAP,
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
    writeNullCS,
  ) where

import BACnet.Prim
import qualified BACnet.Prim as Prim
import BACnet.Writer.Core
import BACnet.Tag.Writer
import BACnet.Tag.Core
import BACnet.Writer.UnfoldNum
import Data.Bits
import Data.Word
import Data.Int
import Prelude hiding (null)
import qualified Data.ByteString.Lazy.UTF8 as UTF8
import qualified Data.ByteString.Lazy as BS
import Data.Monoid (mempty, Monoid, (<>))
import Data.Functor.Contravariant

-- | Writes an application encoded null value which is always @0x00@.
--
-- >>> runW writeNullAP
-- [0]
writeNullAP :: Writer
writeNullAP = writeNullAPTag

-- | Writes a context specific encoded null value.
--
-- >>> runW $ writeNullCS 254
-- [248,254]
--
-- >>> [0xF8,0xFE] == [248,254]
-- True
writeNullCS :: TagNumber -> Writer
writeNullCS = writeNullCSTag

-- | Writes an application encoded boolean value which is either 0x10 (False),
--   or 0x11 (True).
--
-- >>> runW $ writeBoolAP True
-- [17]
writeBoolAP :: Bool -> Writer
writeBoolAP = writeBoolAPTag

-- | Writes a context specific boolean value
--
-- >>> runW $ writeBoolCS 10 True
-- [169,1]
writeBoolCS :: TagNumber -> Bool  -> Writer
writeBoolCS tn b = writeBoolCSTag tn <> unsigned8 fromBool
  where fromBool = if b then 1 else 0

-- | Writes an application encoded unsigned value
--
-- >>> runW $ writeUnsignedAP 0x17F7F
-- [35,1,127,127]
writeUnsignedAP :: Word32 -> Writer
writeUnsignedAP = writeIntegral writeUnsignedAPTag

--writeUnsignedCS :: TagNumber -> Word32 -> Writer
--writeUnsignedCS tn v = writeIntegral writeUnsignedCSTag

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
writeStringAP s = writeStringAPTag (fromIntegral $ BS.length encodedString + 1) <>
                  unsigned8 0x00 <> bytestring encodedString
            where encodedString = UTF8.fromString s

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

writeAnyAP :: Any -> Writer
writeAnyAP Prim.NullAP = writeNullAP
writeAnyAP (Prim.BooleanAP b) = writeBoolAP b
writeAnyAP (Prim.UnsignedAP v) = writeUnsignedAP $ fromIntegral v
writeAnyAP (Prim.SignedAP v) = writeSignedAP $ fromIntegral v
writeAnyAP (Prim.RealAP v) = writeRealAP v
writeAnyAP (Prim.DoubleAP v) = writeDoubleAP v
writeAnyAP (Prim.OctetStringAP v) = writeOctetStringAP v
writeAnyAP (Prim.CharacterStringAP v) = writeStringAP v
writeAnyAP (Prim.BitStringAP v) = writeBitStringAP v
writeAnyAP (Prim.EnumeratedAP v) = writeEnumeratedAP v
writeAnyAP (Prim.DateAP v) = writeDateAP v
writeAnyAP (Prim.TimeAP v) = writeTimeAP v
writeAnyAP (Prim.ObjectIdentifierAP v) = writeObjectIdentifierAP v
