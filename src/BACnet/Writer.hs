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
    writeBoolCS,
    writeUnsignedCS,
    writeSignedCS,
    writeRealCS,
    writeDoubleCS,
    writeOctetStringCS,
    writeStringCS,
    writeBitStringCS,
    writeEnumeratedCS,
    writeDateCS,
    writeTimeCS,
    writeObjectIdentifierCS,
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
writeUnsignedAP = writeIntegralAP writeUnsignedAPTag

-- | Writes a context specific unsigned value
--
-- >>> runW $ writeUnsignedCS 14 128
-- [233,128]
writeUnsignedCS :: TagNumber -> Word32 -> Writer
writeUnsignedCS = flip writeIntegralCS writeCSTag

writeSignedAP :: Int32 -> Writer
writeSignedAP = writeIntegralAP writeSignedAPTag

writeSignedCS :: TagNumber -> Int32 -> Writer
writeSignedCS = flip writeIntegralCS writeCSTag

writeIntegralAP :: (Num a, Unfoldable a, Ord a, Bits a, Integral a)
  => (Length -> Writer) -> a -> Writer
writeIntegralAP tagWriter n =
  let (len, bs) = unfoldNum n
  in tagWriter len <> bytes bs

writeIntegralCS :: (Num a, Unfoldable a, Ord a, Bits a, Integral a)
  => TagNumber -> (TagNumber -> Length -> Writer) -> a -> Writer
writeIntegralCS tn tagWriter n =
  let (len, bs) = unfoldNum n
  in tagWriter tn len <> bytes bs

writeRealAP :: Float -> Writer
writeRealAP f = writeRealAPTag <> real f

writeRealCS :: TagNumber -> Float -> Writer
writeRealCS tn f = writeCSTag tn (4 :: Length) <> real f

writeDoubleAP :: Double -> Writer
writeDoubleAP d = writeDoubleAPTag <> double d

writeDoubleCS :: TagNumber -> Double -> Writer
writeDoubleCS tn d = writeCSTag tn (8 :: Length) <> double d

writeOctetStringAP :: [Word8] -> Writer
writeOctetStringAP = writeAnyString writeOctetStringAPTag BS.pack

-- | Writes a context specific octet string
--
-- >>>runW $ writeOctetStringCS 3 [0x01, 0x02]
-- [58,1,2]
writeOctetStringCS :: TagNumber -> [Word8] -> Writer
writeOctetStringCS tn = writeAnyString (writeCSTag tn) BS.pack

writeStringAP :: String -> Writer
writeStringAP = writeAnyString writeStringAPTag utf8EncodeString

writeStringCS :: TagNumber -> String -> Writer
writeStringCS tn = writeAnyString (writeCSTag tn) utf8EncodeString

utf8EncodeString :: String -> BS.ByteString
utf8EncodeString = BS.cons 0x00 . UTF8.fromString

writeBitStringAP :: BitString -> Writer
writeBitStringAP = writeAnyString writeBitStringAPTag encodeBitString

writeBitStringCS :: TagNumber -> BitString -> Writer
writeBitStringCS tn = writeAnyString (writeCSTag tn) encodeBitString

encodeBitString :: BitString -> BS.ByteString
encodeBitString s = BS.pack $ bitStringUnusedBits s : bitStringBytes s

writeAnyString :: (Length -> Writer) -> (a -> BS.ByteString) -> a -> Writer
writeAnyString fWriter encoder val =
  fWriter (fromIntegral $ BS.length encodedString) <>
  bytestring encodedString
  where encodedString = encoder val

writeEnumeratedAP :: Enumerated -> Writer
writeEnumeratedAP = writeIntegralAP writeEnumeratedAPTag . getEnumValue

writeEnumeratedCS :: TagNumber -> Enumerated -> Writer
writeEnumeratedCS tn = writeIntegralCS tn writeCSTag . getEnumValue

writeDateAP :: Date -> Writer
writeDateAP = writeAnyString (const writeDateAPTag) encodeDate

writeDateCS :: TagNumber -> Date -> Writer
writeDateCS tn = writeAnyString (writeCSTag tn) encodeDate

encodeDate :: Date -> BS.ByteString
encodeDate (Date y m dm dw) = BS.pack [y,m,dm,dw]

writeTimeAP :: Time -> Writer
writeTimeAP = writeAnyString (const writeTimeAPTag) encodeTime

writeTimeCS :: TagNumber -> Time -> Writer
writeTimeCS tn = writeAnyString (writeCSTag tn) encodeTime

encodeTime :: Time -> BS.ByteString
encodeTime (Time h m s hs) = BS.pack [h,m,s,hs]

writeObjectIdentifierAP :: ObjectIdentifier -> Writer
writeObjectIdentifierAP = (writeObjectIdentifierAPTag <>) . unsigned32 . objectIdentifierValue

writeObjectIdentifierCS :: TagNumber -> ObjectIdentifier -> Writer
writeObjectIdentifierCS tn = (writeCSTag tn  4 <>) . unsigned32 . objectIdentifierValue

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
