module BACnet.Tag.Writer
  (
    writeNullAPTag,
    writeBoolAPTag,
    writeUnsignedAPTag,
    writeSignedAPTag,
    writeRealAPTag,
    writeDoubleAPTag,
    writeOctetStringAPTag,
    writeStringAPTag,
    writeBitStringAPTag,
    writeEnumeratedAPTag,
    writeDateAPTag,
    writeTimeAPTag,
    writeObjectIdentifierAPTag,
    writeNullCSTag,
  ) where

import Data.Word
import Data.Monoid
import BACnet.Writer.Core
import BACnet.Tag.Core
import Data.Bits

-- | Writes a tag appropriate for an application encoded null value
writeNullAPTag :: Writer
writeNullAPTag = wzero

writeNullCSTag :: TagNumber -> Writer
writeNullCSTag tn = writeCSTag tn (1 :: Length)

writeBoolAPTag :: Bool -> Writer
writeBoolAPTag b = unsigned8 (if b then 0x11 else 0x10)

writeUnsignedAPTag :: Word32 -> Writer
writeUnsignedAPTag = writeIntegralTag 0x20

writeSignedAPTag :: Word32 -> Writer
writeSignedAPTag = writeIntegralTag 0x30

writeIntegralTag :: (Ord a, Num a, Integral a) => Word8 -> a -> Writer
writeIntegralTag tag len | len < 5 = unsigned8 (tag + fromIntegral len)
                         | otherwise = unsigned8 (tag + 5) <> unsigned8 (fromIntegral len)

writeRealAPTag :: Writer
writeRealAPTag = unsigned8 0x44

writeDoubleAPTag :: Writer
writeDoubleAPTag = unsigned16 0x5508

writeOctetStringAPTag :: Word32 -> Writer
writeOctetStringAPTag = writeAPTag 6

writeStringAPTag :: Word32 -> Writer
writeStringAPTag = writeAPTag 7

writeBitStringAPTag :: Word32 -> Writer
writeBitStringAPTag = writeAPTag 8

writeAPTag :: TagNumber -> Length -> Writer
writeAPTag tn len = writeTag tn len classAP

writeEnumeratedAPTag :: Word32 -> Writer
writeEnumeratedAPTag = writeIntegralTag 0x90

writeDateAPTag :: Writer
writeDateAPTag = unsigned8 0xA4

writeTimeAPTag :: Writer
writeTimeAPTag = unsigned8 0xB4

writeObjectIdentifierAPTag :: Writer
writeObjectIdentifierAPTag = unsigned8 0xC4

writeCSTag :: TagNumber -> Length -> Writer
writeCSTag tn l = writeTag tn l classCS


writeTag :: TagNumber -> Length -> Class -> Writer
writeTag tn len c = writeInitialOctet <> writeExtendedTag <> writeExtendedLength
  where cv = classValue c
        tv
          | tn < 15 = shiftL tn 4
          | otherwise = 0xF0
        initialOctet
          | len < 5 = tv + cv + fromIntegral len
          | otherwise = tv + cv + 5
        writeInitialOctet = unsigned8 initialOctet
        writeExtendedTag
          | tn < 15 = mempty
          | otherwise = unsigned8 tn
        writeExtendedLength
          | len < 5 = mempty
          | len < 254 = unsigned8 $ fromIntegral len
          | len < 65535 = unsigned8 254 <> unsigned16 (fromIntegral len)
          | otherwise = unsigned8 255 <> unsigned32 len
