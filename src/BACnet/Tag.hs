module BACnet.Tag
  (
  apNullTag,
  apTrueTag,
  apFalseTag,
  apUnsignedTag,
  readNullAPTag,
  readNullCSTag,
  readBoolAPTag,
  readUnsignedAPTag,
  readSignedAPTag,
  readRealAPTag,
  readDoubleAPTag,
  readOctetStringAPTag,
  readStringAPTag,
  readBitStringAPTag,
  readEnumeratedAPTag,
  readDateAPTag,
  readTimeAPTag,
  readObjectIdentifierAPTag,
  Tag(..),
  boolVal,
  tagLength,
  writeNullAPTag,
  writeBoolAPTag,
  writeUnsignedAPTag,
  writeSignedAPTag,
  writeRealAPTag,
  writeDoubleAPTag,
  writeOctetStringAPTag,
  writeStringAPTag,
  writeDateAPTag,
  writeTimeAPTag,
  writeObjectIdentifierAPTag,
  unfoldNum,
  Unfoldable,
  ) where

import Control.Monad
import Control.Applicative
import Data.Word (Word, Word32, Word16, Word8)
import Data.Int (Int, Int32, Int16, Int8)
import qualified BACnet.Tag.Core as TC
import BACnet.Reader.Core
import qualified Data.ByteString.Lazy as BS
import Data.Bits
import qualified BACnet.Writer.Core as WC
import BACnet.Writer.Core ((<>))

data Tag =
          NullAP
        | NullCS Word8
        | BoolAP Bool
        | BoolCS Word8
        | UnsignedAP Word32
        | UnsignedCS Word8 Word32
        | SignedAP Word32
        | SignedCS Word8 Word32
        | RealAP
        | RealCS Word8
        | DoubleAP
        | DoubleCS Word8
        | OctetStringAP Word32 -- length
        | OctetStringCS Word8 Word32
        | CharacterStringAP Word32 -- length
        | BitStringAP Word32 -- length
        | BitStringCS Word8 Word32
        | EnumeratedAP Word32 -- length
        | EnumeratedCS Word8 Word32
        | DateAP
        | DateCS Word8
        | TimeAP
        | TimeCS Word8
        | ObjectIdentifierAP
        | ObjectIdentifierCS Word8
  deriving (Show, Eq)

apNullTag :: Tag
apNullTag = NullAP

csNullTag :: Word8 -> Tag
csNullTag = NullCS

apTrueTag :: Tag
apTrueTag = BoolAP True

apFalseTag :: Tag
apFalseTag = BoolAP False

csBoolTag :: Word8 -> Tag
csBoolTag = BoolCS

apUnsignedTag :: Word -> Tag
apUnsignedTag w | w <= fromIntegral (maxBound :: Word8) = UnsignedAP 1
                | w <= fromIntegral (maxBound :: Word16) = UnsignedAP 2
                | w <= 0x0FFF = UnsignedAP 3
                | w <= fromIntegral (maxBound :: Word32) = UnsignedAP 4
                | otherwise = undefined

failure :: Reader a
failure = fail ""

const' :: a -> b -> c -> a
const' = const . const

readNullAPTag :: Reader Tag
readNullAPTag = sat (== 0x00) >> return NullAP

readNullCSTag :: Word8 -> Reader Tag
readNullCSTag t = readCS t (==0) (const' $ NullCS t)

readBoolAPTag :: Reader Tag
readBoolAPTag = (sat (== 0x10) >> return (BoolAP False)) <|>
                (sat (== 0x11) >> return (BoolAP True))

readBoolCSTag :: Word8 -> Reader Tag
readBoolCSTag t = readCS t (==1) (const' $ BoolCS t)

readUnsignedAPTag :: Reader Tag
readUnsignedAPTag = readAP 2 UnsignedAP

readUnsignedCSTag :: Word8 -> Reader Tag
readUnsignedCSTag t = readCS t (/=0) UnsignedCS

readSignedAPTag :: Reader Tag
readSignedAPTag = readAP 3 SignedAP

readSignedCSTag :: Word8 -> Reader Tag
readSignedCSTag t = readCS t (/=0) SignedCS

readRealAPTag :: Reader Tag
readRealAPTag = sat (== 0x44) >> return RealAP

readRealCSTag :: Word8 -> Reader Tag
readRealCSTag t = readCS t (==4) (const' $ RealCS t)

readDoubleAPTag :: Reader Tag
readDoubleAPTag = sat (== 0x55) >> sat (== 0x08) >> return DoubleAP

readDoubleCSTag :: Word8 -> Reader Tag
readDoubleCSTag t = readCS t (==8) (const' $ DoubleCS t)

readOctetStringAPTag :: Reader Tag
readOctetStringAPTag = readAP 6 OctetStringAP

readOctetStringCSTag :: Word8 -> Reader Tag
readOctetStringCSTag t = readCS t (const True) OctetStringCS

readStringAPTag :: Reader Tag
readStringAPTag = readAP 7 CharacterStringAP

readBitStringAPTag :: Reader Tag
readBitStringAPTag = readAP 8 BitStringAP

readBitStringCSTag :: Word8 -> Reader Tag
readBitStringCSTag t = readCS t (const True) BitStringCS

readEnumeratedAPTag :: Reader Tag
readEnumeratedAPTag = readAP 9 EnumeratedAP

readDateAPTag :: Reader Tag
readDateAPTag = sat (== 0xa4) >> return DateAP

readTimeAPTag :: Reader Tag
readTimeAPTag = sat (== 0xb4) >> return TimeAP

readObjectIdentifierAPTag :: Reader Tag
readObjectIdentifierAPTag = sat (== 0xc4) >> return ObjectIdentifierAP

readAP :: Word8 -> (Word32 -> Tag) -> Reader Tag
readAP tn co = sat (\b -> TC.tagNumber b == tn && TC.isAP b) >>=
               (lengthOfContent >=> return . co)

-- | @readCS tn pred co@ succeeds if tn matches the tag number that is read,
--   and the tag is CS encoded, and the length checking predicate returns true.
--   It constructs a Tag by using the given constructor @co@.
readCS :: Word8 -> (Word32 -> Bool) -> (Word8 -> Word32 -> Tag) -> Reader Tag
readCS tn p co
  = do
      b <- sat TC.isCS
      guardTagNumber (TC.tagNumber b) tn
      len <- lengthOfContent b
      guard (p len)
      return $ co tn len
    where
      guardTagNumber expected actual
        = when (actual == 0x0F) (void $ sat(==expected)) <|>
            guard (expected == actual)

lengthOfContent :: Word8 -> Reader Word32
lengthOfContent b | TC.lvt b < 5 = return . fromIntegral $ TC.lvt b
                  | TC.lvt b == 5 = lengthOfContent'
                  | otherwise = failure

-- | Reads the next byte. If it is < 254 it returns that value
--   If it is 254, then reads the next 2 bytes as a Word32
--   If it is 255, then reads the next 4 bytes as a Word32
lengthOfContent' :: Reader Word32
lengthOfContent' = byte >>= \b ->
                    if b < 254
                      then return $ fromIntegral b
                      else fmap foldbytes (bytes (if b == 254 then 2 else 4))

foldbytes :: BS.ByteString -> Word32
foldbytes = BS.foldl (\acc w -> acc * 256 + fromIntegral w) 0

boolVal :: Tag -> Bool
boolVal (BoolAP val) = val

tagLength :: Tag -> Word32
tagLength (UnsignedAP len) = len
tagLength (SignedAP len) = len
tagLength (OctetStringAP len) = len
tagLength (CharacterStringAP len) = len
tagLength (BitStringAP len) = len
tagLength (EnumeratedAP len) = len
tagLength ObjectIdentifierAP = 4





-------------------

class Unfoldable a where
  byteGuard :: a -> Bool

signedByteGuard :: (Ord a, Num a) => a -> Bool
signedByteGuard n = n >= (-128) && n <= 127

unsignedByteGuard :: (Ord a, Num a) => a -> Bool
unsignedByteGuard n = n <= 255

instance Unfoldable Word where
  byteGuard = unsignedByteGuard

instance Unfoldable Word32 where
  byteGuard = unsignedByteGuard

instance Unfoldable Word16 where
  byteGuard = unsignedByteGuard

instance Unfoldable Word8 where
  byteGuard = unsignedByteGuard

instance Unfoldable Int where
  byteGuard = signedByteGuard

instance Unfoldable Int32 where
  byteGuard = signedByteGuard

instance Unfoldable Int16 where
  byteGuard = signedByteGuard

instance Unfoldable Int8 where
  byteGuard = signedByteGuard


unfoldNum :: (Num a, Unfoldable a, Ord a, Bits a, Integral a)
  => a -> (Word32, [Word8])
unfoldNum = flip unfoldNum' []

unfoldNum' :: (Num a, Unfoldable a, Ord a, Bits a, Integral a)
  => a -> [Word8] -> (Word32, [Word8])
unfoldNum' n bs
  | byteGuard n = (1, currentByte : bs)
  | otherwise =
    let (len, bs') = unfoldNum' (shiftR n 8) (currentByte : bs)
    in (len+1, bs')
  where currentByte = fromIntegral (n .&. 0xFF)

-- | Writes a tag appropriate for an application encoded null value
writeNullAPTag :: WC.Writer
writeNullAPTag = WC.null

writeBoolAPTag :: Bool -> WC.Writer
writeBoolAPTag b = WC.unsigned8 (if b then 0x11 else 0x10)

writeUnsignedAPTag :: Word32 -> WC.Writer
writeUnsignedAPTag = writeIntegralTag 0x20

writeSignedAPTag :: Word32 -> WC.Writer
writeSignedAPTag = writeIntegralTag 0x30

writeIntegralTag :: (Ord a, Num a, Integral a) => Word8 -> a -> WC.Writer
writeIntegralTag tag len | len < 5 = WC.unsigned8 (tag + fromIntegral len)
                         | otherwise = WC.unsigned8 (tag + 5) <> WC.unsigned8 (fromIntegral len)

writeRealAPTag :: WC.Writer
writeRealAPTag = WC.unsigned8 0x44

writeDoubleAPTag :: WC.Writer
writeDoubleAPTag = WC.unsigned16 0x5508

writeOctetStringAPTag :: Word32 => WC.Writer
writeOctetStringAPTag len | len < 5 = WC.unsigned8 (0x60 + fromIntegral len)
                          | len < 254 = WC.unsigned8 0x65 <> WC.unsigned8 (fromIntegral len)
                          | len < 65535 = WC.unsigned8 0x65 <>
                              WC.unsigned8 254 <>
                              WC.unsigned16 (fromIntegral len)
                          | otherwise = WC.unsigned8 0x65 <>
                              WC.unsigned8 255 <>
                              WC.unsigned32 len

writeStringAPTag :: Word32 => WC.Writer
writeStringAPTag len | len < 5 = WC.unsigned8 (0x70 + fromIntegral len)
                          | len < 254 = WC.unsigned8 0x75 <> WC.unsigned8 (fromIntegral len)
                          | len < 65535 = WC.unsigned8 0x75 <>
                              WC.unsigned8 254 <>
                              WC.unsigned16 (fromIntegral len)
                          | otherwise = WC.unsigned8 0x75 <>
                              WC.unsigned8 255 <>
                              WC.unsigned32 len

writeDateAPTag :: WC.Writer
writeDateAPTag = WC.unsigned8 0xA4

writeTimeAPTag :: WC.Writer
writeTimeAPTag = WC.unsigned8 0xB4

writeObjectIdentifierAPTag :: WC.Writer
writeObjectIdentifierAPTag = WC.unsigned8 0xC4
