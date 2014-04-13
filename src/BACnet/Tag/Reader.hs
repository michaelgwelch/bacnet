-- | Defines readers for reading tags of BACnet values
module BACnet.Tag.Reader
  (
    readNullAPTag,
    readNullCSTag,
    readBoolAPTag,
    readBoolCSTag,
    readUnsignedAPTag,
    readUnsignedCSTag,
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
    readAnyAPTag,
  ) where

import BACnet.Tag.Core
import BACnet.Reader.Core
import Data.Word
import Control.Monad (when, void, guard)
import Control.Applicative ((<|>))

-- | Like 'const' but applied twice. It takes three arguments
--   and returns the first.
const2 :: a -> b -> c -> a
const2 = const . const

readNullAPTag :: Reader Tag
readNullAPTag = sat (== 0x00) >> return NullAP

readNullCSTag :: TagNumber -> Reader Tag
readNullCSTag t = readCS t (==0) (const2 $ NullCS t)

readBoolAPTag :: Reader Tag
readBoolAPTag = (sat (== 0x10) >> return (BoolAP False)) <|>
                (sat (== 0x11) >> return (BoolAP True))

readBoolCSTag :: TagNumber -> Reader Tag
readBoolCSTag t = readCS t (==1) (const2 $ BoolCS t)

readUnsignedAPTag :: Reader Tag
readUnsignedAPTag = readAP 2 UnsignedAP

readUnsignedCSTag :: TagNumber -> Reader Tag
readUnsignedCSTag t = readCS t (/=0) UnsignedCS

readSignedAPTag :: Reader Tag
readSignedAPTag = readAP 3 SignedAP

readSignedCSTag :: TagNumber -> Reader Tag
readSignedCSTag t = readCS t (/=0) SignedCS

readRealAPTag :: Reader Tag
readRealAPTag = sat (== 0x44) >> return RealAP

readRealCSTag :: TagNumber -> Reader Tag
readRealCSTag t = readCS t (==4) (const2 $ RealCS t)

readDoubleAPTag :: Reader Tag
readDoubleAPTag = sat (== 0x55) >> sat (== 0x08) >> return DoubleAP

readDoubleCSTag :: TagNumber -> Reader Tag
readDoubleCSTag t = readCS t (==8) (const2 $ DoubleCS t)

readOctetStringAPTag :: Reader Tag
readOctetStringAPTag = readAP 6 OctetStringAP

readOctetStringCSTag :: TagNumber -> Reader Tag
readOctetStringCSTag t = readCS t (const True) OctetStringCS

readStringAPTag :: Reader Tag
readStringAPTag = readAP 7 CharacterStringAP

readBitStringAPTag :: Reader Tag
readBitStringAPTag = readAP 8 BitStringAP

readBitStringCSTag :: TagNumber -> Reader Tag
readBitStringCSTag t = readCS t (const True) BitStringCS

readEnumeratedAPTag :: Reader Tag
readEnumeratedAPTag = readAP 9 EnumeratedAP

readDateAPTag :: Reader Tag
readDateAPTag = sat (== 0xa4) >> return DateAP

readTimeAPTag :: Reader Tag
readTimeAPTag = sat (== 0xb4) >> return TimeAP

readObjectIdentifierAPTag :: Reader Tag
readObjectIdentifierAPTag = sat (== 0xc4) >> return ObjectIdentifierAP


peeksat :: (Word8 -> Bool) -> Reader Word8
peeksat p = peek >>= \b -> if p b then return b else fail "predicate failed"

readAnyAPTag :: Reader Tag
readAnyAPTag =
  do
    t <- peeksat isAP
    case tagNumber t of
      0 -> readNullAPTag
      1 -> readBoolAPTag
      2 -> readUnsignedAPTag
      3 -> readSignedAPTag
      4 -> readRealAPTag
      5 -> readDoubleAPTag
      6 -> readOctetStringAPTag
      7 -> readStringAPTag
      8 -> readBitStringAPTag
      9 -> readEnumeratedAPTag
      10 -> readDateAPTag
      11 -> readTimeAPTag
      12 -> readObjectIdentifierAPTag
      _ -> fail "Invalid tag number for AP Tag"

type LengthPredicate = Length -> Bool
type APTagConstructor = Length -> Tag
type CSTagConstructor = TagNumber -> Length -> Tag

readAP :: TagNumber -> APTagConstructor -> Reader Tag
readAP tn co =
  do
    b <- sat (\b -> tagNumber b == tn && isAP b && lvt b <= 5)
    len <- lengthOfContent b
    return $ co len

-- | @readCS tn pred co@ succeeds if tn matches the tag number that is read,
--   and the tag is CS encoded, and the length checking predicate returns true.
--   It constructs a Tag by using the given constructor @co@.
readCS :: TagNumber -> LengthPredicate -> CSTagConstructor -> Reader Tag
readCS tn p co
  = do
      b <- sat isCS
      guardTagNumber (tagNumber b) tn
      len <- lengthOfContent b
      guard (p len)
      return $ co tn len
    where
      guardTagNumber actual expected
        = when (actual == 0x0F) (void $ sat(==expected)) <|>
            guard (expected == actual)

type TagInitialOctet = Word8

-- | Given an initial octet of a tag, reads the length of the content
lengthOfContent :: TagInitialOctet -> Reader Word32
lengthOfContent b | lvt b < 5 = return . fromIntegral $ lvt b
                  | lvt b == 5 = lengthOfContent'
                  | otherwise = fail "Invalid length encoding"

-- | Reads the next byte. If it is < 254 it returns that value
--   If it is 254, then reads the next 2 bytes as a Word32
--   If it is 255, then reads the next 4 bytes as a Word32
lengthOfContent' :: Reader Word32
lengthOfContent' = byte >>= \b ->
                    if b < 254
                      then return $ fromIntegral b
                      else fmap foldbytes (bytes (if b == 254 then 2 else 4))

foldbytes :: [Word8] -> Word32
foldbytes = foldl (\acc w -> acc * 256 + fromIntegral w) 0
