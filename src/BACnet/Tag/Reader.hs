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
    readSignedCSTag,
    readRealAPTag,
    readRealCSTag,
    readDoubleAPTag,
    readDoubleCSTag,
    readOctetStringAPTag,
    readOctetStringCSTag,
    readStringAPTag,
    readStringCSTag,
    readBitStringAPTag,
    readBitStringCSTag,
    readEnumeratedAPTag,
    readEnumeratedCSTag,
    readDateAPTag,
    readDateCSTag,
    readTimeAPTag,
    readTimeCSTag,
    readObjectIdentifierAPTag,
    readObjectIdentifierCSTag,
    readAnyAPTag,
    readOpenTag,
    readCloseTag
  ) where

import BACnet.Tag.Core
import BACnet.Reader.Core
import Data.Word
import Control.Monad (guard)
import Control.Applicative ((<|>))


-- | Like 'const' but applied twice. It takes three arguments
--   and returns the first.
const2 :: a -> b -> c -> a
const2 = const . const

readNullAPTag :: Reader Tag
readNullAPTag = sat (== 0x00) >> return NullAP

readNullCSTag :: TagNumber -> Reader Tag
readNullCSTag t = readCS t (==0) (flip $ const NullCS)

readBoolAPTag :: Reader Tag
readBoolAPTag = (sat (== 0x10) >> return (BoolAP False)) <|>
                (sat (== 0x11) >> return (BoolAP True))

readBoolCSTag :: TagNumber -> Reader Tag
readBoolCSTag t = readCS t (==1) (flip $ const BoolCS)

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

readStringCSTag :: TagNumber -> Reader Tag
readStringCSTag t = readCS t (const True) CharacterStringCS

readBitStringAPTag :: Reader Tag
readBitStringAPTag = readAP 8 BitStringAP

readBitStringCSTag :: TagNumber -> Reader Tag
readBitStringCSTag t = readCS t (const True) BitStringCS

readEnumeratedAPTag :: Reader Tag
readEnumeratedAPTag = readAP 9 EnumeratedAP

readEnumeratedCSTag :: TagNumber -> Reader Tag
readEnumeratedCSTag tn = readCS tn (/=0) EnumeratedCS

readDateAPTag :: Reader Tag
readDateAPTag = sat (== 0xa4) >> return DateAP

readDateCSTag :: TagNumber -> Reader Tag
readDateCSTag tn = readCS tn (==4) (flip $ const DateCS)

readTimeAPTag :: Reader Tag
readTimeAPTag = sat (== 0xb4) >> return TimeAP

readTimeCSTag :: TagNumber -> Reader Tag
readTimeCSTag tn = readCS tn (==4) (flip $ const TimeCS)

readObjectIdentifierAPTag :: Reader Tag
readObjectIdentifierAPTag = sat (== 0xc4) >> return ObjectIdentifierAP

readObjectIdentifierCSTag :: TagNumber -> Reader Tag
readObjectIdentifierCSTag tn =  readCS tn (==4)  (flip $ const ObjectIdentifierCS)

readOpenTag :: TagNumber -> Reader Tag
readOpenTag tn = readTag (==tn) (==classCS) (const True) (==6) (flip $ const Open)

readCloseTag :: TagNumber -> Reader Tag
readCloseTag tn = readTag (==tn) (==classCS) (const True) (==7) (flip $ const Close)

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
type CSTagConstructor = TagConstructor

readAP :: TagNumber -> APTagConstructor -> Reader Tag
readAP tn co = readTag (==tn) (==classAP) (const True) (const True) (const co)


-- | @readCS tn pred co@ succeeds if tn matches the tag number that is read,
--   and the tag is CS encoded, and the length checking predicate returns true.
--   It constructs a Tag by using the given constructor @co@.
readCS :: TagNumber -> LengthPredicate -> CSTagConstructor -> Reader Tag
readCS tn p = readTag (==tn) (==classCS) p (const True)

type TagNumberPredicate = TagNumber -> Bool
type ClassPredicate = Class -> Bool
type TagConstructor = TagNumber -> Length -> Tag
type TypePredicate = Word8 -> Bool
readTag :: TagNumberPredicate -> ClassPredicate -> LengthPredicate ->
  TypePredicate -> TagConstructor -> Reader Tag
readTag tagNumberP classP lengthP typeP co
        = byte >>= \b ->
          readClass b >>= \c ->
          readExtendedTag b c >>= \tn ->
          readExtendedLength b >>= \len ->
          guard (tagNumberP tn && classP c && lengthP len && typeP b) >>
          return (co tn len)
      where readClass b = return $ if isCS b then classCS else classAP
            readExtendedTag b c | c == classAP = readTagNumber b
                                | c == classCS =
                                    (guard (tagNumber b == 15) >> byte) <|>
                                    readTagNumber b
            readTagNumber = return . tagNumber
            readExtendedLength = lengthOfContent






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
