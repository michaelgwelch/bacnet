-- | Defines a collection of helper functions for examining the bits
-- of tags.
module BACnet.Tag.Core (
  isAP,
  isCS,
  lvt,
  isOpen,
  openType,
  isClose,
  closeType,
  isExtendedLength,
  tagNumber,
  Tag(..),
  TagNumber,
  Length,
  Class,
  classAP,
  classCS,
  classValue,
  boolVal,
  tagLength,
  ) where

import Data.Word
import Data.Bits

data Tag =
          NullAP
        | NullCS TagNumber
        | BoolAP Bool
        | BoolCS TagNumber
        | UnsignedAP Length
        | UnsignedCS TagNumber Length
        | SignedAP Length
        | SignedCS TagNumber Length
        | RealAP
        | RealCS TagNumber
        | DoubleAP
        | DoubleCS TagNumber
        | OctetStringAP Length -- length
        | OctetStringCS TagNumber Length
        | CharacterStringAP Length -- length
        | BitStringAP Length -- length
        | BitStringCS TagNumber Length
        | EnumeratedAP Length -- length
        | EnumeratedCS TagNumber Length
        | DateAP
        | DateCS TagNumber
        | TimeAP
        | TimeCS TagNumber
        | ObjectIdentifierAP
        | ObjectIdentifierCS TagNumber
        | Open TagNumber
        | Close TagNumber
  deriving (Show, Eq)

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
tagLength (UnsignedCS _ len) = len

type TagNumber = Word8
type Length = Word32
newtype Class = Class { classValue :: Word8 } deriving Eq
classAP = Class 0
classCS = Class 8

-- | 'True' if the Class bit (the 3rd bit) is not set
isAP :: Word8 -> Bool
isAP = not . isCS

-- | 'True' if the Class bit (the 3rd bit) is set
isCS :: Word8 -> Bool
isCS = flip testBit 3

-- | The value B'111', used to mask off the 3 least significant bits
lvtMask :: Word8
lvtMask = 0x07

-- | The value B'101' (0x05)
extendedLength :: Word8
extendedLength = 0x05

-- | The value B'110' (0x0E)
openType :: Word8
openType = 0x06

-- | The value B'111' (0x0F)
closeType :: Word8
closeType = 0x07

-- | Returns the length/value/type which is the 3 least significant bits
lvt :: Word8 -> Word8
lvt = (.&. lvtMask)

-- | Returns true if the class and length/value/type bits match
clvtMatches :: Word8 -> Word8 -> Bool
clvtMatches expected = (== expected) . (.&. 0x0F)

-- | 'True' if 'isCS' and 'lvt' == 'openType'
isOpen :: Word8 -> Bool
isOpen = clvtMatches 0x0E

-- | 'True' if 'isCS' and 'lvt' == 'closeType'
isClose :: Word8 -> Bool
isClose = clvtMatches 0x0F

-- | 'True' if 'lvt' == 'extendedLength'
isExtendedLength :: Word8 -> Bool
isExtendedLength = (== 0x05) . lvt


-- | Returns the tag number value encoded into an initial octet.
--   As one would expect it can't return the actual tag number in the
--   case of extended tag numbers since it is only given the initial octet
--   as input.
tagNumber :: Word8 -> Word8
tagNumber = (0x0F .&.) . flip shiftR 4
