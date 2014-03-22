module Tag (
  isAP,
  isCS,
  lvt,
  isOpen,
  openType,
  isClose,
  closeType,
  isExtendedLength,
  ) where

import Data.Word
import Data.Bits

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
