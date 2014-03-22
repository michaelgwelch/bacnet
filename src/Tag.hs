module Tag (
  isAP,
  isCS,
  ) where

import Data.Word
import Data.Bits

isAP :: Word8 -> Bool
isAP = not . isCS

isCS :: Word8 -> Bool
isCS = flip testBit 3
