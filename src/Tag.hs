module Tag (
  isAP,
  ) where

import Data.Word
import Data.Bits

isAP :: Word8 -> Bool
isAP = not . (flip testBit) 3
