-- | Defines the class 'Unfoldable', the function 'unfoldNum',
-- and useful instances of 'Unfoldable'.
module BACnet.Writer.UnfoldNum
  (
    unfoldNum,
    Unfoldable
  ) where

import Data.Bits
import Data.Word
import Data.Int

-- | A useful class for unfolding numbers.
class Unfoldable a where
  -- | Returns 'True' if the argument can be represented in a 'Word8'
  byteGuard :: a -> Bool

-- | Returns 'True' if the argument fits in the bounds of an 'Int8'.
signedByteGuard :: (Ord a, Num a) => a -> Bool
signedByteGuard n = n >= (-128) && n <= 127

-- | Returns 'True' if the argument fits in the bounds of a 'Word8'.
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


-- | Given an argument whose type is an instance of 'Num', 'Unfoldable',
-- 'Ord', 'Bits', and 'Integral', this function will returns a pair
-- that contains the representation of the argument as a ['Word8'] and
-- that contains the length of the list.
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
