-- | Provides utility functions for 'BS.ByteString'
module BACnet.Writer.ByteString
  (
    unfoldl
  ) where

import qualified Data.ByteString.Lazy as BS
import Data.Word

-- | The function 'unfoldl' is invoked as @unfoldl f seed@. It takes a
--   function and a seed value and builds a 'BS.ByteString'. It functions
--   much like 'BS.unfoldr' but the bytes unfold to the left rather than to
--   the right. In the case of @unfoldr@ the first byte generated is on the left
--   and subsequent bytes show up to the right. In the case of 'unfoldl' the
--   opposite is true.
--
--   Given this defintion of @unfoldWord@
--
-- > unfoldWord :: Word -> BS.ByteString
-- > unfoldWord = unfoldl unfoldStep
-- >   where unfoldStep 0 = Nothing
-- >         unfoldStep n = Just(fromIntegral (n .&. 0xFF), shiftR n 8)
--
-- We can demonstrate how 'unfoldl' words with this example:
--
-- >>> unfoldWord 256
-- [1,0]
unfoldl :: (a -> Maybe(Word8, a)) -> a -> BS.ByteString
unfoldl f a = BS.pack $ unfoldl' f a []

unfoldl' :: (a -> Maybe(Word8, a)) -> a -> [Word8] -> [Word8]
unfoldl' f a bs = case f a of
                    Nothing -> []
                    Just(b, a') -> unfoldl' f a' (b : bs)
