module BACnet.Writer.ByteString
  (
    unfoldl
  ) where

import qualified Data.ByteString.Lazy as BS
import Data.Word

unfoldl :: (a -> Maybe(Word8, a)) -> a -> BS.ByteString
unfoldl f a = BS.pack $ unfoldl' f a []

unfoldl' :: (a -> Maybe(Word8, a)) -> a -> [Word8] -> [Word8]
unfoldl' f a bs = case f a of
                    Nothing -> []
                    Just(b, a') -> unfoldl' f a' (b : bs)
