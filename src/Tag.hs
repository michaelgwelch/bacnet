module Tag
  (
  readTag,
  runReader,
  apNullTag,
  apTrueTag,
  ) where

import Control.Exception (assert)
import Data.Word
import Tag.Core
data Tag = Tag [Word8] deriving (Show, Eq)

apNullTag :: Tag
apNullTag = Tag [0x00]

apTrueTag :: Tag
apTrueTag = Tag [0x11]

newtype Reader a = R {runReader :: [Word8] -> Maybe (a, [Word8])}



readTag :: Reader Tag
readTag = R (\bs -> case bs of
                    [] -> Nothing
                    (x:xs) -> if isAP x then runReader readAPTag bs
                                        else Nothing)

assertNotEmptyAndAP :: [Word8] -> a -> a
assertNotEmptyAndAP [] = assert False
assertNotEmptyAndAP (b:bs) = assert (isAP b)

-- | reads the next ap encoded tag.
--   Calling conventions: input should not be empty and first byte should not
--   have class bit set.
readAPTag :: Reader Tag
readAPTag = R (\bs -> assertNotEmptyAndAP bs $
                case head bs of
                  0x00 -> Just (apNullTag, tail bs)
                  0x11 -> Just (apTrueTag, tail bs)
            )
