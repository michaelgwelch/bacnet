module Tag
  (
  readTag,
  runReader,
  apNullTag,
  apTrueTag,
  ) where

import Data.Word
data Tag = Tag [Word8] deriving (Show, Eq)

apNullTag :: Tag
apNullTag = Tag [0x00]

apTrueTag :: Tag
apTrueTag = Tag [0x11]

newtype Reader a = R {runReader :: [Word8] -> Maybe (a, [Word8])}



readTag :: Reader Tag
readTag = R (\bs -> case bs of
                    [] -> Nothing
                    (x:xs) -> case x of
                      0x00 -> Just(Tag [x], xs)
                      0x11 -> Just(Tag [x], xs))
