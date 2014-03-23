module Tag
  (
  readTag,
  readAPTag,
  runReader,
  apNullTag,
  apTrueTag,
  apFalseTag,
  apUnsignedTag
  ) where

import Control.Exception (assert)
import Data.Word
import qualified Tag.Core as TC
data Tag = Tag [Word8] deriving (Show, Eq)

apNullTag :: Tag
apNullTag = Tag [0x00]

apTrueTag :: Tag
apTrueTag = Tag [0x11]

apFalseTag :: Tag
apFalseTag = Tag [0x10]

apUnsignedTag :: Word -> Tag
apUnsignedTag w | w <= fromIntegral (maxBound :: Word8) = Tag [0x21]
                | w <= fromIntegral (maxBound :: Word16) = Tag [0x22]
                | w <= fromIntegral 0x0FFF = Tag [0x23]
                | w <= fromIntegral (maxBound :: Word32) = Tag [0x24]
                | otherwise = undefined

newtype Reader a = R {runReader :: [Word8] -> Maybe (a, [Word8])}


readTag :: Reader Tag
readTag = R (\bs -> case bs of
                    [] -> Nothing
                    (x:xs) -> if TC.isAP x then runReader readAPTag bs
                                        else Nothing)

assertNotEmptyAndAP :: [Word8] -> Maybe(Tag, [Word8]) -> Maybe(Tag, [Word8])
assertNotEmptyAndAP [] _ = Nothing
assertNotEmptyAndAP (b:bs) result = if (TC.isAP b) then result else Nothing

readAPTag :: Reader Tag
readAPTag = R (\bs -> assertNotEmptyAndAP bs $
                case TC.tagNumber $ head bs of
                  0 -> readAPNullTag bs
                  1 -> readAPBoolTag bs
                  2 -> readAPUnsignedTag bs
                  3 -> undefined
                  4 -> undefined
                  5 -> undefined
                  6 -> undefined
                  7 -> undefined
                  8 -> undefined
                  9 -> undefined
                  10-> undefined
                  11-> undefined
                  12-> undefined
            )

type ReaderTag' = [Word8] -> Maybe(Tag, [Word8])

readAPNullTag :: ReaderTag'
readAPNullTag (0x00:bs) = Just(apNullTag, bs)
readAPNullTag _ = Nothing

readAPBoolTag :: ReaderTag'
readAPBoolTag (0x10:bs) = Just(apFalseTag, bs)
readAPBoolTag (0x11:bs) = Just(apTrueTag, bs)
readAPBoolTag _ = Nothing

readAPUnsignedTag :: ReaderTag'
readAPUnsignedTag (0x21:bs) = Just(Tag [0x21], bs)
