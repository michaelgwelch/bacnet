module BACnet.Tag
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
import qualified BACnet.Tag.Core as TC
import BACnet.Reader.Core
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

failure :: Reader a
failure = fail ""

readTag :: Reader Tag
readTag = byte >>= \b ->
          if TC.isAP b then readAPTag else failure

assertNotEmptyAndAP :: [Word8] -> Maybe(Tag, [Word8]) -> Maybe(Tag, [Word8])
assertNotEmptyAndAP [] _ = Nothing
assertNotEmptyAndAP (b:bs) result = if (TC.isAP b) then result else Nothing

readAPTag :: Reader Tag
readAPTag = byte >>= \b ->
            if TC.isCS b then failure else
                case TC.tagNumber b of
                  0 -> readAPNullTag b
                  1 -> readAPBoolTag b
                  2 -> readAPUnsignedTag b
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


readAPNullTag :: Word8 -> Reader Tag
readAPNullTag 0x00 = return $ Tag [0x00]
readAPNullTag _ = failure

readAPBoolTag :: Word8 -> Reader Tag
readAPBoolTag 0x10 = return $ Tag [0x10]
readAPBoolTag 0x11 = return $ Tag [0x11]
readAPBoolTag _ = failure

readAPUnsignedTag :: Word8 -> Reader Tag
readAPUnsignedTag 0x21 = return $ Tag [0x21]
readAPUnsignedTag _ = failure
