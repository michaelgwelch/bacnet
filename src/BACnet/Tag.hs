module BACnet.Tag
  (
  readTag,
  readAPTag,
  runReader,
  apNullTag,
  apTrueTag,
  apFalseTag,
  apUnsignedTag,
  readUnsignedAPTag,
  Tag(..)
  ) where

import Control.Exception (assert)
import Data.Word
import qualified BACnet.Tag.Core as TC
import BACnet.Reader.Core
data Tag =
          NullAP
        | BoolAP Bool
        | UnsignedAP Word32
        | SignedAP Word32
        | RealAP
        | DoubleAP
        | OctetStringAP Word32 -- length
        | CharacterStringAP Word8 Word32 -- encoding, length
        | BitStringAP Word32 -- length
        | EnumeratedAP Word32 -- length
        | DateAP
        | TimeAP
        | ObjectIdentifierAP
  deriving (Show, Eq)

apNullTag :: Tag
apNullTag = NullAP

apTrueTag :: Tag
apTrueTag = BoolAP True

apFalseTag :: Tag
apFalseTag = BoolAP False

apUnsignedTag :: Word -> Tag
apUnsignedTag w | w <= fromIntegral (maxBound :: Word8) = UnsignedAP 1
                | w <= fromIntegral (maxBound :: Word16) = UnsignedAP 2
                | w <= fromIntegral 0x0FFF = UnsignedAP 3
                | w <= fromIntegral (maxBound :: Word32) = UnsignedAP 4
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
                  0 -> readNullAPTag b
                  1 -> readBoolAPTag b
                  2 -> readUnsignedAPTag b
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


readNullAPTag :: Word8 -> Reader Tag
readNullAPTag 0x00 = return NullAP
readNullAPTag _ = failure

readBoolAPTag :: Word8 -> Reader Tag
readBoolAPTag 0x10 = return $ BoolAP False
readBoolAPTag 0x11 = return $ BoolAP True
readBoolAPTag _ = failure

readUnsignedAPTag :: Word8 -> Reader Tag
readUnsignedAPTag 0x21 = return $ UnsignedAP 1
readUnsignedAPTag _ = failure
