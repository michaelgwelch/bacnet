module BACnet.Tag
  (
  readTag,
  readAPTag,
  runReader,
  apNullTag,
  apTrueTag,
  apFalseTag,
  apUnsignedTag,
  readNullAPTag,
  readBoolAPTag,
  readUnsignedAPTag,
  Tag(..)
  ) where

import Control.Applicative
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
                | w <= 0x0FFF = UnsignedAP 3
                | w <= fromIntegral (maxBound :: Word32) = UnsignedAP 4
                | otherwise = undefined

failure :: Reader a
failure = fail ""

readTag :: Reader Tag
readTag = byte >>= \b ->
          if TC.isAP b then readAPTag else failure

assertNotEmptyAndAP :: [Word8] -> Maybe(Tag, [Word8]) -> Maybe(Tag, [Word8])
assertNotEmptyAndAP [] _ = Nothing
assertNotEmptyAndAP (b:bs) result = if TC.isAP b then result else Nothing

readAPTag :: Reader Tag
readAPTag = peek >>= \b ->
            if TC.isCS b then failure else
                case TC.tagNumber b of
                  0 -> readNullAPTag
                  1 -> readBoolAPTag
                  2 -> readUnsignedAPTag
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


readNullAPTag :: Reader Tag
readNullAPTag = sat (== 0x00) >> return NullAP

readBoolAPTag :: Reader Tag
readBoolAPTag = (sat (== 0x10) >> return (BoolAP False)) <|>
                (sat (== 0x11) >> return (BoolAP True))

readUnsignedAPTag :: Reader Tag
readUnsignedAPTag = pure (UnsignedAP . fromIntegral . TC.lvt) <*>
                    sat (\b -> TC.tagNumber b == 2 && TC.isAP b)
