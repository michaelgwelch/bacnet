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
  readSignedAPTag,
  readRealAPTag,
  readOctetStringAPTag,
  Tag(..)
  ) where

import Control.Monad
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
readUnsignedAPTag = readAP 2 UnsignedAP

readSignedAPTag :: Reader Tag
readSignedAPTag = readAP 3 SignedAP

readRealAPTag :: Reader Tag
readRealAPTag = sat (== 0x44) >> return RealAP

readOctetStringAPTag :: Reader Tag
readOctetStringAPTag = readAP 6 OctetStringAP

readAP :: Word8 -> (Word32 -> Tag) -> Reader Tag
readAP tn co = sat (\b -> TC.tagNumber b == tn && TC.isAP b) >>=
               (lengthOfContent >=> return . co)

lengthOfContent :: Word8 -> Reader Word32
lengthOfContent b | TC.lvt b < 5 = return . fromIntegral $ TC.lvt b
                  | TC.lvt b == 5 = lengthOfContent'
                  | otherwise = failure

-- | Reads the next byte. If it is < 254 it returns that value
--   If it is 254, then reads the next 2 bytes as a Word32
--   If it is 255, then reads the next 4 bytes as a Word32
lengthOfContent' :: Reader Word32
lengthOfContent' = byte >>= \b ->
                    if b < 254 then
                      return $ fromIntegral b
                    else
                      fmap foldbytes (bytes (if b == 254 then 2 else 4))

foldbytes :: [Word8] -> Word32
foldbytes = foldl (\acc w -> acc * 256 + fromIntegral w) 0
