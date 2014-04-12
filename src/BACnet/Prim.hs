module BACnet.Prim
  (
    Null(..),
    CharacterString(..),
    OctetString(..),
    BitString,
    bitStringUnusedBits,
    bitStringBytes,
    bitStringLength,
    bitString,
    bitStringEmpty,
    testBit,
    Enumerated(..),
    Date(..),
    Time(..),
    ObjectIdentifier(..),
    objectIdentifier,
    Any(..)
  ) where

import Data.Word (Word8, Word16, Word32, Word)
import Data.Bits (shiftR, shiftL, (.&.), (.|.))
import Control.Monad (liftM, liftM2)
import qualified Data.Bits as B

newtype Null = Null { unNull :: () }

newtype CharacterString = CharacterString { getString :: String }
newtype OctetString = OctetString { getOSBytes :: [Word8] }
  deriving (Eq, Show)
data BitString = BitString { bitStringUnusedBits :: Word8, bitStringBytes :: [Word8] }
  deriving (Eq, Show)

bitStringEmpty :: BitString
bitStringEmpty = BitString 0 []

bitString :: Word8 -> [Word8] -> Maybe BitString
bitString n bs | null bs && n /= 0 || n >= 8 = Nothing
               | n < 8                       = Just $ BitString n bs

bitStringLength :: BitString -> Int
bitStringLength bs = 1 + length (bitStringBytes bs)

-- | The function @testBit b n@ returns true if the nth bit is set. Counting
--   starts at 0 at the left most bit of the left most byte of 'getBytes'
testBit :: BitString -> Word -> Bool
testBit (BitString unusedBits bs) = testBit' unusedBits bs

testBit' :: Word8 -> [Word8] -> Word -> Bool
testBit' _ [] _ = error "empty list of bytes"
testBit' u [b] n | n > fromIntegral (7 - u) = error "index out of bounds"
                 | otherwise   = B.testBit b (fromIntegral $ 7 - n)
testBit' u (b:bs) n | n > 7 = testBit' u bs (n - 8)
                    | otherwise = B.testBit b (fromIntegral $ 7 - n)

newtype Enumerated = Enumerated { getEnumValue :: Word } deriving (Eq, Show)

data Date = Date
  {
    getYear :: Word8,
    getMonth :: Word8,
    getDayOfMonth :: Word8,
    getDayOfWeek :: Word8
  } deriving (Show, Eq)

data Time = Time
  {
    getHour :: Word8,
    getMinute :: Word8,
    getSecond :: Word8,
    getHundredth :: Word8
  } deriving (Show, Eq)

newtype ObjectIdentifier = ObjectIdentifier { getRawValue :: Word32 }
  deriving (Show, Eq)

objectIdentifier :: Word16 -> Word32 -> Maybe ObjectIdentifier
objectIdentifier ot inum =
  liftM ObjectIdentifier rawValue
  where rawValue = liftM2 (+) inum' ot'
        inum'
          | inum > 0x3FFFFF = Nothing
          | otherwise       = Just inum
        ot'
          | ot > 0x3FF      = Nothing
          | otherwise       = Just $ shiftL (fromIntegral ot) 22

getObjectType :: ObjectIdentifier -> Word16
getObjectType = fromIntegral . (.&. 0x3F) . flip shiftR 22 . getRawValue

getInstanceNumber :: ObjectIdentifier -> Word32
getInstanceNumber = (.&. 0x003FFFFF) . getRawValue


data Any =
    NullAP
  | BooleanAP Bool
  | UnsignedAP Word
  | SignedAP Int
  | RealAP Float
  | DoubleAP Double
  | OctetStringAP [Word8]
  | CharacterStringAP String
  | BitStringAP BitString
  | EnumeratedAP Enumerated
  | DateAP Date
  | TimeAP Time
  | ObjectIdentifierAP ObjectIdentifier
