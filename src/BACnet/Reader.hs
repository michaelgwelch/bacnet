-- | Defines 'Reader's for reading BACnet encoded values
module BACnet.Reader
  (
    Reader,
    run,
    readAnyAP,
    readNullAP,
    readBoolAP,
    readUnsignedAP,
    readSignedAP,
    readRealAP,
    readDoubleAP,
    readOctetStringAP,
    readStringAP,
    readBitStringAP,
    readEnumeratedAP,
    readDateAP,
    readTimeAP,
    readObjectIdentifierAP
  ) where

import Control.Applicative
import Control.Monad
import Data.Binary
import Data.Binary.Get
import Data.Binary.IEEE754
import Data.Int
import Data.Maybe
import BACnet.Tag
import BACnet.Reader.Core
import qualified BACnet.Prim as Prim
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.UTF8 as UTF8


readAnyAP :: Reader Prim.Any
readAnyAP =
  do
    t <- readAnyAPTag
    case t of
      NullAP -> return Prim.NullAP
      BoolAP b -> return $ Prim.BooleanAP b
      UnsignedAP _ -> Prim.UnsignedAP <$> readUnsigned t
      SignedAP _ -> Prim.SignedAP <$> readSigned t
      RealAP -> Prim.RealAP <$> readReal
      DoubleAP -> Prim.DoubleAP <$> readDouble
      OctetStringAP _ -> Prim.OctetStringAP <$> readOctetString t
      CharacterStringAP _ -> Prim.CharacterStringAP <$> readString t
      BitStringAP _ -> Prim.BitStringAP <$> readBitString t
      EnumeratedAP _ -> Prim.EnumeratedAP <$> readEnumerated t
      DateAP -> Prim.DateAP <$> readDate
      TimeAP -> Prim.TimeAP <$> readTime
      ObjectIdentifierAP -> Prim.ObjectIdentifierAP <$> readObjectIdentifier t
      _ -> fail "Invalid AP Tag"


-- | Reads an application encoded null value
readNullAP :: Reader ()
readNullAP = void readNullAPTag

readNullCS :: Word8 -> Reader ()
readNullCS = void . readNullCSTag

-- | Reads an application encoded boolean value
readBoolAP :: Reader Bool
readBoolAP = boolVal <$> readBoolAPTag

-- | Reads an application encoded unsigned integral value
readUnsignedAP' :: Reader Word
readUnsignedAP' = readUnsignedAPTag >>=
                  readUnsigned

-- | Reads an application encoded unsigned integral value.
readUnsignedAP :: Reader Word32
readUnsignedAP = fromIntegral <$> readUnsignedAP'

readSignedAP' :: Reader Int
readSignedAP' = readSignedAPTag >>=
                readSigned

readSignedAP :: Reader Int32
readSignedAP = fromIntegral <$> readSignedAP'

readRealAP :: Reader Float
readRealAP = readRealAPTag *> readReal

readReal :: Reader Float
readReal = runGet getFloat32be <$> bytestring 4

readDoubleAP :: Reader Double
readDoubleAP = readDoubleAPTag >> readDouble

readDouble :: Reader Double
readDouble = runGet getFloat64be <$> bytestring 8

readOctetStringAP :: Reader [Word8]
readOctetStringAP = readOctetStringAPTag >>= readOctetString

readOctetString :: Tag -> Reader [Word8]
readOctetString t = content id t >>= return . BS.unpack

readStringAP :: Reader String
readStringAP = readStringAPTag >>= readString

readString :: Tag -> Reader String
readString t =
  do
    sat (==0x00) -- encoding is 0x00 which is the value used to indicate UTF-8 (formerly ANSI X3.4)
    bs <- bytestring $ fromIntegral $ tagLength t - 1
    return $ UTF8.toString bs

readBitStringAP :: Reader Prim.BitString
readBitStringAP = readBitStringAPTag >>= readBitString

readBitString :: Tag -> Reader Prim.BitString
readBitString t =
  do
    guard (tagLength t /= 0)
    bs <- content id t
    let bString = Prim.bitString (BS.head bs) (BS.unpack $ BS.tail bs)
    maybe (fail "Invalid BitString encoding") return bString

readEnumeratedAP :: Reader Prim.Enumerated
readEnumeratedAP = readEnumeratedAPTag >>= readEnumerated

readEnumerated :: Tag -> Reader Prim.Enumerated
readEnumerated t = Prim.Enumerated <$> readUnsigned t

readDateAP :: Reader Prim.Date
readDateAP = readDateAPTag >> readDate

readDate :: Reader Prim.Date
readDate = Prim.Date <$> byte <*> byte <*> byte <*> byte

readTimeAP :: Reader Prim.Time
readTimeAP = readTimeAPTag >> readTime

readTime :: Reader Prim.Time
readTime = Prim.Time <$> byte <*> byte <*> byte <*> byte

readObjectIdentifierAP :: Reader Prim.ObjectIdentifier
readObjectIdentifierAP = readObjectIdentifierAPTag >>= readObjectIdentifier

readObjectIdentifier :: Tag -> Reader Prim.ObjectIdentifier
readObjectIdentifier t = Prim.ObjectIdentifier . fromIntegral <$> readUnsigned t

readUnsigned :: Tag -> Reader Word
readUnsigned = content foldbytes

readSigned :: Tag -> Reader Int
readSigned = content foldsbytes

foldbytes :: BS.ByteString -> Word
foldbytes = BS.foldl (\acc w -> acc * 256 + fromIntegral w) 0

-- | The reader @content f t@ reads a 'BS.ByteString' of length indicated by the
--   length specified in the 'Tag', @t@, and returns the value obtained by applying
--   @f@ to that ByteString.
content :: (BS.ByteString -> a) -> Tag -> Reader a
content f t = f <$> bytestring (fromIntegral $ tagLength t)

foldsbytes :: BS.ByteString -> Int
foldsbytes bs | BS.null bs = 0
              | otherwise =
  let (val, len) = BS.foldl (\(accv,accl) w -> (accv * 256 + fromIntegral w, accl+1)) (0,0) (BS.tail bs)
  in fromIntegral (fromIntegral (BS.head bs) :: Int8) * 256 ^ len + val
