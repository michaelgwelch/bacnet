-- | Defines a reader for reading BACnet encoded values
module BACnet.Reader
  (
    Reader,
    runReader,
    run,
    readNullAP,
    readBoolAP,
    readUnsignedAP,
    readSignedAP,
    readRealAP,
    readDoubleAP,
    readOctetStringAP
  ) where

import Control.Applicative
import Control.Monad
import Data.Binary
import Data.Binary.Get
import Data.Binary.IEEE754
import Data.Word
import Data.Int
import BACnet.Tag
import BACnet.Reader.Core
import Data.ByteString.Lazy hiding (foldl)
import qualified Data.ByteString.Lazy as BS

-- | Reads an application encoded null value
readNullAP :: Reader ()
readNullAP = void readNullAPTag

-- | Reads an application encoded boolean value
readBoolAP :: Reader Bool
readBoolAP = boolVal <$> readBoolAPTag

-- | Reads an application encoded unsigned integral value
readUnsignedAP' :: Reader Word
readUnsignedAP' = readUnsignedAPTag >>=
                  content foldbytes

-- | Reads an application encoded unsigned integral value.
readUnsignedAP :: Reader Word32
readUnsignedAP = fromIntegral <$> readUnsignedAP'

readSignedAP' :: Reader Int
readSignedAP' = readSignedAPTag >>=
                content foldsbytes

readSignedAP :: Reader Int32
readSignedAP = fromIntegral <$> readSignedAP'

readRealAP :: Reader Float
readRealAP = runGet getFloat32be <$ readRealAPTag <*> bytes 4

readDoubleAP :: Reader Double
readDoubleAP = runGet getFloat64be <$ readDoubleAPTag <*> bytes 8

readOctetStringAP :: Reader [Word8]
readOctetStringAP = readOctetStringAPTag >>=
                    (content id >=>
                     return . BS.unpack)

foldbytes :: BS.ByteString -> Word
foldbytes = BS.foldl (\acc w -> acc * 256 + fromIntegral w) 0

-- | The reader @content f t@ reads a 'BS.ByteString' of length indicated by the
--   length specified in the 'Tag', @t@, and returns the value obtained by applying
--   @f@ to that ByteString. 
content :: (BS.ByteString -> a) -> Tag -> Reader a
content f t = f <$> bytes (fromIntegral $ tagLength t)

foldsbytes :: BS.ByteString -> Int
foldsbytes bs | BS.null bs = 0
              | otherwise =
  let (val, len) = BS.foldl (\(accv,accl) w -> (accv * 256 + fromIntegral w, accl+1)) (0,0) (BS.tail bs)
  in fromIntegral (fromIntegral (BS.head bs) :: Int8) * 256 ^ len + val
