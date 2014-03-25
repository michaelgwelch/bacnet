module BACnet.Reader
  (
    Reader,
    runReader,
    readNullAP,
    readBoolAP,
    readUnsignedAP,
    readSignedAP,
    readRealAP,
    readOctetStringAP
  ) where

import Control.Applicative
import Data.Binary
import Data.Binary.Get
import Data.Binary.IEEE754
import Data.Word
import Data.Int
import BACnet.Tag
import BACnet.Reader.Core
import Data.ByteString.Lazy hiding (foldl)


readNullAP :: Reader ()
readNullAP = pure (const ()) <*> readNullAPTag

readBoolAP :: Reader Bool
readBoolAP = pure (\(BoolAP val) -> val) <*> readBoolAPTag

foldbytes :: [Word8] -> Word
foldbytes = foldl (\acc w -> acc * 256 + fromIntegral w) 0


readUnsignedAP' :: Reader Word
readUnsignedAP' = readUnsignedAPTag >>= \(UnsignedAP len) ->
                  bytes (fromIntegral len) >>= \bs ->
                  return $ foldbytes bs

readUnsignedAP :: Reader Word32
readUnsignedAP = fmap fromIntegral readUnsignedAP'

foldsbytes :: [Word8] -> Int
foldsbytes [] = 0
foldsbytes (sb:sbs) = let (val, len) = foldl (\(accv,accl) w -> (accv * 256 + fromIntegral w, accl+1)) (0,0) sbs in
                      fromIntegral (fromIntegral sb :: Int8) * 256 ^ len + val

readSignedAP' :: Reader Int
readSignedAP' = readSignedAPTag >>= \(SignedAP len) ->
                bytes (fromIntegral len) >>= \bs ->
                return $ foldsbytes bs

readSignedAP :: Reader Int32
readSignedAP = fmap fromIntegral readSignedAP'


readRealAP :: Reader Float
readRealAP = readRealAPTag >>
             bytes 4 >>= \bs ->
             return $ runGet getFloat32be $ pack bs

readOctetStringAP :: Reader [Word8]
readOctetStringAP = readOctetStringAPTag >>= \(OctetStringAP len) ->
                    bytes (fromIntegral len)
