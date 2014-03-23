module BACnet.Reader
  (
    Reader,
    runReader,
    readNullAP,
    readBoolAP,
    readUnsignedAP
  ) where

import Control.Applicative
import Data.Word
import BACnet.Tag
import BACnet.Reader.Core


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
