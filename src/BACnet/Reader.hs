module BACnet.Reader
  (
    Reader,
    readUnsignedAP
  ) where

import Data.Word
import BACnet.Tag
import BACnet.Reader.Core

readUnsignedAP :: Reader Word
readUnsignedAP = byte >>= \b -> readUnsignedAPTag b >>= \(UnsignedAP len) ->
                 bytes (fromIntegral len) >>= \bs ->
                 let words = map fromIntegral bs in
                 return $ foldl (\acc w -> acc * 256 + w) 0 words
