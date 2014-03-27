{-# LANGUAGE TupleSections, RankNTypes #-}
module BACnet.Writer.Core
  (
    Writer,
    runW,
    nullAPTag,
    boolAPTag,
    unsignedAPTag,
    empty,
    append,
    (<>)
  ) where

import qualified Data.ByteString.Lazy as BS
import Data.ByteString.Lazy.Builder
import Data.Word
import Data.Monoid
import Prelude hiding (null)
import qualified Prelude as P
import Data.Bits
import qualified Control.Applicative as A
import qualified Control.Monad as M
import BACnet.Writer.ByteString

-- | A writer - this type actually provides no value that isn't already
--   provided directly by builder. The tag functions below should go in Tag.hs
--   So this whole file can be deleted.
newtype Writer = W { unWriter :: Builder }

-- | Runs a writer and returns the result as an array of bytes.
runW :: Writer -> [Word8]
runW = BS.unpack . toLazyByteString . unWriter


null :: Builder
null = word8 0x00

unfoldWord' :: Word -> BS.ByteString
unfoldWord' = unfoldl unfoldStep
  where unfoldStep 0 = Nothing
        unfoldStep n = Just(fromIntegral (n .&. 0xFF), shiftR n 8)

unfoldWord :: Word -> [Word8] -> (Word32,[Word8])
unfoldWord 0 bs = (0, bs)
unfoldWord n bs =
  let (len,bs') = unfoldWord (shiftR n 8) (fromIntegral (n .&. 0xFF) : bs)
  in (len+1, bs')

-- | Writes a tag appropriate for an application encoded null value
nullAPTag :: Writer
nullAPTag = W null

boolAPTag :: Bool -> Writer
boolAPTag b = W (word8 (if b then 0x11 else 0x10))

unsignedAPTag :: Word32 -> Writer
unsignedAPTag 0 = W (word16BE 0x2000)
unsignedAPTag v =
  let initialOctet = 0x20 + len
      (len, bs)    = unfoldWord (fromIntegral v) []
  in W (lazyByteString $ BS.pack $ (fromIntegral initialOctet) : bs)

empty :: Writer
empty = W (lazyByteString BS.empty)

append :: Writer -> Writer -> Writer
append (W b1) (W b2) = W (b1 <> b2)

instance Monoid Writer where
  mempty = empty
  mappend = append
