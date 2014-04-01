{-# LANGUAGE TupleSections, RankNTypes #-}
module BACnet.Writer.Core
  (
    Writer,
    runW,
    empty,
    append,
    (<>),
    null,
    unsigned8,
    unsigned16,
    signed8,
    signed16,
    real,
    double,
    bytes
  ) where

import qualified Data.ByteString.Lazy as BS
import Data.ByteString.Lazy.Builder (
  word8, word16BE, int8, int16BE, lazyByteString, toLazyByteString,
  Builder, floatBE, doubleBE)
import Data.Word (Word8, Word16)
import Data.Int (Int8, Int16)
import Data.Monoid (Monoid, (<>), mempty, mappend)
import Prelude hiding (null)
import qualified Prelude as P
import Data.Bits ((.&.))
import qualified Control.Applicative as A
import qualified Control.Monad as M

-- | A writer
newtype Writer = W { unWriter :: Builder }

-- | A builder that writes the byte 0x00
null :: Writer
null = W $ word8 0x00

unsigned8 :: Word8 -> Writer
unsigned8 = W . word8

unsigned16 :: Word16 -> Writer
unsigned16 = W . word16BE

signed8 :: Int8 -> Writer
signed8 = W . int8

signed16 :: Int16 -> Writer
signed16 = W . int16BE

bytes :: [Word8] -> Writer
bytes ws = W $ lazyByteString $ BS.pack ws

real :: Float -> Writer
real = W . floatBE

double :: Double -> Writer
double = W . doubleBE

-- | Runs a writer and returns the result as an array of bytes.
runW :: Writer -> [Word8]
runW = BS.unpack . toLazyByteString . unWriter

empty :: Writer
empty = W (lazyByteString BS.empty)

append :: Writer -> Writer -> Writer
append (W b1) (W b2) = W (b1 <> b2)

instance Monoid Writer where
  mempty = empty
  mappend = append
