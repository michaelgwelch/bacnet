-- | Defines the Writer type and a handful of primitive writers.
module BACnet.Writer.Core
  (
    -- * Introduction
    -- $Introduction
    Writer,
    runW,

    -- * The Writers
    wzero,
    unsigned8,
    unsigned16,
    unsigned32,
    signed8,
    signed16,
    real,
    double,
    bytes,
    bytestring

-- * About the Instances
-- $Instances
  ) where

import qualified Data.ByteString.Lazy as BS
import Data.ByteString.Lazy.Builder (
  word8, word16BE, word32BE, int8, int16BE, lazyByteString, toLazyByteString,
  Builder, floatBE, doubleBE)
import Data.Word (Word8, Word16, Word32)
import Data.Int (Int8, Int16)
import Data.Monoid (Monoid, (<>), mempty, mappend)
import Prelude hiding (null, maybe)


-- | A writer. All of the BACnet writers are this type. Writers
--   can be combined together to form more complex writers using
--   'mappend' or the equivalent operator ('<>')
newtype Writer = W { unWriter :: Builder }

instance Eq Writer where
  w1 == w2 = runW w1 == runW w2

instance Show Writer where
  show = show . runW

-- | A writer that writes the byte 0x00
--
-- >>>runW wzero
-- [0]
wzero :: Writer
wzero = W $ word8 0x00

-- | A function that writes a 'Word8' value. It will always write 1 byte.
--
-- >>>runW $ unsigned8 192
-- [192]
unsigned8 :: Word8 -> Writer
unsigned8 = W . word8

-- | A function that writes a 'Word16' value. It
--   will always write 2 bytes using big endian format.
--
-- >>>runW $ unsigned16 257
-- [1,1]
unsigned16 :: Word16 -> Writer
unsigned16 = W . word16BE

-- | A function that writes a 'Word32' value. It will
-- always write 4 bytes using big endian format.
--
-- >>>runW $ unsigned32 256
-- [0,0,1,0]
unsigned32 :: Word32 -> Writer
unsigned32 = W . word32BE

-- | A function that writes an Int8 value. It will always write 1 byte.
--
-- >>>runW $ signed8 (-1)
-- [255]
signed8 :: Int8 -> Writer
signed8 = W . int8

-- | A function that writes an Int16 value. It will always write 2 bytes
-- in big endian format.
--
-- >>>runW $ signed16 23
-- [0,23]
--
-- >>>runW $ signed16 (-100)
-- [255,156]
signed16 :: Int16 -> Writer
signed16 = W . int16BE

-- | A function that writes a list of 'Word8' values.
--
-- >>>runW $ bytes [1,2,100,101]
-- [1,2,100,101]
bytes :: [Word8] -> Writer
bytes = W . lazyByteString . BS.pack

-- | A function that writes a BS.ByteString
bytestring :: BS.ByteString -> Writer
bytestring = W . lazyByteString

-- | A function that writes a float value. It will always write
-- 4 bytes in big endian format.
--
-- >>>runW $ real 1.2e-5
-- [55,73,83,156]
real :: Float -> Writer
real = W . floatBE

-- | A function that writes a double value. It will always write
-- 8 bytes in big endian format.
--
-- >>>runW $ double 1.2e-5
--[62,233,42,115,113,16,228,84]
double :: Double -> Writer
double = W . doubleBE

-- | Runs a writer and returns the result as an array of bytes.
runW :: Writer -> [Word8]
runW = BS.unpack . toLazyByteString . unWriter

-- | A writer that writes nothing.
empty :: Writer
empty = W (lazyByteString BS.empty)

append :: Writer -> Writer -> Writer
append (W b1) (W b2) = W (b1 <> b2)

instance Monoid Writer where
  mempty = empty
  mappend = append

-- $Introduction
-- This module defines the type 'Writer'. This type is used to write
-- data to a 'BS.ByteString' or ['Word32']. This module also defines
-- some primitive writers and the 'runW' function which executes a writer.
--
-- Here's some examples:
--
-- >>>runW wzero
-- [0]
--
-- >>> runW $ (unsigned8 7) <> (unsigned8 5)
-- [7,5]

-- $Writers
-- These are the primitive writers.

-- $Instances
-- 'Writer' is an instance of 'Eq', 'Show' and 'Monoid'. The 'Monoid' instance
-- allows you to chain writers together using '<>', and of course 'mempty'
-- is the identity writer with respect to '<>'.
--
-- >>>runW $ wzero
-- [0]
--
-- >>>runW $ wzero <> mempty
-- [0]
--
-- >>>runW $ (unsigned8 100) <> (unsigned8 200)
-- [100,200]
