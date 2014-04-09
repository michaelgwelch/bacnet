module BACnet.Writer.Core
  (
    Writer,
    runW,
    empty,

    -- | Appends one writer to another
    append,
    -- | An infix synonym for 'append'
    (<>),
    null,
    unsigned8,
    unsigned16,
    unsigned32,
    signed8,
    signed16,
    real,
    double,
    bytes,
    bytestring,
    mconcat,
  ) where

import qualified Data.ByteString.Lazy as BS
import Data.ByteString.Lazy.Builder (
  word8, word16BE, word32BE, int8, int16BE, lazyByteString, toLazyByteString,
  Builder, floatBE, doubleBE)
import Data.Word (Word8, Word16, Word32)
import Data.Int (Int8, Int16)
import Data.Monoid (Monoid, (<>), mempty, mappend, mconcat)
import Prelude hiding (null, maybe)
import qualified Prelude as P


-- | A writer. All of the BACnet writers are this type. Writers
--   can be combined together to form more complex writers using
--   'mappend' or the equivalent operator ('<>')
newtype Writer = W { unWriter :: Builder }

instance Eq Writer where
  w1 == w2 = runW w1 == runW w2

instance Show Writer where
  show = show . runW

-- | A builder that writes the byte 0x00
null :: Writer
null = W $ word8 0x00

unsigned8 :: Word8 -> Writer
unsigned8 = W . word8

unsigned16 :: Word16 -> Writer
unsigned16 = W . word16BE

unsigned32 :: Word32 -> Writer
unsigned32 = W . word32BE

signed8 :: Int8 -> Writer
signed8 = W . int8

signed16 :: Int16 -> Writer
signed16 = W . int16BE

bytes :: [Word8] -> Writer
bytes ws = W $ lazyByteString $ BS.pack ws

bytestring :: BS.ByteString -> Writer
bytestring bs = W $ lazyByteString bs

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
