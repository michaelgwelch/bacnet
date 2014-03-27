{-# LANGUAGE TupleSections #-}
module BACnet.Writer.Core where

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

newtype Writer a = W { unWriter :: (a, Builder) }

runW :: Writer a -> [Word8]
runW = BS.unpack . toLazyByteString . snd . unWriter


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
nullAPTag :: Writer ()
nullAPTag = W ((), null)

boolAPTag :: Bool -> Writer Bool
boolAPTag b = W (b, word8 (if b then 0x11 else 0x10))

unsignedAPTag :: Word32 -> Writer Word32
unsignedAPTag 0 = W (0, word16BE 0x2000)
unsignedAPTag v =
  let initialOctet = 0x20 + len
      (len, bs)    = unfoldWord (fromIntegral v) []
  in W (v, lazyByteString $ BS.pack $ (fromIntegral initialOctet) : bs)

instance M.Monad Writer where
  return v = W (v, mempty)
  wa >>= f = let W (x, b) = wa
                 W (y, b') = f x
             in  W (y, b `mappend` b')

instance Functor Writer where
  fmap f wa = let W (a, b) = wa
              in  W (f a, b)

instance A.Applicative Writer where
  pure = return
  (<*>) = M.ap
