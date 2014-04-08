-- | A neat idea. The type in this module WriterC is a Contravariant
--   Functor which allows a user to easily combine writers in interesting
--   fashion with the use of 'contramap'. It's not really necessary, but fun.
module BACnet.WriterT where

import BACnet.Writer.Core
import BACnet.Writer
import Data.Word
import Data.Int

import Data.Functor.Contravariant

writerMap :: (b -> a) -> WriterC a -> WriterC b
writerMap f wa = W' (unWriterC wa . f)

instance Contravariant WriterC where
  contramap = writerMap


newtype WriterC a = W' { unWriterC :: a -> Writer }

runW' :: WriterC a -> a -> [Word8]
runW' w = runW . unWriterC w


-- Sample WriterC instances

writeUnsignedAP1 :: WriterC Word32
writeUnsignedAP1 = W' writeUnsignedAP

writeUnsignedAP2 :: WriterC Word8
writeUnsignedAP2 = fromIntegral >$< writeUnsignedAP1

writeEvenAP :: WriterC Word32
writeEvenAP = (*2) >$< writeUnsignedAP1

writeNumSat :: (Word32 -> Bool) -> WriterC Word32
writeNumSat = (>$< W' writeBoolAP)

lengthS :: String -> Int32
lengthS = fromIntegral . length

writeStringLength :: WriterC String
writeStringLength = lengthS >$< W' writeSignedAP

writeBoolIfStringIsNotEmpty :: WriterC String
writeBoolIfStringIsNotEmpty = not . Prelude.null >$< W' writeBoolAP
