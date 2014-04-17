-- | Defines the 'Encodable' class and instances of 'Encodable' for
-- all the primitive types supported by 'Reader' and 'Writer'
-- An instance of 'Encodable' is able to encode a value to a 'BS.ByteString' and
-- decode a 'BS.ByteString' to a value. All 'Encodable' instances should have
-- the property
--
-- @ run bacnetDecode (runW $ bacnetEncode v) == v @
module BACnet.Encodable
  (
  Encodable(..),
  CSEncodable(..)
  ) where

import BACnet.Writer.Core
import qualified BACnet.Writer.Core as WC
import BACnet.Writer
import BACnet.Reader.Core
import BACnet.Reader
import BACnet.Prim
import Data.Int
import Data.Word
import Control.Applicative
import Data.Monoid (mempty, mconcat)


class Encodable a where
  bacnetEncode :: a -> Writer
  bacnetDecode :: Reader a



instance Encodable a => Encodable [a] where
  bacnetEncode = mconcat . map bacnetEncode
  bacnetDecode = many bacnetDecode

instance Encodable a => Encodable (Maybe a) where
  bacnetEncode Nothing = mempty
  bacnetEncode (Just v) = bacnetEncode v
  bacnetDecode = try (Just <$> bacnetDecode) <|> return Nothing

instance (Encodable a, Encodable b) => Encodable (Either a b) where
  bacnetEncode (Left a) = bacnetEncode a
  bacnetEncode (Right b) = bacnetEncode b
  bacnetDecode = try (Left <$> bacnetDecode) <|> (Right <$> bacnetDecode)

instance Encodable () where
  bacnetEncode = const writeNullAP
  bacnetDecode = readNullAP

instance Encodable Bool where
  bacnetEncode = writeBoolAP
  bacnetDecode = readBoolAP

instance Encodable Word32 where
  bacnetEncode = writeUnsignedAP
  bacnetDecode = readUnsignedAP

instance Encodable Int32 where
  bacnetEncode = writeSignedAP
  bacnetDecode = readSignedAP

instance Encodable Float where
  bacnetEncode = writeRealAP
  bacnetDecode = readRealAP

instance Encodable Double where
  bacnetEncode = writeDoubleAP
  bacnetDecode = readDoubleAP

instance Encodable OctetString where
  bacnetEncode = writeOctetStringAP . octetStringBytes
  bacnetDecode = OctetString <$> readOctetStringAP

instance Encodable CharacterString where
  bacnetEncode = writeStringAP . getString
  bacnetDecode = CharacterString <$> readStringAP

instance Encodable BitString where
  bacnetEncode = writeBitStringAP
  bacnetDecode = readBitStringAP

instance Encodable Enumerated where
  bacnetEncode = writeEnumeratedAP
  bacnetDecode = readEnumeratedAP

instance Encodable Date where
  bacnetEncode = writeDateAP
  bacnetDecode = readDateAP

instance Encodable Time where
  bacnetEncode = writeTimeAP
  bacnetDecode = readTimeAP

instance Encodable ObjectIdentifier where
  bacnetEncode = writeObjectIdentifierAP
  bacnetDecode = readObjectIdentifierAP

instance Encodable Any where
  bacnetEncode = writeAnyAP
  bacnetDecode = readAnyAP

class CSEncodable a where
  csbacnetEncode :: Word8 -> a -> Writer
  csbacnetDecode :: Word8 -> Reader a


instance CSEncodable () where
  csbacnetEncode = const . writeNullCS
  csbacnetDecode = readNullCS

instance CSEncodable Bool where
  csbacnetEncode = writeBoolCS
  csbacnetDecode = readBoolCS

instance CSEncodable Word32 where
  csbacnetEncode = writeUnsignedCS
  csbacnetDecode = readUnsignedCS

instance CSEncodable Int32 where
  csbacnetEncode = writeSignedCS
  csbacnetDecode = readSignedCS

instance CSEncodable Float where
  csbacnetEncode = writeRealCS
  csbacnetDecode = readRealCS

instance CSEncodable Double where
  csbacnetEncode = writeDoubleCS
  csbacnetDecode = readDoubleCS

instance CSEncodable OctetString where
  csbacnetEncode tn = writeOctetStringCS tn . octetStringBytes
  csbacnetDecode tn = OctetString <$> readOctetStringCS tn

instance CSEncodable CharacterString where
  csbacnetEncode tn = writeStringCS tn . getString
  csbacnetDecode tn = CharacterString <$> readStringCS tn

instance CSEncodable BitString where
  csbacnetEncode = writeBitStringCS
  csbacnetDecode = readBitStringCS

instance CSEncodable Enumerated where
  csbacnetEncode = writeEnumeratedCS
  csbacnetDecode = readEnumeratedCS

instance CSEncodable Date where
  csbacnetEncode = writeDateCS
  csbacnetDecode = readDateCS

instance CSEncodable Time where
  csbacnetEncode = writeTimeCS
  csbacnetDecode = readTimeCS

instance CSEncodable ObjectIdentifier where
  csbacnetEncode = writeObjectIdentifierCS
  csbacnetDecode = readObjectIdentifierCS

{-
data ValueUpdate = VU { identity :: Int32, val :: Word32 }
  deriving Show

instance Encodable ValueUpdate where
  bacnetDecode = VU <$> readSignedAP <*> readUnsignedAP
  bacnetEncode (VU i v) = (signed8 $ fromIntegral i) <>
                          (unsigned8 $ fromIntegral v)

data Register = Register Int8

instance Encodable Register where
  bacnetDecode = undefined
  bacnetEncode = undefined

data ValueRegister = VR ValueUpdate Register

instance Encodable ValueRegister where
  bacnetDecode = VR <$> bacnetDecode <*> bacnetDecode
  bacnetEncode (VR vu r) = (bacnetEncode vu) <> (bacnetEncode r)
-}
