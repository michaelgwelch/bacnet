module BACnet.Encodable
  (
  Encodable,
  bacnetEncode,
  bacnetDecode,
  ) where

import BACnet.Writer.Core
import qualified BACnet.Writer.Core as WC
import BACnet.Reader.Core
import BACnet.Reader
import BACnet.Prim
import Control.Applicative ((<$>), (<*>))
import Data.Int
import Data.Word


class Encodable a where
  bacnetEncode :: a -> Writer
  bacnetDecode :: Reader a

instance Encodable a => Encodable [a] where
  bacnetEncode = mconcat . map bacnetEncode
  bacnetDecode = many bacnetDecode

instance Encodable a => Encodable (Maybe a) where
  bacnetEncode Nothing = WC.empty
  bacnetEncode (Just v) = bacnetEncode v
  bacnetDecode = try (Just <$> bacnetDecode) <|> return Nothing

instance (Encodable a, Encodable b) => Encodable (Either a b) where
  bacnetEncode (Left a) = bacnetEncode a
  bacnetEncode (Right b) = bacnetEncode b
  bacnetDecode = try (Left <$> bacnetDecode) <|> (Right <$> bacnetDecode)

instance Encodable Bool where
  bacnetEncode = undefined
  bacnetDecode = readBoolAP

instance Encodable Word where
  bacnetEncode = undefined
  bacnetDecode = fromIntegral <$> readUnsignedAP

instance Encodable Int where
  bacnetEncode = undefined
  bacnetDecode = fromIntegral <$> readSignedAP

instance Encodable Float where
  bacnetEncode = undefined
  bacnetDecode = readRealAP

instance Encodable Double where
  bacnetEncode = undefined
  bacnetDecode = readDoubleAP

instance Encodable OctetString where
  bacnetEncode = undefined
  bacnetDecode = OctetString <$> readOctetStringAP

instance Encodable CharacterString where
  bacnetEncode = undefined
  bacnetDecode = CharacterString <$> readStringAP

instance Encodable BitString where
  bacnetEncode = undefined
  bacnetDecode = readBitStringAP

instance Encodable Enumerated where
  bacnetEncode = undefined
  bacnetDecode = readEnumeratedAP

instance Encodable Date where
  bacnetEncode = undefined
  bacnetDecode = readDateAP

instance Encodable Time where
  bacnetEncode = undefined
  bacnetDecode = readTimeAP

instance Encodable ObjectIdentifier where
  bacnetEncode = undefined
  bacnetDecode = readObjectIdentifierAP


class CSEncodable a where
  csbacnetEncode :: Word8 -> a -> Writer
  csbacnetDecode :: Word8 -> Reader a

{-}
instance CSEncodable Bool where
  csbacnetEncode = undefined
  csbacnetDecode = readBoolCS -}

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
