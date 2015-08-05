{-# OPTIONS_GHC -fno-warn-orphans #-}
module BACnet.EncodableSpec where

import Test.Hspec
import Test.QuickCheck
import BACnet.Tag.Core (TagNumber)
import BACnet.Writer
import BACnet.Reader
import BACnet.Encodable
import BACnet.Prim
import Data.Maybe
import Data.Word (Word32)
import Data.Int (Int32)
import Control.Applicative
import Control.Monad

class (Eq a, Show a, Encodable a) => RoundTrippable a where
  roundTrip :: a -> Expectation
  roundTrip a = run bacnetDecode (runW $ bacnetEncode a) `shouldBe` a

instance RoundTrippable ()
instance RoundTrippable Bool
instance RoundTrippable Word32
instance RoundTrippable Int32
instance RoundTrippable Float
instance RoundTrippable Double
instance RoundTrippable OctetString
instance RoundTrippable CharacterString
instance RoundTrippable BitString
instance RoundTrippable Enumerated
instance RoundTrippable Date
instance RoundTrippable Time
instance RoundTrippable ObjectIdentifier
instance RoundTrippable Any

instance RoundTrippable a => RoundTrippable [a]
instance RoundTrippable a => RoundTrippable (Maybe a)
instance (RoundTrippable a, RoundTrippable b) => RoundTrippable (Either a b)

class (Eq a, Show a, CSEncodable a) => CSRoundTrippable a where
  csroundTrip :: TagNumber -> a -> Expectation
  csroundTrip tn a = run (csbacnetDecode tn) (runW $ csbacnetEncode tn a) `shouldBe` a

instance CSRoundTrippable ()
instance CSRoundTrippable Bool
instance CSRoundTrippable Word32
instance CSRoundTrippable Int32
instance CSRoundTrippable Float
instance CSRoundTrippable Double
instance CSRoundTrippable OctetString
instance CSRoundTrippable CharacterString
instance CSRoundTrippable BitString
instance CSRoundTrippable Enumerated
instance CSRoundTrippable Date
instance CSRoundTrippable Time
instance CSRoundTrippable ObjectIdentifier

instance Arbitrary OctetString where
  arbitrary = OctetString <$> arbitrary
  shrink = map OctetString . shrink . octetStringBytes

instance Arbitrary CharacterString where
  arbitrary = CharacterString <$> arbitrary
  shrink = map CharacterString . shrink . characterStringValue

instance Arbitrary Enumerated where
  arbitrary = liftM Enumerated arbitrary

instance Arbitrary Date where
  arbitrary = liftM4 Date arbitrary arbitrary arbitrary arbitrary

-- This could be implemented just like Date, but again, I'm just
-- playing around with different styles.
instance Arbitrary Time where
  arbitrary = Time <$> arbitrary <*> arbitrary
                   <*> arbitrary <*> arbitrary

instance Arbitrary ObjectIdentifier where
  arbitrary = ObjectIdentifier <$> arbitrary
  shrink = map ObjectIdentifier . shrink . objectIdentifierValue

instance Arbitrary BitString where
  arbitrary =
    do
      unusedBits <- choose(0,7)
      singleByte <- arbitrary
      bytes <- arbitrary
      return . fromJust $ bitString unusedBits (singleByte : bytes)

instance Arbitrary Any where
  arbitrary = oneof [
    return NullAP,
    BooleanAP <$> arbitrary,
    UnsignedAP <$> arbitrary,
    SignedAP <$> arbitrary,
    RealAP <$> arbitrary,
    DoubleAP <$> arbitrary,
    OctetStringAP <$> arbitrary,
    CharacterStringAP <$> arbitrary,
    BitStringAP <$> arbitrary,
    EnumeratedAP <$> arbitrary,
    DateAP <$> arbitrary,
    TimeAP <$> arbitrary,
    ObjectIdentifierAP <$> arbitrary
    ]


spec :: Spec
spec =
  do
    describe "Encodable ()" $
      it "round trips" $
        property $ \v -> roundTrip (v :: ())

    describe "Encodable Bool" $
      it "round trips properly" $
        property $ \b -> roundTrip (b :: Bool)

    describe "Encodable Word32" $
      it "round trips" $
        property $ \w -> roundTrip (w :: Word32)

    describe "Encodable [Word32]" $
      it "round trips" $
        property $ \ws -> roundTrip (ws :: [Word32])

    describe "Encodable (Maybe Word32)" $
      it "round trips" $
        property $ \mw -> roundTrip (mw :: Maybe Word32)

    describe "Encodable (Either Word32 Int32)" $
      it "round trips" $
        property $ \e -> roundTrip (e :: Either Word32 Int32)

    describe "Encodable Int32" $
      it "round trips" $
        property $ \i -> roundTrip (i :: Int32)

    describe "Encodable Real" $
      it "round trips" $
        property $ \f -> roundTrip (f :: Float)

    describe "Encodable Double" $
      it "round trips" $
        property $ \d -> roundTrip (d :: Double)

    describe "Encodable OctetString" $
      it "round trips" $
        property $ \s -> roundTrip (s :: OctetString)

    describe "Encodable String" $
      it "round trips" $
        property $ \s -> roundTrip (s :: CharacterString)

    describe "Encodable BitString" $
      it "round trips" $
        property $ \s -> roundTrip (s :: BitString)

    describe "Encodable Enumerated" $
      it "round trips" $
        property $ \e -> roundTrip (e :: Enumerated)

    describe "Encodable Date" $
      it "round trips" $
        property $ \d -> roundTrip (d :: Date)

    describe "Encodable Time" $
      it "round trips" $
        property $ \t -> roundTrip (t :: Time)

    describe "Encodable ObjectIdentifier" $
      it "round trips" $
        property $ \o -> roundTrip (o :: ObjectIdentifier)

    describe "Encodable Any" $
      it "round trips" $
        property $ \v -> roundTrip (v :: Any)

    describeRoundTrip "Null" (undefined :: ())

    describeRoundTrip "Bool" (undefined :: Bool)

    describeRoundTrip "Word32" (undefined :: Word32)

    describeRoundTrip "Int32" (undefined :: Int32)

    describeRoundTrip "Float" (undefined :: Float)

    describeRoundTrip "Double" (undefined :: Double)

    describeRoundTrip "OctetString" (undefined :: OctetString)

    describeRoundTrip "CharacterString" (undefined :: CharacterString)

    describeRoundTrip "BitString" (undefined :: BitString)

    describeRoundTrip "Enumerated" (undefined :: Enumerated)

    describeRoundTrip "Date" (undefined :: Date)

    describeRoundTrip "Time" (undefined :: Time)

    describeRoundTrip "ObjectIdentifier" (undefined :: ObjectIdentifier)


describeRoundTrip :: (Arbitrary a, CSRoundTrippable a) => String -> a -> Spec
describeRoundTrip typeName val =
  describe ("CSEncodable " ++ typeName) $
    it "round trips" $
      property $ \tn v -> csroundTrip tn (v `asTypeOf` val)
