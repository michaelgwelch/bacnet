module BACnet.EncodableSpec where

import Test.Hspec
import Test.QuickCheck
import BACnet.Writer
import BACnet.Reader
import BACnet.Encodable
import BACnet.Prim
import Data.Word (Word32)
import Data.Int (Int32)
import Control.Applicative
import Control.Monad

class (Eq a, Show a, Encodable a) => RoundTrippable a where
  roundTrip :: a -> Expectation
  roundTrip a = run bacnetDecode (runW $ bacnetEncode a) `shouldBe` a

instance RoundTrippable Bool
instance RoundTrippable Word32
instance RoundTrippable Int32
instance RoundTrippable Float
instance RoundTrippable Double
instance RoundTrippable OctetString

instance RoundTrippable Date
instance RoundTrippable Time
instance RoundTrippable ObjectIdentifier

instance Arbitrary OctetString where
  arbitrary = OctetString <$> arbitrary
  shrink = map OctetString . shrink . getOSBytes

instance Arbitrary Date where
  arbitrary = liftM4 Date arbitrary arbitrary arbitrary arbitrary

-- This could be implemented just like Date, but again, I'm just
-- playing around with different styles.
instance Arbitrary Time where
  arbitrary = Time <$> arbitrary <*> arbitrary
                   <*> arbitrary <*> arbitrary

instance Arbitrary ObjectIdentifier where
  arbitrary = ObjectIdentifier <$> arbitrary
  shrink = map ObjectIdentifier . shrink . getRawValue


spec :: Spec
spec =
  do
    describe "Encodable Bool" $
      it "round trips properly" $
        property $ \b -> roundTrip (b :: Bool)

    describe "Encodable Word32" $
      it "round trips" $
        property $ \w -> roundTrip (w :: Word32)

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


    describe "Encodable Date" $
      it "round trips" $
        property $ \d -> roundTrip (d :: Date)

    describe "Encodable Time" $
      it "round trips" $
        property $ \t -> roundTrip (t :: Time)
{-}
    describe "Encodable ObjectIdentifier" $
      it "round trips" $
        property $ \o -> roundTrip (o :: ObjectIdentifier)-}
