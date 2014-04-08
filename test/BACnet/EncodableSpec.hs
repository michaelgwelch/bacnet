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

class (Eq a, Show a, Encodable a) => RoundTrippable a where
  roundTrip :: a -> Expectation
  roundTrip a = run bacnetDecode (runW $ bacnetEncode a) `shouldBe` a

instance RoundTrippable Bool
instance RoundTrippable Word32
instance RoundTrippable Int32

instance RoundTrippable Date
instance RoundTrippable Time
instance RoundTrippable ObjectIdentifier

instance Arbitrary Date where
  arbitrary = Date <$> arbitrarySizedIntegral <*> arbitrarySizedIntegral
                   <*> arbitrarySizedIntegral <*> arbitrarySizedIntegral

instance Arbitrary Time where
  arbitrary = Time <$> arbitrarySizedIntegral <*> arbitrarySizedIntegral
                   <*> arbitrarySizedIntegral <*> arbitrarySizedIntegral

instance Arbitrary ObjectIdentifier where
  arbitrary = ObjectIdentifier <$> arbitrarySizedIntegral
  shrink = fmap ObjectIdentifier <$> shrinkIntegral . getRawValue


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
