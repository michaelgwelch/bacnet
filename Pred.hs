{- See https://www.fpcomplete.com/school/to-infinity-and-beyond/pick-of-the-week/profunctors -}
module Pred where

import qualified Data.Functor.Contravariant as C
import qualified Data.Functor as F

newtype Predicate a = Predicate { getPredicate :: a -> Bool }

pContramap :: (b -> a) -> Predicate a -> Predicate b
pContramap f (Predicate pa) = Predicate (pa . f)

instance C.Contravariant Predicate where
  contramap = pContramap

newtype Comparison a = Comparison { getComparison :: a -> a -> Ordering }

compContramap :: (b -> a) -> Comparison a -> Comparison b
compContramap f (Comparison comp) =
  Comparison (\b1 -> comp (f b1) . f)

instance C.Contravariant Comparison where
  contramap = compContramap

defaultComparison :: Ord a => Comparison a
defaultComparison = Comparison compare


class Bifunctor f where
  bimap :: (a -> c) -> (b -> d) -> f a b -> f c d


instance Bifunctor Either where
  bimap fa fb (Left a) = Left $ fa a
  bimap fa fb (Right b) = Right $ fb b

instance Bifunctor (,) where
  bimap fa fb (a, b) = (fa a, fb b)

class Profunctor f where
  dimap :: (c -> a) -> (b -> d) -> f a b -> f c d

instance Profunctor (->) where
  dimap fc fb fa = fb . fa . fc

newtype Indexed i a b = Indexed { runIndexed :: i -> a -> b }
