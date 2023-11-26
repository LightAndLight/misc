module Data.Set.CommutativeSemigroup (Intersect (..)) where

import Data.Semigroup.Commutative (CommutativeSemigroup (..))
import Data.Set (Set)
import qualified Data.Set

newtype Intersect a = Intersect (Set a)

instance (Ord a) => Semigroup (Intersect a) where
  Intersect a <> Intersect b = Intersect (Data.Set.intersection a b)