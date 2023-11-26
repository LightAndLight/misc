module Data.Semigroup.Commutative (CommutativeSemigroup (..)) where

import Data.Semigroup (Product, Sum)

class (Semigroup m) => CommutativeSemigroup m

instance (Num a) => CommutativeSemigroup (Sum a)
instance (Num a) => CommutativeSemigroup (Product a)