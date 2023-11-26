{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Map.CommutativeSemigroup (Intersect (..)) where

import Data.Map (Map)
import qualified Data.Map
import Data.Monoid.Commutative (CommutativeMonoid (..))
import Data.Semigroup.Commutative (CommutativeSemigroup (..))

newtype Intersect k v = Intersect (Map k v)

instance (Ord k, Semigroup v) => Semigroup (Intersect k v) where
  Intersect a <> Intersect b = Intersect (Data.Map.intersectionWith (<>) a b)

instance (Ord k, CommutativeSemigroup v) => CommutativeSemigroup (Intersect k v)