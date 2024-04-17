{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
module Maybe where

import Data.Kind (Type)
import Prelude hiding (maybe)

class HasMaybe (l :: Type -> Type) where
  nothing :: l (Maybe a)
  just :: l a -> l (Maybe a)
  maybe :: l r -> (l a -> l r) -> l (Maybe a) -> l r

map :: HasMaybe l => (l a -> l b) -> l (Maybe a) -> l (Maybe b)
map f = maybe nothing (just . f)
