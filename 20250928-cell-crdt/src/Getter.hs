{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}

module Getter where

import Data.Kind (Type)

newtype Getter (b :: (Type -> Type) -> Type) a = Getter (forall f. b f -> f a)
