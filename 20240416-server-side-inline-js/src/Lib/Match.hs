{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Lib.Match
  ( HasMatch(..)
  , PMaybe(..)
  )
where

import Data.Kind (Type)
  
type family Pattern (a :: Type) :: (Type -> Type) -> Type

class HasMatch (l :: Type -> Type) (a :: Type) where
  match :: l a -> (Pattern a l -> l b) -> l b

type instance Pattern (Maybe a) = PMaybe a

data PMaybe a (l :: Type -> Type)
  = PNothing
  | PJust (l a)
