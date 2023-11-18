{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}

module Lib where

import Data.Kind (Type)

data Index :: [Type] -> Type -> Type where
  Z :: Index (a ': as) a
  S :: Index as a -> Index (b ': as) a

data Context :: (Type -> Type) -> [Type] -> Type where
  Nil :: Context f '[]
  Cons :: f a -> Context f as -> Context f (a ': as)

getIndex :: Context f ctx -> Index ctx a -> f a
getIndex (Cons a _) Z = a
getIndex (Cons _ as) (S ix) = getIndex as ix
getIndex Nil Z = undefined
getIndex Nil S{} = undefined