{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Lib (
  Cat,
  CtxC,
  HasCtxC,
  TyC,
  HasTyC,

  -- * Composition
  id,
  compose,
  (.),

  -- * Context manipulation
  var,
  par,
  drop,
  bind,

  -- * Recursion
  fix,

  -- * Products
  pair,
  unpair,

  -- * Sums
  inl,
  inr,
  matchSum,

  -- * Functions
  fn,
  app,

  -- * Booleans
  true,
  false,
  ifte,

  -- * Unicode characters
  char,
  matchChar,
  eqChar,

  -- * Strings
  string,
  uncons,
  consString,

  -- * Integers
  int,
  add,
  mul,

  -- * Compound datatypes

  -- ** Maybe
  nothing,
  just,
  matchMaybe,

  -- ** List
  nil,
  cons,
  matchList,
) where

import Data.Kind (Constraint, Type)
import Data.Text (Text)
import Lib.Ty
import Prelude hiding (drop, filter, foldl, foldr, id, last, map, sum, (.))

class (TyC t a) => HasTyC (t :: [Ty] -> [Ty] -> Type) (a :: Ty)

instance (TyC t a) => HasTyC t a

class (CtxC t a) => HasCtxC (t :: [Ty] -> [Ty] -> Type) (a :: [Ty])

instance (CtxC t a) => HasCtxC t a

class Trivial (a :: k)

instance Trivial a

class
  ( HasCtxC t '[]
  , forall a as. (TyC t a, CtxC t as) => HasCtxC t (a ': as)
  , forall a b. (TyC t a, TyC t b) => HasTyC t (TSum a b)
  , forall a b. (TyC t a, TyC t b) => HasTyC t (TProd a b)
  , forall a b. (TyC t a, TyC t b) => HasTyC t (TExp a b)
  , forall a. (TyC t a) => HasTyC t (TMaybe a)
  , forall a. (TyC t a) => HasTyC t (TList a)
  , HasTyC t TChar
  , HasTyC t TString
  , HasTyC t TBool
  , HasTyC t TInt
  ) =>
  Cat (t :: [Ty] -> [Ty] -> Type)
  where
  type CtxC t :: [Ty] -> Constraint
  type CtxC t = Trivial

  type TyC t :: Ty -> Constraint
  type TyC t = Trivial

  id :: (CtxC t a) => t a a
  compose :: (CtxC t a, CtxC t b, CtxC t c) => t b c -> t a b -> t a c

  var :: (CtxC t x, TyC t a) => Index x a -> t x '[a]
  drop :: (TyC t a, CtxC t ctx) => t (a ': ctx) ctx
  bind :: (TyC t a, CtxC t x, CtxC t b) => ((forall x'. (CtxC t x') => t x' (a ': x')) -> t x b) -> t (a ': x) b
  par :: (CtxC t ctx, TyC t a, CtxC t b) => t ctx '[a] -> t ctx b -> t ctx (a ': b)

  fix :: (CtxC t a, CtxC t b) => (t a b -> t a b) -> t a b

  fn :: t (a ': ctx) '[b] -> t ctx (TExp b a ': ctx)
  app :: t (TExp b a ': a ': ctx) (b ': ctx)

  inl :: (TyC t a, TyC t b, CtxC t ctx) => t (a ': ctx) (TSum a b ': ctx)
  inr :: (TyC t a, TyC t b, CtxC t ctx) => t (b ': ctx) (TSum a b ': ctx)
  matchSum ::
    (TyC t a, TyC t b, CtxC t ctx, CtxC t ctx') =>
    t (a ': ctx) ctx' ->
    t (b ': ctx) ctx' ->
    t (TSum a b ': ctx) ctx'

  pair :: (TyC t a, TyC t b, CtxC t ctx) => t (a ': b ': ctx) (TProd a b ': ctx)
  unpair :: (TyC t a, TyC t b, CtxC t ctx) => t (TProd a b ': ctx) (a ': b ': ctx)

  true :: (CtxC t ctx) => t ctx (TBool ': ctx)
  false :: (CtxC t ctx) => t ctx (TBool ': ctx)
  ifte :: (CtxC t ctx, CtxC t ctx') => t ctx ctx' -> t ctx ctx' -> t (TBool ': ctx) ctx'

  char :: (CtxC t ctx) => Char -> t ctx (TChar ': ctx)
  matchChar :: (CtxC t ctx, CtxC t ctx') => [(Char, t ctx ctx')] -> t ctx ctx' -> t (TChar ': ctx) ctx'
  eqChar :: (CtxC t ctx) => t (TChar ': TChar ': ctx) (TBool ': ctx)

  string :: (CtxC t ctx) => Text -> t ctx (TString ': ctx)
  uncons :: t (TString ': ctx) (TMaybe (TProd TChar TString) ': ctx)
  consString :: t (TChar ': TString ': ctx) (TString ': ctx)

  int :: (CtxC t ctx) => Int -> t ctx (TInt ': ctx)
  add :: (CtxC t ctx) => t (TInt ': TInt ': ctx) (TInt ': ctx)
  mul :: (CtxC t ctx) => t (TInt ': TInt ': ctx) (TInt ': ctx)

  nothing :: t ctx (TMaybe a ': ctx)
  just :: t (a ': ctx) (TMaybe a ': ctx)
  matchMaybe :: t ctx ctx' -> t (a ': ctx) ctx' -> t (TMaybe a ': ctx) ctx'

  nil :: t ctx (TList a ': ctx)
  cons :: t (a ': TList a ': ctx) (TList a ': ctx)
  matchList :: t ctx ctx' -> t (a ': TList a ': ctx) ctx' -> t (TList a ': ctx) ctx'

(.) :: (Cat t, CtxC t a, CtxC t b, CtxC t c) => t b c -> t a b -> t a c
(.) = compose

infixl 5 .