module Lib (
  Cat,

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

import Data.Kind (Type)
import Data.Text (Text)
import Lib.Ty
import Prelude hiding (drop, filter, foldl, foldr, id, last, map, sum, (.))

class Cat (t :: [Ty] -> [Ty] -> Type) where
  id :: t a a
  compose :: t b c -> t a b -> t a c

  var :: Index x a -> t x '[a]
  drop :: t (a ': ctx) ctx
  bind :: ((forall x'. t x' (a ': x')) -> t x b) -> t (a ': x) b
  par :: t ctx '[a] -> t ctx b -> t ctx (a ': b)

  fix :: (t a b -> t a b) -> t a b

  fn :: t (a ': ctx) '[b] -> t ctx (TExp b a ': ctx)
  app :: t (TExp b a ': a ': ctx) (b ': ctx)

  inl :: t (a ': ctx) (TSum a b ': ctx)
  inr :: t (b ': ctx) (TSum a b ': ctx)
  matchSum :: t (a ': ctx) ctx' -> t (b ': ctx) ctx' -> t (TSum a b ': ctx) ctx'

  pair :: t (a ': b ': ctx) (TProd a b ': ctx)
  unpair :: t (TProd a b ': ctx) (a ': b ': ctx)

  true :: t ctx (TBool ': ctx)
  false :: t ctx (TBool ': ctx)
  ifte :: t ctx ctx' -> t ctx ctx' -> t (TBool ': ctx) ctx'

  char :: Char -> t ctx (TChar ': ctx)
  matchChar :: [(Char, t ctx ctx')] -> t ctx ctx' -> t (TChar ': ctx) ctx'
  eqChar :: t (TChar ': TChar ': ctx) (TBool ': ctx)

  string :: Text -> t ctx (TString ': ctx)
  uncons :: t (TString ': ctx) (TMaybe (TProd TChar TString) ': ctx)
  consString :: t (TChar ': TString ': ctx) (TString ': ctx)

  int :: Int -> t ctx (TInt ': ctx)
  add :: t (TInt ': TInt ': ctx) (TInt ': ctx)
  mul :: t (TInt ': TInt ': ctx) (TInt ': ctx)

  nothing :: t ctx (TMaybe a ': ctx)
  just :: t (a ': ctx) (TMaybe a ': ctx)
  matchMaybe :: t ctx ctx' -> t (a ': ctx) ctx' -> t (TMaybe a ': ctx) ctx'

  nil :: t ctx (TList a ': ctx)
  cons :: t (a ': TList a ': ctx) (TList a ': ctx)
  matchList :: t ctx ctx' -> t (a ': TList a ': ctx) ctx' -> t (TList a ': ctx) ctx'

(.) :: (Cat t) => t b c -> t a b -> t a c
(.) = compose

infixl 5 .