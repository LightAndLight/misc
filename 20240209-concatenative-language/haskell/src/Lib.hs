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

class Cat (t :: Ctx -> Ctx -> Type) where
  id :: t a a
  compose :: t a b -> t b c -> t a c

  var :: Index x a -> t x (Nil :. a)
  drop :: t (ctx :. a) ctx
  bind :: ((forall x'. t x' (x' :. a)) -> t x b) -> t (x :. a) b
  par :: t ctx a -> t ctx (Nil :. b) -> t ctx (a :. b)

  fix :: (t a b -> t a b) -> t a b

  fn :: t (ctx :. a) (Nil :. b) -> t ctx (ctx :. TExp b a)
  app :: t (ctx :. TExp b a :. a) (ctx :. b)

  inl :: t (ctx :. a) (ctx :. TSum a b)
  inr :: t (ctx :. b) (ctx :. TSum a b)
  matchSum :: t (ctx :. a) ctx' -> t (ctx :. b) ctx' -> t (ctx :. TSum a b) ctx'

  pair :: t (ctx :. a :. b) (ctx :. TProd a b)
  unpair :: t (ctx :. TProd a b) (ctx :. a :. b)

  true :: t ctx (ctx :. TBool)
  false :: t ctx (ctx :. TBool)
  ifte :: t ctx ctx' -> t ctx ctx' -> t (ctx :. TBool) ctx'

  char :: Char -> t a (a :. TChar)
  matchChar :: [(Char, t ctx ctx')] -> t ctx ctx' -> t (ctx :. TChar) ctx'
  eqChar :: t (ctx :. TChar :. TChar) (ctx :. TBool)

  string :: Text -> t a (a :. TString)
  uncons :: t (ctx :. TString) (ctx :. TMaybe (TProd TString TChar))
  consString :: t (ctx :. TString :. TChar) (ctx :. TString)

  int :: Int -> t a (a :. TInt)
  add :: t (x :. TInt :. TInt) (x :. TInt)
  mul :: t (x :. TInt :. TInt) (x :. TInt)

  nothing :: t x (x :. TMaybe a)
  just :: t (x :. a) (x :. TMaybe a)
  matchMaybe :: t ctx ctx' -> t (ctx :. a) ctx' -> t (ctx :. TMaybe a) ctx'

  nil :: t x (x :. TList a)
  cons :: t (x :. TList a :. a) (x :. TList a)
  matchList :: t ctx ctx' -> t (ctx :. TList a :. a) ctx' -> t (ctx :. TList a) ctx'

(.) :: (Cat t) => t a b -> t b c -> t a c
(.) = compose

infixr 5 .