{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Compiler.Plugin.Interface (
  Quoted (..),
  Expr (..),
  Sum (..),
  Product (..),
  Index (..),
  Ctx (..),
  Branch (..),
  Pattern (..),
  getCtx,
  quote,
  compose,
  toString,
  append,
  isEmpty,
  Quote' (..),
) where

import Data.Kind (Type)
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Exts (Coercible, Symbol, noinline)
import GHC.Stack (HasCallStack)
import GHC.TypeLits (KnownSymbol)

data Expr :: [Type] -> Type -> Type where
  Var :: forall ctx a. Index ctx a -> Expr ctx a
  Lam :: forall ctx a b. Expr (a ': ctx) b -> Expr ctx (a -> b)
  App :: forall ctx a b. Expr ctx (a -> b) -> Expr ctx a -> Expr ctx b
  Add :: forall ctx. Expr ctx Int -> Expr ctx Int -> Expr ctx Int
  IfThenElse :: forall ctx a. Expr ctx Bool -> Expr ctx a -> Expr ctx a -> Expr ctx a
  Lt :: forall ctx. Expr ctx Int -> Expr ctx Int -> Expr ctx Bool
  Case :: forall ctx a b. Expr ctx a -> [Branch ctx a b] -> Expr ctx b
  Int :: forall ctx. Int -> Expr ctx Int
  Bool :: forall ctx. Bool -> Expr ctx Bool
  Char :: forall ctx. Char -> Expr ctx Char
  String :: forall ctx. Text -> Expr ctx Text
  List :: forall ctx a. [Expr ctx a] -> Expr ctx [a]
  Weaken :: Expr ctx a -> Expr (b ': ctx) a
  ToString :: forall ctx a. (Show a) => Expr ctx (a -> String)
  Append :: forall ctx a. Expr ctx ([a] -> [a] -> [a])
  IsEmpty :: forall ctx a. Expr ctx ([a] -> Bool)
  Sum :: Sum ctors -> Expr ctx (Sum ctors)
  Product :: Product fields -> Expr ctx (Product fields)
  Coerced :: (Coercible a b, Show a) => a -> Expr ctx b

deriving instance Show (Expr ctx a)

data Branch :: [Type] -> Type -> Type -> Type where
  Branch :: forall ctx ctx' a b. Pattern ctx a ctx' -> Expr ctx' b -> Branch ctx a b

deriving instance Show (Branch ctx a b)

data Pattern :: [Type] -> Type -> [Type] -> Type where
  PDefault :: forall ctx a. Pattern ctx a ctx
  PInt :: forall ctx. Int -> Pattern ctx Int ctx
  PPair :: forall ctx a b. Pattern ctx (a, b) (b ': a ': ctx)
  PUnit :: forall ctx. Pattern ctx () ctx

deriving instance Show (Pattern ctx a ctx')

data Sum :: [(Symbol, Type)] -> Type where
  Sum_Z :: (KnownSymbol ctor) => Expr ctx a -> Sum ('(ctor, a) ': ctors)
  Sum_S :: (KnownSymbol ctor) => Sum ctors -> Sum ('(ctor, a) ': ctors)

deriving instance Show (Sum ctors)

data Product :: [(Symbol, Type)] -> Type where
  Product_Nil :: Product '[]
  Product_Cons :: (KnownSymbol field) => Expr ctx a -> Product fields -> Product ('(field, a) ': fields)

deriving instance Show (Product fields)

data Index :: [Type] -> Type -> Type where
  Z :: forall ctx a. Index (a ': ctx) a
  S :: forall ctx a b. Index ctx a -> Index (b ': ctx) a

deriving instance Show (Index ctx a)

data Ctx :: (Type -> Type) -> [Type] -> Type where
  Nil :: Ctx f '[]
  Cons :: f a -> Ctx f as -> Ctx f (a ': as)

getCtx :: Index ctx a -> Ctx f ctx -> f a
getCtx Z (Cons a _) = a
getCtx (S ix) (Cons _ as) = getCtx ix as

data Quoted a = Quoted {quotedCode :: Expr '[] a, quotedValue :: a}

instance Show (Quoted a) where
  showsPrec d (Quoted code _) =
    showParen (d > appPrec)
      $ showString "Quoted "
      . showsPrec (appPrec + 1) code
      . showString " <<function>>"
   where
    appPrec = 10

{-# NOINLINE quote #-}
quote :: (HasCallStack) => a -> Quoted a
quote a = noinline error "quote not removed by compiler plugin" a

compose :: Quoted (b -> c) -> Quoted (a -> b) -> Quoted (a -> c)
compose f g = Quoted (Lam $ App (Weaken $ quotedCode f) $ App (Weaken $ quotedCode g) (Var Z)) (quotedValue f . quotedValue g)

{-# NOINLINE toString #-}
toString :: (Show a) => a -> Text
toString = Text.pack . show

{-# NOINLINE append #-}
append :: [a] -> [a] -> [a]
append = (++)

{-# NOINLINE isEmpty #-}
isEmpty :: [a] -> Bool
isEmpty = null

class Quote' a where
  type QuoteTy' a :: Type
  quote' :: a -> Expr '[] (QuoteTy' a)