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
  Index (..),
  Ctx (..),
  Branch (..),
  Pattern (..),
  getCtx,
  quote,
  toString,
) where

import Data.Kind (Type)
import GHC.Exts (noinline)
import GHC.Stack (HasCallStack)

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
  ToString :: forall ctx a. (Show a) => Expr ctx (a -> String)

deriving instance Show (Expr ctx a)

data Branch :: [Type] -> Type -> Type -> Type where
  Branch :: forall ctx ctx' a b. Pattern ctx a ctx' -> Expr ctx' b -> Branch ctx a b

deriving instance Show (Branch ctx a b)

data Pattern :: [Type] -> Type -> [Type] -> Type where
  PDefault :: forall ctx a. Pattern ctx a ctx
  PInt :: forall ctx. Int -> Pattern ctx Int ctx

deriving instance Show (Pattern ctx a ctx')

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

{-# NOINLINE toString #-}
toString :: (Show a) => a -> String
toString = show
