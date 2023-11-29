{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

module Compiler.Plugin.Interface (Quoted (..), Expr (..), Index (..), quote) where

import Data.Kind (Type)
import GHC.Stack (HasCallStack)

data Expr :: [Type] -> Type -> Type where
  Var :: Index ctx a -> Expr ctx a
  Lam :: Expr (a ': ctx) b -> Expr ctx (a -> b)
  App :: Expr ctx (a -> b) -> Expr ctx a -> Expr ctx b
  Int :: Int -> Expr ctx Int

deriving instance Show (Expr ctx a)

data Index :: [Type] -> Type -> Type where
  Z :: Index (a ': as) a
  S :: Index as a -> Index (b ': as) a

deriving instance Show (Index ctx a)

data Quoted a = Quoted (Expr '[] a) a

instance Show (Quoted a) where
  show (Quoted code _) = show code

quote :: (HasCallStack) => a -> Quoted a
quote = error "quote not removed by compiler plugin"