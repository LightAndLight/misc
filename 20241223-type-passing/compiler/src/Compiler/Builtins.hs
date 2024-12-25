{-# LANGUAGE NoFieldSelectors #-}

module Compiler.Builtins where

import Data.Text (Text)

data Builtins a = Builtins
  { trace_i32 :: a
  -- ^ @trace_i32 : i32 -> ()@
  , trace_bool :: a
  -- ^ @trace_bool : bool -> ()@
  , add_i32 :: a
  -- ^ @add_i32 : i32 -> i32 -> i32@
  }

foldMapBuiltins :: Monoid m => (Text -> a -> m) -> Builtins a -> m
foldMapBuiltins f (Builtins trace_i32 trace_bool add_i32) =
  f "trace_i32" trace_i32
    <> f "trace_bool" trace_bool
    <> f "add_i32" add_i32
