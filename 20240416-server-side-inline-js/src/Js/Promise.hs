{-# language AllowAmbiguousTypes #-}
module Js.Promise where

import Js

(>>=) :: Expr (PromiseOf a) -> (Expr a -> Expr (PromiseOf b)) -> Expr (PromiseOf b)
(>>=) e f = Call1 (Prj e "then") (Lam f)

(>>) :: Expr (PromiseOf a) -> Expr (PromiseOf b) -> Expr (PromiseOf b)
(>>) a b = a Js.Promise.>>= Prelude.const b

pure :: Expr a -> Expr (Promise a)
pure = Call1 (Prj (Var $ Ident "Promise") "resolve")

lift_ :: Statement a () -> Expr (Promise ())
lift_ st =
  UnsafeNew $
  Call1
    (Var $ Ident "Promise")
    (Proc2 $ \(success, _failure) -> do
      st
      Expr $ Call0 success
    )
