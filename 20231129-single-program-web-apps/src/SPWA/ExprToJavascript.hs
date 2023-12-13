{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module SPWA.ExprToJavascript (exprToJavascript) where

import qualified Compiler.Plugin.Interface as Expr
import Control.Monad.State.Class (MonadState)
import Control.Monad.Trans (lift)
import Control.Monad.Writer.Strict (WriterT, runWriterT)
import Data.Foldable (fold)
import Data.Functor.Const (Const (..))
import Data.Monoid (Any (..))
import SPWA.Js (Js (..))
import qualified SPWA.Js as Js
import SPWA.Supply (HasSupply, freshId)

exprToJavascript :: (MonadState s m, HasSupply s) => Expr.Ctx (Const String) ctx -> Expr.Expr ctx a -> m (Js, String)
exprToJavascript = go id
 where
  go :: (MonadState s m, HasSupply s) => (forall x. Expr.Index ctx' x -> Expr.Index ctx x) -> Expr.Ctx (Const String) ctx -> Expr.Expr ctx' a -> m (Js, String)
  go weaken ctx expr =
    case expr of
      Expr.Var v -> do
        let Const v' = Expr.getCtx (weaken v) ctx
        pure (mempty, v')
      Expr.Lam (body :: Expr.Expr (a ': ctx) b) -> do
        arg <- ("arg_" <>) <$> freshId
        (ls, body') <- go (\case Expr.Z -> Expr.Z; Expr.S ix -> Expr.S (weaken ix)) (Expr.Cons (Const arg :: Const String a) ctx) body
        temp <- ("temp_" <>) <$> freshId
        pure
          ( Js ["const " <> temp <> " = (" <> arg <> ") => {"]
              <> Js.indent 2 (ls <> Js ["return " <> body' <> ";"])
              <> Js ["};"]
          , temp
          )
      Expr.App f x -> do
        (ls, f') <- go weaken ctx f
        (ls', x') <- go weaken ctx x
        pure (ls <> ls', f' <> "(" <> x' <> ")")
      Expr.Int i -> pure (mempty, show i)
      Expr.Add a b -> do
        (ls, a') <- go weaken ctx a
        (ls', b') <- go weaken ctx b
        pure (ls <> ls', "(" <> a' <> " + " <> b' <> ")")
      Expr.Bool b ->
        if b then pure (mempty, "true") else pure (mempty, "false")
      Expr.IfThenElse cond t e -> do
        (ls, cond') <- go weaken ctx cond
        (ls', t') <- go weaken ctx t
        (ls'', e') <- go weaken ctx e
        pure (ls <> ls' <> ls'', "(" <> cond' <> " ? " <> t' <> " : " <> e' <> ")")
      Expr.Lt a b -> do
        (ls, a') <- go weaken ctx a
        (ls', b') <- go weaken ctx b
        pure (ls <> ls', "(" <> a' <> " < " <> b' <> ")")
      Expr.Case a branches -> do
        value <- ("value_" <>) <$> freshId
        (ls, a') <- go weaken ctx a
        result <- ("result_" <>) <$> freshId
        (branches', Any tagged) <- runWriterT $ traverse (branchToJavascript value result weaken ctx) branches
        pure
          ( ls
              <> Js ["const " <> value <> " = " <> a' <> ";"]
              <> Js ["var " <> result <> ";"]
              <> Js ["switch (" <> value <> (if tagged then ".tag" else "") <> ") {"]
              <> Js.indent 2 (fold branches')
              <> Js ["}"]
          , result
          )
       where
        branchToJavascript ::
          (MonadState s m, HasSupply s) =>
          String ->
          String ->
          (forall x. Expr.Index ctx' x -> Expr.Index ctx x) ->
          Expr.Ctx (Const String) ctx ->
          Expr.Branch ctx' a b ->
          WriterT Any m Js
        branchToJavascript value result weaken ctx (Expr.Branch pattern body) =
          case pattern of
            Expr.PDefault -> do
              (ls, body') <- lift $ go weaken ctx body
              pure
                $ Js ["default:"]
                <> Js.indent
                  2
                  ( ls
                      <> Js
                        [ result <> " = " <> body' <> ";"
                        , "break;"
                        ]
                  )
            Expr.PInt i -> do
              (ls, body') <- lift $ go weaken ctx body
              pure
                $ Js ["case " <> show i <> ":"]
                <> Js.indent
                  2
                  ( ls
                      <> Js
                        [ result <> " = " <> body' <> ";"
                        , "break;"
                        ]
                  )
            Expr.PUnit -> do
              (ls, body') <- lift $ go weaken ctx body
              pure
                $ Js ["default:"]
                <> Js.indent
                  2
                  ( ls
                      <> Js
                        [ result <> " = " <> body' <> ";"
                        , "break;"
                        ]
                  )
            Expr.PPair @ctx @a @b -> do
              (ls, body') <-
                lift
                  $ go
                    ( \case
                        Expr.Z -> Expr.Z
                        Expr.S n ->
                          Expr.S $ case n of
                            Expr.Z -> Expr.Z
                            Expr.S n' -> Expr.S (weaken n')
                    )
                    (Expr.Cons (Const (value <> ".snd") :: Const String b) $ Expr.Cons (Const (value <> ".fst") :: Const String a) ctx)
                    body
              pure
                $ Js ["default:"]
                <> Js.indent
                  2
                  ( ls
                      <> Js
                        [ "" <> result <> " = " <> body' <> ";"
                        , "break;"
                        ]
                  )
      Expr.Char c -> do
        pure (mempty, show c)
      Expr.ToString -> do
        pure (mempty, "JSON.stringify")
      Expr.Weaken x -> go (weaken . Expr.S) ctx x