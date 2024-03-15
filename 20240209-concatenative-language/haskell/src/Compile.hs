{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Compile (
  Compile,
  compile,
  module Control.Monad.Gen,
) where

import Control.Monad ((<=<))
import Control.Monad.Gen
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Writer.CPS (WriterT, execWriterT, tell)
import qualified Data.ByteString as ByteString
import Data.Char (ord)
import qualified Data.Text.Encoding as Text.Encoding
import qualified Data.Text.Lazy as Lazy
import Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as Builder
import Lib hiding ((.))
import Lib.Ty
import Numeric (showHex)
import Prelude hiding (drop)

newtype Compile m (ctx :: [Ty]) (ctx' :: [Ty]) = Compile ((SList ctx, [Register]) -> WriterT Builder m (SList ctx'))

data Register = Rax | Rbx | Rcx | Rdx | Rsi | Rdi | R8 | R9 | R10 | R11 | R12 | R13 | R14 | R15

printRegister :: Register -> Builder
printRegister r =
  case r of
    Rax -> "rax"
    Rbx -> "rbx"
    Rcx -> "rcx"
    Rdx -> "rdx"
    Rsi -> "rsi"
    Rdi -> "rdi"
    R8 -> "r8"
    R9 -> "r9"
    R10 -> "r10"
    R11 -> "r11"
    R12 -> "r12"
    R13 -> "r13"
    R14 -> "r14"
    R15 -> "r15"

initialTemporaries :: [Register]
initialTemporaries = [Rcx, Rdx, Rsi, Rdi, R8, R9, R10, R11, R12, R13, R14, R15]

-- | Compile a 'Cat' expression to Intel x86-64 assembly.
compile :: (MonadGen Int m) => Compile m '[] ctx' -> m Lazy.Text
compile (Compile f) = Builder.toLazyText <$> execWriterT (f (SNil, initialTemporaries))

emit :: (Monad m) => Builder -> WriterT Builder m ()
emit x = do
  tell x
  tell "\n"

{- @rbx@ is empty and the context lives in the stack.

Shift the entire context "to the left" so that it starts in @rbx@.
-}
shiftl1 :: (Monad m) => SList ctx -> WriterT Builder m ()
shiftl1 ctx =
  case ctx of
    SNil ->
      pure ()
    SCons ty ctx' ->
      case ty of
        STProd a b ->
          shiftl1 (SCons a (SCons b ctx'))
        _ ->
          emit "pop rbx"

{- @rax@ is empty and the context lives in @rbx@ onwards.

Shift the entire context "to the left" so that it starts in @rax@.
-}
shiftl0 :: (Monad m) => SList ctx -> WriterT Builder m ()
shiftl0 ctx =
  case ctx of
    SNil ->
      pure ()
    SCons ty ctx' ->
      case ty of
        STProd a b ->
          shiftl0 (SCons a (SCons b ctx'))
        _ -> do
          emit "mov rax, rbx"
          shiftl1 ctx'

pop :: (Monad m) => SList (a ': ctx) -> WriterT Builder m (SList ctx)
pop (SCons ty ctx) = do
  case ty of
    STProd a b -> do
      pop =<< pop (SCons a (SCons b ctx))
    _ -> do
      emit "; drop rax"
      case ctx of
        SNil ->
          pure ctx
        SCons{} -> do
          shiftl0 ctx
          pure ctx

data Literal (ty :: Ty) where
  LInt :: Int -> Literal TInt
  LBool :: Bool -> Literal TBool
  LChar :: Char -> Literal TChar

litTy :: Literal ty -> STy ty
litTy lit =
  case lit of
    LInt{} -> STInt
    LBool{} -> STBool
    LChar{} -> STChar

printLiteral :: Literal ty -> Builder
printLiteral lit =
  case lit of
    LInt n -> Builder.fromString $ show n
    LBool b -> if b then "1" else "0"
    LChar c -> Builder.fromString Prelude.. show $ ord c

-- The context starts at @rbx@. Move everything one slot "to the right", freeing @rbx@.
shiftr1 :: (Monad m) => SList (a ': ctx) -> WriterT Builder m ()
shiftr1 (SCons ty ctx) =
  case ty of
    STProd a b ->
      shiftr1 (SCons a (SCons b ctx))
    _ ->
      emit "push rbx"

-- The context starts at @rax@. Move everything one slot "to the right", freeing @rax@.
shiftr0 :: (Monad m) => SList (a ': ctx) -> WriterT Builder m ()
shiftr0 (SCons ty ctx) =
  case ty of
    STProd a b ->
      shiftr0 (SCons a (SCons b ctx))
    _ ->
      case ctx of
        SNil ->
          emit "mov rbx, rax"
        SCons{} -> do
          shiftr1 ctx
          emit "mov rbx, rax"

push' :: forall m a ctx. (Monad m) => Builder -> STy a -> SList ctx -> WriterT Builder m (SList (a ': ctx))
push' val ty ctx =
  case ty of
    STProd a b ->
      error "TODO: push' for products"
    _ ->
      case ctx of
        SNil -> do
          emit $ "mov rax, " <> val
          pure $ SCons ty SNil
        SCons{} -> do
          shiftr0 ctx
          emit $ "mov rax, " <> val
          pure $ SCons ty ctx

push :: (Monad m) => Literal a -> SList ctx -> WriterT Builder m (SList (a ': ctx))
push lit = push' (printLiteral lit) (litTy lit)

genLabel :: (MonadGen Int m) => Builder -> m Builder
genLabel prefix = do
  n <- gen
  pure $ prefix <> "_" <> Builder.fromString (show n)

instance (MonadGen e m) => MonadGen e (WriterT w m) where
  gen = lift gen

instance (MonadGen Int m) => Cat (Compile m) where
  id =
    Compile (pure . fst)

  compose (Compile f) (Compile g) =
    Compile (\(ctx, tmps) -> f . (,tmps) =<< g (ctx, tmps))

  drop =
    Compile $ \(ctx, _tmps) -> pop ctx

  var ix = error "TODO: var"

  fix f = error "TODO: fix"

  bind f = Compile $ \(SCons ty ctx, tmps) -> do
    (tmp, tmps') <-
      case tmps of
        [] ->
          error "TODO: not enough tmps"
        tmp : tmps' -> do
          emit $ "mov " <> printRegister tmp <> ", rax"
          shiftl0 ctx
          pure (tmp, tmps')
    let Compile f' =
          f
            ( Compile $ \(x, _y) -> do
                push' (printRegister tmp) ty x
            )
    ctx' <- f' (ctx, tmps')
    emit $ "; drop " <> printRegister tmp
    pure ctx'

  fn f = error "TODO: fn"

  app = error "TODO: app"

  inl = error "TODO: inl"

  inr = error "TODO: inr"

  matchSum l r = error "TODO: matchSum"

  pair = Compile $ \(SCons a (SCons b ctx), _tmps) ->
    pure $ SCons (STProd a b) ctx

  unpair = Compile $ \(SCons (STProd a b) ctx, _tmps) ->
    pure $ SCons a (SCons b ctx)

  par f g = error "TODO: par"

  true = Compile $ \(ctx, _tmps) -> push (LBool True) ctx

  false = Compile $ \(ctx, _tmps) -> push (LBool False) ctx

  ifte (Compile f) (Compile g) =
    Compile $ \(ctx, tmps) -> do
      thenLabel <- genLabel "then"
      elseLabel <- genLabel "else"
      afterLabel <- genLabel "after"

      emit "cmp rax, 0"
      ctx' <- pop ctx
      emit $ "je " <> elseLabel
      emit $ thenLabel <> ":"
      _ <- f (ctx', tmps)
      emit $ "jmp " <> afterLabel
      emit $ elseLabel <> ":"
      ctx'' <- g (ctx', tmps)
      emit $ afterLabel <> ":"
      pure ctx''

  char c =
    Compile $ \(ctx, _tmps) -> push (LChar c) ctx

  eqChar =
    Compile $ \(SCons STChar (SCons STChar ctx), _tmps) -> do
      emit "cmp rax, rbx"
      emit "adc rax, 0"
      shiftl0 ctx
      pure $ SCons STBool ctx

  matchChar [] g = g `compose` drop
  matchChar ((c, f) : fs) g = bind $ \c' -> ifte f (matchChar fs g `compose` c') `compose` eqChar `compose` char c `compose` c'

  string s =
    Compile $ \(ctx, tmps) -> do
      stringLabel <- genLabel "string"
      emit $
        stringLabel
          <> ": .byte "
          <> sepBy
            ( fmap
                (Builder.fromString Prelude.. ("0x" <>) Prelude.. ($ "") Prelude.. showHex)
                (ByteString.unpack $ Text.Encoding.encodeUtf8 s)
            )
            ", "
      push' stringLabel STString ctx
   where
    sepBy [] _ = mempty
    sepBy [x] _ = x
    sepBy (x : xs) sep = x <> sep <> sepBy xs sep

  uncons = error "TODO: uncons"

  consString = error "TODO: consString"

  int i =
    Compile $ \(ctx, _tmps) -> push (LInt i) ctx

  add =
    Compile $ \(SCons STInt (SCons STInt ctx), _tmps) -> do
      emit "add rax, rbx"
      shiftl0 ctx
      pure $ SCons STInt ctx

  mul =
    Compile $ \(SCons STInt (SCons STInt ctx), _tmps) -> do
      emit "mul rax, rbx"
      shiftl0 ctx
      pure $ SCons STInt ctx

  nothing = error "TODO: nothing"

  just = error "TODO: just"

  matchMaybe f g = error "TODO: matchMaybe"

  nil = error "TODO: nil"

  cons = error "TODO: cons"

  matchList f g = error "TODO: matchList"