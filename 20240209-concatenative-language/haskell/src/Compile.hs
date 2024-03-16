{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Compile (
  Compile,
  compile,
  module Control.Monad.Gen,
) where

import Control.Monad.Fix (MonadFix)
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

data Compile m (ctx :: [Ty]) (ctx' :: [Ty]) = Compile (SList ctx) (SList ctx') ([Register] -> WriterT Builder m ())

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
compile :: (MonadGen Int m) => Compile m '[] '[a] -> m Lazy.Text
compile (Compile ctx ctx' f) = Builder.toLazyText <$> execWriterT (f initialTemporaries)

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

instance (MonadGen Int m, MonadFix m) => Cat (Compile m) where
  type CtxC (Compile m) = KnownCtx
  type TyC (Compile m) = KnownTy
  id =
    Compile input input (const $ pure ())
   where
    input = ctxVal

  compose (Compile _ ctx' f) (Compile ctx _ g) =
    Compile ctx ctx' (\tmps -> g tmps *> f tmps)

  drop =
    Compile input output $ \_tmps -> do
      _ <- pop input
      pure ()
   where
    input = ctxVal
    output = ctxVal

  var ix = error "TODO: var"

  fix f = Compile input output $ \tmps -> do
    loopLabel <- genLabel "loop"
    emit $ loopLabel <> ":"
    rec let Compile _input _output f' =
              f
                ( Compile input output $ \_tmps -> do
                    emit $ "jmp " <> loopLabel
                )
    f' tmps
   where
    input = ctxVal
    output = ctxVal

  bind f = Compile input output $ \tmps -> do
    let SCons ty ctx = input

    (tmp, tmps') <-
      case tmps of
        [] ->
          error "TODO: not enough tmps"
        tmp : tmps' -> do
          emit $ "mov " <> printRegister tmp <> ", rax"
          shiftl0 ctx
          pure (tmp, tmps')
    let Compile _input _output f' =
          f
            ( let input' = ctxVal
              in Compile input' (SCons ty input') $ \_y -> do
                  _ <- push' (printRegister tmp) ty input'
                  pure ()
            )
    ctx' <- f' tmps'
    emit $ "; drop " <> printRegister tmp
    pure ctx'
   where
    input = ctxVal
    output = ctxVal

  fn f = error "TODO: fn"

  app = error "TODO: app"

  inl = error "TODO: inl"

  inr = error "TODO: inr"

  matchSum l r = error "TODO: matchSum"

  pair = Compile input output $ \_tmps -> pure ()
   where
    input = ctxVal
    output = ctxVal

  unpair = Compile input output $ \_tmps -> pure ()
   where
    input = ctxVal
    output = ctxVal

  par f g = error "TODO: par"

  true = Compile input output $ \_tmps -> do
    _ <- push (LBool True) input
    pure ()
   where
    input = ctxVal
    output = ctxVal

  false = Compile input output $ \_tmps -> do
    _ <- push (LBool False) input
    pure ()
   where
    input = ctxVal
    output = ctxVal

  ifte (Compile _input1 _output1 f) (Compile _input2 _output2 g) =
    Compile input3 output3 $ \tmps -> do
      thenLabel <- genLabel "then"
      elseLabel <- genLabel "else"
      afterLabel <- genLabel "after"

      emit "cmp rax, 0"
      _ <- pop input3
      emit $ "je " <> elseLabel
      emit $ thenLabel <> ":"
      f tmps
      emit $ "jmp " <> afterLabel
      emit $ elseLabel <> ":"
      g tmps
      emit $ afterLabel <> ":"
   where
    input3 = ctxVal
    output3 = ctxVal

  char c =
    Compile input output $ \_tmps -> do
      _ <- push (LChar c) input
      pure ()
   where
    input = ctxVal
    output = ctxVal

  eqChar =
    Compile input output $ \_tmps -> do
      let SCons STChar (SCons STChar ctx) = input
      emit "cmp rax, rbx"
      emit "adc rax, 0"
      shiftl0 ctx
   where
    input = ctxVal
    output = ctxVal

  matchChar [] g = g `compose` drop
  matchChar ((c, f) : fs) g = bind $ \c' -> ifte f (matchChar fs g `compose` c') `compose` eqChar `compose` char c `compose` c'

  string s =
    Compile input output $ \_tmps -> do
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
      _ <- push' stringLabel STString input
      pure ()
   where
    input = ctxVal
    output = ctxVal

    sepBy [] _ = mempty
    sepBy [x] _ = x
    sepBy (x : xs) sep = x <> sep <> sepBy xs sep

  uncons = error "TODO: uncons"

  consString = error "TODO: consString"

  int i =
    Compile input output $ \_tmps -> do
      _ <- push (LInt i) input
      pure ()
   where
    input = ctxVal
    output = ctxVal

  add =
    Compile input output $ \_tmps -> do
      let SCons STInt (SCons STInt ctx) = input
      emit "add rax, rbx"
      shiftl0 ctx
   where
    input = ctxVal
    output = ctxVal

  mul =
    Compile input output $ \_tmps -> do
      let SCons STInt (SCons STInt ctx) = input
      emit "mul rax, rbx"
      shiftl0 ctx
   where
    input = ctxVal
    output = ctxVal

  nothing = error "TODO: nothing"

  just = error "TODO: just"

  matchMaybe f g = error "TODO: matchMaybe"

  nil = error "TODO: nil"

  cons = error "TODO: cons"

  matchList f g = error "TODO: matchList"