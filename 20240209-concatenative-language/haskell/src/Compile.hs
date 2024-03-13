{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Compile (
  Compile,
  compile,
  module Control.Monad.Gen,
) where

import Control.Monad ((>=>))
import Control.Monad.Gen
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Writer.CPS (WriterT, execWriterT, tell)
import qualified Data.ByteString as ByteString
import Data.Char (ord)
import qualified Data.Text.Encoding as Text.Encoding
import qualified Data.Text.Lazy as Lazy
import Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as Builder
import Lib
import Lib.Ty
import Numeric (showHex)

newtype Compile m (ctx :: Ctx) (ctx' :: Ctx) = Compile (SCtx ctx -> WriterT Builder m (SCtx ctx'))

-- | Compile a 'Cat' expression to Intel x86-64 assembly.
compile :: (MonadGen Int m) => Compile m Nil ctx' -> m Lazy.Text
compile (Compile f) = Builder.toLazyText <$> execWriterT (f SNil)

emit :: (Monad m) => Builder -> WriterT Builder m ()
emit x = do
  tell x
  tell "\n"

shift :: (Monad m) => SCtx ctx -> WriterT Builder m (SCtx ctx)
shift ctx =
  case ctx of
    SNil ->
      pure ctx
    SSnoc{} -> do
      emit "pop rbx"
      pure ctx

pop :: (Monad m) => SCtx (ctx :. a) -> WriterT Builder m (SCtx ctx)
pop (SSnoc ctx _a) = do
  emit "mov rax, rbx"
  shift ctx

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

push' :: forall m a ctx. (Monad m) => Builder -> STy a -> SCtx ctx -> WriterT Builder m (SCtx (ctx :. a))
push' val ty ctx = do
  let
    m :: WriterT Builder m ()
    m =
      case ctx of
        SNil ->
          pure ()
        SSnoc ctx' _a -> do
          let
            m' :: WriterT Builder m ()
            m' = case ctx' of
              SNil -> pure ()
              SSnoc{} -> emit "push rbx"
          m'
          emit "mov rbx, rax"
  m
  emit $ "mov rax, " <> val
  pure $ SSnoc ctx ty

push :: (Monad m) => Literal a -> SCtx ctx -> WriterT Builder m (SCtx (ctx :. a))
push lit = push' (printLiteral lit) (litTy lit)

genLabel :: (MonadGen Int m) => Builder -> m Builder
genLabel prefix = do
  n <- gen
  pure $ prefix <> "_" <> Builder.fromString (show n)

instance (MonadGen e m) => MonadGen e (WriterT w m) where
  gen = lift gen

instance (MonadGen Int m) => Cat (Compile m) where
  id =
    Compile pure

  compose (Compile f) (Compile g) =
    Compile (f >=> g)

  drop =
    Compile pop

  var ix = error "TODO: var"

  fix f = error "TODO: fix"

  bind f = error "TODO: bind"

  fn f = error "TODO: fn"

  app = error "TODO: app"

  inl = error "TODO: inl"

  inr = error "TODO: inr"

  matchSum l r = error "TODO: matchSum"

  pair = error "TODO: pair"

  unpair = error "TODO: unpair"

  par f g = error "TODO: par"

  true = Compile $ push (LBool True)

  false = Compile $ push (LBool False)

  ifte (Compile f) (Compile g) =
    Compile $ \ctx -> do
      thenLabel <- genLabel "then"
      elseLabel <- genLabel "else"
      afterLabel <- genLabel "after"

      emit "cmp rax, 0"
      ctx' <- pop ctx
      emit $ "je " <> elseLabel
      emit $ thenLabel <> ":"
      _ <- f ctx'
      emit $ "jmp " <> afterLabel
      emit $ elseLabel <> ":"
      ctx'' <- g ctx'
      emit $ afterLabel <> ":"
      pure ctx''

  char c =
    Compile $ push (LChar c)

  eqChar =
    Compile $ \(SSnoc (SSnoc ctx STChar) STChar) -> do
      emit "cmp rax, rbx"
      emit "adc rax, 0"
      shift $ SSnoc ctx STBool

  matchChar fs g = error "TODO: matchChar"

  string s =
    Compile $ \ctx -> do
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
    Compile $ push (LInt i)

  add =
    Compile $ \(SSnoc (SSnoc ctx STInt) STInt) -> do
      emit "add rax, rbx"
      shift $ SSnoc ctx STInt

  mul =
    Compile $ \(SSnoc (SSnoc ctx STInt) STInt) -> do
      emit "mul rax, rbx"
      shift $ SSnoc ctx STInt

  nothing = error "TODO: nothing"

  just = error "TODO: just"

  matchMaybe f g = error "TODO: matchMaybe"

  nil = error "TODO: nil"

  cons = error "TODO: cons"

  matchList f g = error "TODO: matchList"
