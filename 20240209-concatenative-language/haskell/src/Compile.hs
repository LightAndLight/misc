module Compile (Compile, compile) where

import Control.Monad ((>=>))
import Control.Monad.Trans.Writer.CPS (Writer, execWriter, tell)
import qualified Data.ByteString as ByteString
import Data.Char (ord)
import qualified Data.Text.Encoding as Text.Encoding
import qualified Data.Text.Lazy as Lazy
import Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as Builder
import Lib
import Lib.Ty
import Numeric (showHex)

newtype Compile (ctx :: Ctx) (ctx' :: Ctx) = Compile (SCtx ctx -> Writer Builder (SCtx ctx'))

-- | Compile a 'Cat' expression to Intel x86-64 assembly.
compile :: Compile Nil ctx' -> Lazy.Text
compile (Compile f) = Builder.toLazyText $ execWriter (f SNil)

emit :: Builder -> Writer Builder ()
emit x = do
  tell x
  tell "\n"

shift :: SCtx ctx -> Writer Builder (SCtx ctx)
shift ctx =
  case ctx of
    SNil ->
      pure ctx
    SSnoc{} -> do
      emit "pop rbx"
      pure ctx

pop :: SCtx (ctx :. a) -> Writer Builder (SCtx ctx)
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

push' :: Builder -> STy a -> SCtx ctx -> Writer Builder (SCtx (ctx :. a))
push' val ty ctx = do
  let
    m :: Writer Builder ()
    m =
      case ctx of
        SNil ->
          pure ()
        SSnoc ctx' _a -> do
          let
            m' :: Writer Builder ()
            m' = case ctx' of
              SNil -> pure ()
              SSnoc{} -> emit "push rbx"
          m'
          emit "mov rbx, rax"
  m
  emit $ "mov rax, " <> val
  pure $ SSnoc ctx ty

push :: Literal a -> SCtx ctx -> Writer Builder (SCtx (ctx :. a))
push lit = push' (printLiteral lit) (litTy lit)

instance Cat Compile where
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
      emit "cmp rax, 0"
      ctx' <- pop ctx
      emit "je else"
      emit "then:"
      _ <- f ctx'
      emit "jmp after"
      emit "else:"
      ctx'' <- g ctx'
      emit "after:"
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
      emit $
        "string: .byte "
          <> sepBy
            ( fmap
                (Builder.fromString Prelude.. ("0x" <>) Prelude.. ($ "") Prelude.. showHex)
                (ByteString.unpack $ Text.Encoding.encodeUtf8 s)
            )
            ", "
      push' "string" STString ctx
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
