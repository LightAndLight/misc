{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}

module Compiler.Codegen.Builder
  ( -- * Builders

    -- ** Modules
    includes
  , typedef
  , externConst
  , const'
  , functionSignature
  , function
  , section

    -- ** Sections
  , line

    -- ** Blocks
  , toplevel
  , comment
  , statement
  , block
  , declare
  , define
  , assign
  , ifThenElse
  , return'

    -- ** Types
  , tyStruct

    -- ** Lvalues
  , lvalFunctionPointer

    -- ** Expressions
  , parens
  , ident
  , int
  , word
  , cast
  , call
  , struct

    -- * Types

    -- ** Modules
  , ModuleT (..)
  , runModuleT
  , hoistModuleT

    -- ** Sections
  , SectionT (..)
  , runSectionT

    -- ** Blocks
  , BlockT (..)
  , runBlockT
  , hoistBlockT
  )
where

import Compiler.Fresh (MonadFresh)
import Control.Monad.Reader.Class (ask, local)
import Control.Monad.State.Class (gets, modify)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Trans.Reader (ReaderT, mapReaderT, runReaderT)
import Control.Monad.Trans.State.Strict (StateT, evalStateT, mapStateT)
import Control.Monad.Trans.Writer.CPS (WriterT, mapWriterT, runWriterT)
import Control.Monad.Writer.Class (tell)
import Data.DList (DList)
import qualified Data.DList as DList
import Data.Foldable (traverse_)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Lazy
import Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as Builder
import qualified Data.Tuple as Tuple

newtype ModuleState = ModuleState {isEmpty :: Bool}

newtype ModuleT m a = ModuleT (StateT ModuleState (WriterT Builder m) a)
  deriving (Functor, Applicative, Monad, MonadFresh)

instance MonadTrans ModuleT where
  lift = ModuleT . lift . lift

runModuleT :: Monad m => ModuleT m a -> m (Lazy.Text, a)
runModuleT (ModuleT ma) =
  fmap (Tuple.swap . fmap Builder.toLazyText) . runWriterT $
    evalStateT ma ModuleState{isEmpty = True}

hoistModuleT :: (Monad m, Monad m') => (forall x. m x -> m' x) -> ModuleT m a -> ModuleT m' a
hoistModuleT f (ModuleT ma) = ModuleT $ mapStateT (mapWriterT f) ma

section :: Monad m => SectionT m a -> ModuleT m a
section (SectionT ma) = ModuleT $ do
  empty <- gets isEmpty
  if empty
    then modify $ \s -> s{isEmpty = False}
    else tell "\n"
  lift ma

newtype SectionT m a = SectionT (WriterT Builder m a)
  deriving (Functor, Applicative, Monad)

runSectionT :: Monad m => SectionT m a -> m (Lazy.Text, a)
runSectionT (SectionT ma) =
  Tuple.swap . fmap Builder.toLazyText <$> runWriterT ma

line :: Monad m => Builder -> SectionT m ()
line l = SectionT . tell $ l <> "\n"

newtype BlockT m a = BlockT (ReaderT Word (WriterT (DList Builder) (ModuleT m)) a)
  deriving (Functor, Applicative, Monad, MonadFresh)

instance MonadTrans BlockT where
  lift = BlockT . lift . lift . lift

runBlockT ::
  Monad m =>
  -- | Indent level
  Word ->
  BlockT m a ->
  ModuleT m (SectionT m a)
runBlockT n (BlockT ma) = do
  (a, ls) <- runWriterT $ runReaderT ma n
  pure $ a <$ traverse_ line (DList.toList ls)

hoistBlockT :: (Monad m, Monad m') => (forall x. m x -> m' x) -> BlockT m a -> BlockT m' a
hoistBlockT f (BlockT ma) = BlockT $ mapReaderT (mapWriterT (hoistModuleT f)) ma

statement :: Monad m => Builder -> BlockT m ()
statement s = BlockT $ do
  n <- ask
  tell . DList.singleton $
    Builder.fromText (Text.replicate (fromIntegral n) " ") <> s <> ";"

ifThenElse ::
  Monad m =>
  -- | Condition
  Builder ->
  -- | True case
  BlockT m () ->
  -- | False case
  BlockT m () ->
  BlockT m ()
ifThenElse cond (BlockT mt) (BlockT mf) = BlockT $ do
  tell . DList.singleton $ hsep ["if", parens cond, "{"]
  local (+ 2) mt
  tell . DList.singleton $ hsep ["}", "else", "{"]
  local (+ 2) mf
  tell $ DList.singleton "}"

return' ::
  Monad m =>
  Builder ->
  BlockT m ()
return' val = statement $ hsep ["return", val]

toplevel :: Monad m => ModuleT m a -> BlockT m a
toplevel = BlockT . lift . lift

comment :: Monad m => Builder -> BlockT m ()
comment c = BlockT . tell . DList.singleton $ "// " <> c

includes ::
  Monad m =>
  -- | Include items
  --
  -- e.g. @"<stdio.h>"@, @"\"file.h\""@
  [Builder] ->
  ModuleT m ()
includes ls = section $ traverse_ (line . ("#include " <>)) ls

text :: Text -> Builder
text = Builder.fromText

parens :: Builder -> Builder
parens x = "(" <> x <> ")"

sepBy :: Monoid m => m -> [m] -> m
sepBy _ [] = mempty
sepBy _ [x] = x
sepBy sep (x : xs@(_ : _)) = x <> sep <> sepBy sep xs

hsep :: [Builder] -> Builder
hsep = sepBy " "

functionSignature ::
  Monad m =>
  -- | Return type
  Builder ->
  -- | Name
  Builder ->
  -- \| Type

  -- | Arguments
  [ ( Builder
    , -- \| Name
      Builder
    )
  ] ->
  ModuleT m ()
functionSignature ret name args =
  section . line $
    hsep
      [ ret
      , name
          <> parens (sepBy ", " $ fmap (\(argTy, argName) -> argTy <> " " <> argName) args)
          <> ";"
      ]

function ::
  Monad m =>
  -- | Return type
  Builder ->
  -- | Name
  Builder ->
  -- \| Type

  -- | Arguments
  [ ( Builder
    , -- \| Name
      Builder
    )
  ] ->
  -- | Body
  BlockT m a ->
  ModuleT m a
function ret name args ma = do
  s <- runBlockT 2 ma
  section $ do
    line $
      hsep
        [ ret
        , name
            <> parens
              (sepBy ", " $ fmap (\(argTy, argName) -> argTy <> " " <> argName) args)
        , "{"
        ]
    a <- s
    line "}"
    pure a

typedef ::
  Monad m =>
  -- | Type
  Builder ->
  -- | Alias
  Builder ->
  ModuleT m ()
typedef type_ alias =
  section . line $ hsep ["typedef", type_, alias <> ";"]

externConst ::
  Monad m =>
  -- | Type
  Builder ->
  -- | Name
  Builder ->
  ModuleT m ()
externConst type_ name =
  section . line $ hsep ["extern", "const", type_, name <> ";"]

const' ::
  Monad m =>
  -- | Type
  Builder ->
  -- | Name
  Builder ->
  -- | Value
  Builder ->
  ModuleT m ()
const' type_ name value =
  section . line $ hsep ["const", type_, name, "=", value <> ";"]

declare ::
  Monad m =>
  -- | Type
  Builder ->
  -- | Name
  Builder ->
  BlockT m ()
declare type_ name = statement $ hsep [type_, name]

define ::
  Monad m =>
  -- | Type
  Builder ->
  -- | Name
  Builder ->
  -- | Value
  Builder ->
  BlockT m ()
define type_ name value = statement $ hsep [type_, name, "=", value]

tyStruct :: [(Builder, Builder)] -> Builder
tyStruct [] = "struct {}"
tyStruct fields =
  hsep $
    "struct {" : fmap (\(ty, name) -> ty <> " " <> name <> ";") fields ++ ["}"]

struct :: [(Builder, Builder)] -> Builder
struct fields =
  "{" <> sepBy ", " (fmap (\(name, val) -> "." <> name <> " = " <> val) fields) <> "}"

lvalFunctionPointer ::
  -- | Return type
  Builder ->
  -- | Name
  Builder ->
  -- | Argument types
  [Builder] ->
  Builder
lvalFunctionPointer ret name args =
  ret <> parens ("*" <> name) <> parens (sepBy ", " args)

int :: Int -> Builder
int = Builder.fromString . show

word :: Word -> Builder
word = Builder.fromString . show

ident :: Text -> Builder
ident = text

cast ::
  -- | Type
  Builder ->
  -- | Value
  Builder ->
  Builder
cast ty val = parens (parens ty <> val)

assign ::
  Monad m =>
  -- | LHS
  Builder ->
  -- | RHS
  Builder ->
  BlockT m ()
assign lhs rhs = statement $ hsep [lhs, "=", rhs]

block :: Monad m => BlockT m a -> BlockT m a
block (BlockT ma) = BlockT $ do
  do
    n <- ask
    tell . DList.singleton $ text (Text.replicate (fromIntegral n) " ") <> "{"
  a <- local (+ 2) ma
  do
    n <- ask
    tell . DList.singleton $ text (Text.replicate (fromIntegral n) " ") <> "}"
  pure a

call ::
  -- | Function
  Builder ->
  -- | Arguments
  [Builder] ->
  Builder
call f xs = f <> parens (sepBy ", " xs)
