{-# language UndecidableInstances #-}
{-# language GADTs #-}
{-# language TypeFamilies #-}
{-# language PolyKinds #-} {- https://stackoverflow.com/questions/45659190/why-is-this-hasfield-instance-not-being-resolved -}
{-# language TypeApplications #-}
{-# language ScopedTypeVariables #-}
{-# language FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LambdaCase #-}
module Js 
  ( -- * Identifiers
    Ident(..)

    -- * Expressions
  , proc0
  , proc
  , proc2
  , lam
  , lam2
  , app0
  , (@@)
  , array
  , arrayOfLen
  , str
  , Expr(..)
  , SomeExpr(..)
  , renderExpr

    -- * Statements
  , (.=)
  , Js.const
  , var
  , var_
  , call
  , expr
  , ret
  , ret_
  , Statement(..)
  , renderStatement

    -- * Imports
  , Definition(..)
  , require

    -- * Pattern matching
  , HasMatch(..)

    -- * Functions
  , Args
  , Func(..)
  , func1
  , func2
  , func3
  , func5

    -- * Subtyping
  , As(..)
  , as
  , refine

    -- * Objects
  , IsObject(..)
  , HasNew(..)
  , Object
  , object

    -- * Nullability
  , Js.null
  , nonNull
  , Null
  , NullOr
  , PNullOr(..)
  , Nullable

    -- * Undefinedness
  , Js.undefined
  , defined
  , Undefined
  , UndefinedOr
  , PUndefinedOr(..)
  , Optional

    -- * Error handling
  , throw
  , Debug(..)
  , Fallible(..)
  , unwrap

    -- * Type reflection
  , IsType
  , type_

    -- * Functional programming
    
    -- ** @Maybe@
  , Maybe
  , PMaybe(..)
  , nothing
  , just

    -- ** @Unit@
  , unit

    -- * Builtins
  , instanceOf
  , PInstanceOf(..)
    
    -- ** Builtin functions
  , encodeURIComponent

    -- *** JSON
  , stringify
  , parse

    -- ** Errors
  , Error
  , TypeError

    -- * Primitive Javascript types

    -- ** @Array@
  , Array
  , ArrayFields(..)

    -- ** @Int@
  , Int

    -- ** @String@
  , Text
  , TextFields(..)

    -- * Web API types

    -- ** @Attr@
  , Attr
  , AttrFields(..)

    -- ** @Document@
  , document
  , Document
  , DocumentFields(..)

    -- ** @Console@
  , console
  , Console
  , ConsoleFields(..)

    -- ** @DOMStringMap@
  , DOMStringMap

    -- ** @Element@
  , Element
  , ElementFields(..)

    -- ** @HTMLElement@
  , HTMLElement
  , HTMLElementFields(..)
    
    -- ** @HTMLElement@
  , HTMLTextAreaElement
  , HTMLTextAreaElementFields(..)

    -- ** @Node@
  , Node
  , NodeFields(..)
  , PNode(..)

    -- ** @NodeList@
  , NodeList
  , NodeListFields(..)

    -- ** @Promise@
  , Promise
  , PromiseOf

    -- ** @Response@
  , Response
  , ResponseFields(..)

    -- ** @TextNode@
  , TextNode
  , TextNodeFields(..)

    -- ** @Window@
  , window
  , Window
  , WindowFields(..)

    -- *** Fetch API
  , FetchOptions
  , FetchOptionsFields(..)
  , newFetchOptions

    -- * Misc
  , createTextNode
  , createElement
  , createAttr
  )
where

import Data.Foldable (traverse_)
import Data.Kind (Type)
import GHC.TypeLits (Symbol, KnownSymbol, symbolVal)
import GHC.Exts (IsList(..))
import qualified Data.Text as Text
import Data.Proxy (Proxy(..))
import GHC.Records
import Control.Monad.Writer.CPS (WriterT, execWriterT, tell)
import Data.Text (Text)
import Id (MonadFresh(..))
import Data.String (IsString(..))
import Type.Reflection (Typeable, TypeRep, typeRep, typeRepTyCon, tyConName)
import Data.Tuple (Solo)
import Data.Aeson (Value)
import Data.Coerce (Coercible)
import qualified Data.List as List
import Html.Class (Html)
import qualified Html.Class as Html
import qualified Maybe

newtype Ident = Ident Text
  deriving IsString

renderIdent :: Ident -> Text
renderIdent (Ident i) = i

data Statement r a where
  -- Sequencing
  --
  -- | An empty statement.
  Pure ::
    a ->
    -- | &#xA0;
    Statement r a

  -- | Sequential statements.
  Bind ::
    Statement r a ->
    (a -> Statement r b) ->
    -- | &#xA0;
    Statement r b

  -- | @«expr»@
  Expr ::
    Expr a ->
    -- | &#xA0;
    Statement r ()

  -- Variable definition
  --
  -- | @const «ident»: «type» = «expr»;@
  DefConst ::
    Ident ->
    Expr a ->
    -- | &#xA0;
    Statement r (Expr a)
  
  -- | @var «ident»: «type»;@
  --
  -- @var «ident»: «type» = «expr»;@
  DefVar ::
    Ident ->
    Maybe (Expr a) ->
    -- | &#xA0;
    Statement r (Expr a)
  
  -- Control
  --
  -- | @return «expr»;@
  Ret ::
    Expr r ->
    -- | &#xA0;
    Statement r ()
  
  -- | @return;@
  Ret_ ::
    -- | &#xA0;
    Statement () ()

  -- | @if («expr») { «statements» } else { «statements» }@
  IfThenElse ::
    Expr Bool ->
    Statement r () ->
    Statement r () ->
    -- | &#xA0;
    Statement r ()

  -- |
  -- @
  -- switch («expr») {
  --   case «expr»:
  --     «statements»
  --     break;
  --
  --   ...
  --
  --   default:
  --     «statements»
  --     break;
  -- }
  -- @
  Switch ::
    Expr a ->
    [(Expr a, Statement r ())] ->
    Statement r () ->
    -- | &#xA0;
    Statement r ()

  -- |
  -- @
  -- throw «expr»;
  -- @
  Throw ::
    Expr a ->
    -- | &#xA0;
    Statement r ()

  -- Function definition
  --
  -- |
  -- @
  -- function «ident»(«ident») {
  --   «statements»
  -- }
  -- @
  Func1 ::
    (Func (Solo a) b -> Expr a -> Statement b ()) ->
    -- | &#xA0;
    Statement r (Func (Solo a) b)
  
  -- |
  -- @
  -- function «ident»(«ident», «ident») {
  --   «statements»
  -- }
  -- @
  Func2 ::
    (Func (a, b) c -> (Expr a, Expr b) -> Statement c ()) ->
    -- | &#xA0;
    Statement r (Func (a, b) c)
  
  -- |
  -- @
  -- function «ident»(«ident», «ident», «ident») {
  --   «statements»
  -- }
  -- @
  Func3 ::
    ((Func (a, b, c) d) -> (Expr a, Expr b, Expr c) -> Statement d ()) ->
    -- | &#xA0;
    Statement r (Func (a, b, c) d)
  
  -- |
  -- @
  -- function «ident»(«ident», «ident», «ident», «ident», «ident») {
  --   «statements»
  -- }
  -- @
  Func5 ::
    ((Func (a, b, c, d, e) f) -> (Expr a, Expr b, Expr c, Expr d, Expr e) -> Statement f ()) ->
    -- | &#xA0;
    Statement r (Func (a, b, c, d, e) f)

  -- Assignment
  --
  -- | @«expr» = «expr»;@
  Assign ::
    Expr a ->
    Expr a ->
    -- | &#xA0;
    Statement r ()

instance Functor (Statement r) where
  fmap f ma = do
    a <- ma
    pure (f a)

instance Applicative (Statement r) where
  pure = Pure
  (<*>) mf ma = do
    f <- mf
    a <- ma
    pure (f a)

instance Monad (Statement r) where
  (>>=) = Bind

data Expr a where
  Var :: Ident -> Expr a
  Prj :: Expr a -> Text -> Expr b
  Lam :: (Expr a -> Expr b) -> Expr (a -> b)
  Lam2 :: ((Expr a, Expr b) -> Expr c) -> Expr ((a, b) -> c)
  Proc0 :: Statement b () -> Expr (() -> b)
  Proc :: (Expr a -> Statement b ()) -> Expr (a -> b)
  Proc2 :: ((Expr a, Expr b) -> Statement c ()) -> Expr ((a, b) -> c)
  Proc3 :: ((Expr a, Expr b, Expr c) -> Statement d ()) -> Expr ((a, b, c) -> d)
  Call0 :: Expr (() -> b) -> Expr b
  Call1 :: Expr (a -> b) -> Expr a -> Expr b
  Call2 :: Expr ((a, b) -> b) -> (Expr a, Expr b) -> Expr c
  Call3 :: Expr ((a, b, c) -> d) -> (Expr a, Expr b, Expr c) -> Expr d
  Call5 :: Expr ((a, b, c, d, e) -> f) -> (Expr a, Expr b, Expr c, Expr d, Expr e) -> Expr f
  Str :: Text -> Expr Text
  Array :: [Expr a] -> Expr (Array a)
  ArrayOfLen :: Typeable a => Expr Int -> Expr (Array a)
  Index :: Expr (Array a) -> Expr Int -> Expr a
  StrictlyEq :: Expr a -> Expr a -> Expr Bool
  InstanceOf :: Expr a -> Expr (TypeRep b) -> Expr Bool
  UnsafeCast :: forall a b. Expr a -> Expr b
  UnsafeObject :: [(Text, SomeExpr)] -> Expr a
  UnsafeNew :: Expr a -> Expr b
  AddInt :: Expr Int -> Expr Int -> Expr Int
  Int :: Integer -> Expr Int

data SomeExpr where
  SomeExpr :: Expr a -> SomeExpr

-- | Javascript strings can be constructed using Haskell string syntax:
--
-- @
-- {-# LANGUAGE OverloadedStrings #-}
--
-- x :: Expr Text
-- x = "Hello, world!"
-- @
instance a ~ Text => IsString (Expr a) where
  fromString = Str . fromString

-- | Javascript t'Array's can be constructed using Haskell list syntax:
--
-- @
-- {-# LANGUAGE OverloadedLists #-}
--
-- x :: Expr (Array Text)
-- x = ['str' "a", str "b", str "c"]
-- @
--
-- &#xA0;
--
-- Note: 'GHC.Exts.toList' is not implemented. Destructuring an @t'Expr' t'Array'@
-- using list pattern syntax will fail at runtime.
instance IsList (Expr (Array a)) where
  type Item (Expr (Array a)) = Expr a
  fromList = Array
  toList = error "list patterns for Expr not supported"

const :: Expr a -> Statement r (Expr a)
const = DefConst "const"

var :: Expr a -> Statement r (Expr a)
var = DefVar "var" . Just

var_ :: Statement r (Expr a)
var_ = DefVar "var" Nothing

freshIdent :: MonadFresh m => Text -> m Ident
freshIdent name =
  Ident . (name <>) . ("_" <>) . fromString . show <$> freshId

renderStatement :: MonadFresh m => Statement r a -> m [Text]
renderStatement = execWriterT . go
  where    
    go :: MonadFresh m => Statement r a -> WriterT [Text] m a
    go (DefConst (Ident name) value) = do
      v <- freshIdent name
      value' <- renderExpr value
      tell ["const " <> renderIdent v <> " = " <> value' <> ";"]
      pure $ Var v
    go (DefVar (Ident name) value) = do
      v <- freshIdent name
      value' <- traverse renderExpr value
      tell ["var " <> renderIdent v <> Prelude.maybe "" (" = " <>) value' <> ";"]
      pure $ Var v
    go (Expr e) = do
      e' <- renderExpr e
      tell [e' <> ";"]
    go (Pure a) =
      pure a
    go (Bind a f) = do
      a' <- go a
      go (f a')
    go (Ret a) = do
      a' <- renderExpr a
      tell ["return " <> a' <> ";"]
    go Ret_ =
      tell ["return;"]
    go (Assign lvar value) = do
      lvar' <- renderExpr lvar
      value' <- renderExpr value
      tell [lvar' <> " = " <> value' <> ";"]
    go (IfThenElse cond t e) = do
      cond' <- renderExpr cond
      tell [ "if (" <> cond' <> ") {" ]
      go t
      tell [ "} else {"]
      go e
      tell ["}"]
    go (Switch val cases def) = do
      val' <- renderExpr val
      tell ["switch (" <> val' <> ") {"]
      traverse_
        (\(e, s) -> do
          e' <- renderExpr e
          tell ["case " <> e' <> ":"]
          go s
          tell ["break;"]
        )
        cases
      tell ["default:"]
      go def
      tell ["break;"]
      tell ["}"]
    go (Func1 f) = do
      name <- freshIdent "func"
      arg <- freshIdent "arg"
      tell ["function " <> renderIdent name <> "(" <> renderIdent arg <> ") {"]
      go $ f (Func (Var name `Call1`)) (Var arg)
      tell ["}"]
      pure (Func (Var name `Call1`))
    go (Func2 f) = do
      name <- freshIdent "func"
      arg1 <- freshIdent "arg"
      arg2 <- freshIdent "arg"
      tell ["function " <> renderIdent name <> "(" <> Text.intercalate ", " (fmap renderIdent [arg1, arg2]) <> ") {"]
      go $ f (Func (Call2 $ Var name)) (Var arg1, Var arg2)
      tell ["}"]
      pure (Func (Call2 $ Var name))
    go (Func3 f) = do
      name <- freshIdent "func"
      arg1 <- freshIdent "arg"
      arg2 <- freshIdent "arg"
      arg3 <- freshIdent "arg"
      tell ["function " <> renderIdent name <> "(" <> Text.intercalate ", " (fmap renderIdent [arg1, arg2, arg3]) <> ") {"]
      go $ f (Func (Call3 $ Var name)) (Var arg1, Var arg2, Var arg3)
      tell ["}"]
      pure (Func (Call3 $ Var name))
    go (Func5 f) = do
      name <- freshIdent "func"
      arg1 <- freshIdent "arg"
      arg2 <- freshIdent "arg"
      arg3 <- freshIdent "arg"
      arg4 <- freshIdent "arg"
      arg5 <- freshIdent "arg"
      tell ["function " <> renderIdent name <> "(" <> Text.intercalate ", " (fmap renderIdent [arg1, arg2, arg3, arg4, arg5]) <> ") {"]
      go $ f (Func (Call5 $ Var name)) (Var arg1, Var arg2, Var arg3, Var arg4, Var arg5)
      tell ["}"]
      pure (Func (Call5 $ Var name))
    go (Throw e) = do
      e' <- renderExpr e
      tell ["throw " <> e' <> ";"]

parens :: (Semigroup a, IsString a) => a -> a
parens x = "(" <> x <> ")"

isLambda :: Expr a -> Bool
isLambda Var{} = False
isLambda Prj{} = False
isLambda Call0{} = False
isLambda Call1{} = False
isLambda Call2{} = False
isLambda Call3{} = False
isLambda Call5{} = False
isLambda Str{} = False
isLambda Array{} = False
isLambda ArrayOfLen{} = False
isLambda Index{} = False
isLambda StrictlyEq{} = False
isLambda InstanceOf{} = False
isLambda UnsafeCast{} = False
isLambda UnsafeObject{} = False
isLambda UnsafeNew{} = False
isLambda AddInt{} = False
isLambda Int{} = False
isLambda Lam{} = True
isLambda Lam2{} = True
isLambda Proc0{} = True
isLambda Proc{} = True
isLambda Proc2{} = True
isLambda Proc3{} = True

renderExpr :: MonadFresh m => Expr a -> m Text
renderExpr (Var v) = pure $ renderIdent v
renderExpr (Prj e field) = do
  e' <- renderExpr e
  pure $ e' <> "." <> field
renderExpr (Call0 f) = do
  f' <- renderExpr f
  pure $ (if isLambda f then parens else id) f' <> "()"
renderExpr (Call1 f x) = do
  f' <- renderExpr f
  x' <- renderExpr x
  pure $ (if isLambda f then parens else id) f' <> "(" <> x' <> ")"
renderExpr (Call2 f (x, y)) = do
  f' <- renderExpr f
  x' <- renderExpr x
  y' <- renderExpr y
  pure $ (if isLambda f then parens else id) f' <> "(" <> x' <> ", " <> y' <> ")"
renderExpr (Call3 f (x, y, z)) = do
  f' <- renderExpr f
  x' <- renderExpr x
  y' <- renderExpr y
  z' <- renderExpr z
  pure $ (if isLambda f then parens else id) f' <> "(" <> Text.intercalate ", " [x', y', z'] <> ")"
renderExpr (Call5 f (a, b, c, d, e)) = do
  f' <- renderExpr f
  a' <- renderExpr a
  b' <- renderExpr b
  c' <- renderExpr c
  d' <- renderExpr d
  e' <- renderExpr e
  pure $ (if isLambda f then parens else id) f' <> "(" <> Text.intercalate ", " [a', b', c', d', e'] <> ")"
renderExpr (Str s) =
  -- TODO: escape
  pure $ "\"" <> s <> "\""
renderExpr (Lam f) = do
  v <- freshIdent "arg"
  e <- renderExpr $ f (Var v)
  pure $ "(" <> renderIdent v <> ") => " <> e
renderExpr (Lam2 f) = do
  v1 <- freshIdent "arg"
  v2 <- freshIdent "arg"
  e <- renderExpr $ f (Var v1, Var v2)
  pure $ "(" <> renderIdent v1 <> ", " <> renderIdent v2 <> ") => " <> e
renderExpr (Proc0 f) = do
  sts <- renderStatement f
  pure $ "() => {" <> Text.intercalate " " sts <> "}"
renderExpr (Proc f) = do
  v <- freshIdent "arg"
  sts <- renderStatement $ f (Var v)
  pure $ "(" <> renderIdent v <> ") => {" <> Text.intercalate " " sts <> "}"
renderExpr (Proc2 f) = do
  v1 <- freshIdent "arg"
  v2 <- freshIdent "arg"
  sts <- renderStatement $ f (Var v1, Var v2)
  pure $ "(" <> renderIdent v1 <> ", " <> renderIdent v2 <> ") => {" <> Text.intercalate " " sts <> "}"
renderExpr (Proc3 f) = do
  v1 <- freshIdent "arg"
  v2 <- freshIdent "arg"
  v3 <- freshIdent "arg"
  sts <- renderStatement $ f (Var v1, Var v2, Var v3)
  pure $ "(" <> Text.intercalate ", " (fmap renderIdent [v1, v2, v3]) <> ") => {" <> Text.intercalate " " sts <> "}"
renderExpr (ArrayOfLen len) = do
  len' <- renderExpr len
  pure $ "Array(" <> len' <> ")"
renderExpr (Index arr ix) = do
  arr' <- renderExpr arr
  ix' <- renderExpr ix
  pure $ arr' <> "[" <> ix' <> "]"
renderExpr (StrictlyEq a b) = do
  a' <- renderExpr a
  b' <- renderExpr b
  pure $ a' <> " === " <> b'
renderExpr (UnsafeCast a) =
  renderExpr a
renderExpr (UnsafeObject fields) = do
  fields' <-
    traverse
      (\(key, SomeExpr value) -> do
        value' <- renderExpr value
        pure $ renderField key <> ": " <> value'
      )
      fields
  pure $ "{" <> Text.intercalate ", " fields' <> "}"
renderExpr (Array items) = do
  items' <- traverse renderExpr items
  pure $ "[" <> Text.intercalate ", " items' <> "]"
renderExpr (InstanceOf val ty) = do
  val' <- renderExpr val
  ty' <- renderExpr ty
  pure $ val' <> " instanceof " <> ty'
renderExpr (UnsafeNew val) = do
  val' <- renderExpr val
  -- TODO: precedence
  pure $ "new " <> val'
renderExpr (AddInt a b) = do
  a' <- renderExpr a
  b' <- renderExpr b
  -- TODO: precedence / associativity
  pure $ a' <> " + " <> b'
renderExpr (Int n) =
  pure $ fromString (show n)

renderField :: Text -> Text
renderField field
  | Text.any (\c -> c `List.elem` (" -" :: [Char])) field =
      -- TODO: escape
      "\"" <> field <> "\""
  | otherwise = field

str :: Text -> Expr Text
str = Str

expr :: Expr a -> Statement r ()
expr = Expr

call :: (a -> Expr ()) -> a -> Statement r ()
call f x = Expr $ f x

app0 :: Expr (() -> a) -> Expr a
app0 = Call0

lam :: (Expr a -> Expr b) -> Expr (a -> b)
lam = Lam

lam2 :: ((Expr a, Expr b) -> Expr c) -> Expr ((a, b) -> c)
lam2 = Lam2

proc0 :: Statement b () -> Expr (() -> b)
proc0 = Proc0

proc :: (Expr a -> Statement b ()) -> Expr (a -> b)
proc = Proc

proc2 :: ((Expr a, Expr b) -> Statement c ()) -> Expr ((a, b) -> c)
proc2 = Proc2

(.=) :: Expr a -> Expr a -> Statement r ()
(.=) = Assign

infix 5 .=

(@@) :: Expr (a -> b) -> Expr a -> Expr b
(@@) = Call1

ret :: Expr r -> Statement r ()
ret = Ret

ret_ :: Statement () ()
ret_ = Ret_

data Element

instance IsType Element where
  typeName = "Element"

data ElementFields
  = ElementFields
  { addEventListener :: Expr Element -> (Expr Text, Expr (() -> ())) -> Expr ()
  , append :: Expr Element -> Expr Node -> Expr ()
  , id :: Expr Element -> Expr Text
  , style :: Expr Element -> Expr Styles
  , textContent :: Expr Element -> Expr Text
  , remove :: Expr Element -> () -> Expr ()
  , querySelector :: Expr Element -> Expr Text -> Expr (Nullable Element)
  , setAttribute :: Expr Element -> (Expr Text, Expr Text) -> Expr ()
  , setAttributeNode :: Expr Element -> Expr Attr -> Expr ()
  , asNode :: Expr Element -> Expr Node
  }

class IsObject object where
  type FieldsFor object :: Type
  mkFieldsFor :: FieldsFor object

instance IsObject Element where
  type FieldsFor Element = ElementFields
  
  mkFieldsFor =
    ElementFields
    { addEventListener = \el -> Call2 (Prj el "addEventListener")
    , append = \el el' -> Call1 (Prj el "append") el'
    , id = \el -> Prj el "id"
    , style = \el -> Prj el "style"
    , textContent = \el -> Prj el "textContent"
    , remove = \el () -> Call0 (Prj el "remove")
    , querySelector = \el -> Call1 (Prj el "querySelector")
    , setAttribute = \el -> Call2 (Prj el "setAttribute")
    , setAttributeNode = \el -> Call1 (Prj el "setAttributeNode")
    , asNode = UnsafeCast @Element @Node
    }

-- | Javascript object fields can be accessed using [record dot syntax](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/overloaded_record_dot.html):
--
-- @
-- {-# LANGUAGE OverloadedRecordDot #-}
--
-- x :: Expr ()
-- x = 'console'.log ('str' "Hello, Javascript!")
-- @
instance {-# overlappable #-} (IsObject object, HasField field (FieldsFor object) (Expr object -> ty)) => HasField (field :: Symbol) (Expr object) ty where
  getField = getField @field (mkFieldsFor @object)

data Styles

instance {-# overlaps #-} (KnownSymbol field, ty ~ Expr Text) => HasField (field :: Symbol) (Expr Styles) ty where
  getField e = Prj e (fromString $ symbolVal (Proxy :: Proxy field))

data TextNode

data TextNodeFields
  = TextNodeFields
  { asNode :: Expr TextNode -> Expr Node
  }

instance IsObject TextNode where
  type FieldsFor TextNode = TextNodeFields
  
  mkFieldsFor =
    TextNodeFields
    { asNode = UnsafeCast @TextNode @Node
    }

data Document

data DocumentFields
  = DocumentFields
  { getElementById :: Expr Document -> Expr Text -> Expr Element
  , createElement :: Expr Document -> Expr Text -> Expr Element
  , createTextNode :: Expr Document -> Expr Text -> Expr TextNode
  , createAttribute :: Expr Document -> Expr Text -> Expr Attr
  , querySelectorAll :: Expr Document -> Expr Text -> Expr NodeList
  }

document :: Expr Document
document = Var "document"

instance IsObject Document where
  type FieldsFor Document = DocumentFields
  
  mkFieldsFor =
    DocumentFields
    { getElementById = \doc elId -> Call1 (Prj doc "getElementById") elId
    , createElement = \doc -> Call1 (Prj doc "createElement")
    , createTextNode = \doc -> Call1 (Prj doc "createTextNode")
    , createAttribute = \doc -> Call1 (Prj doc "createAttribute")
    , querySelectorAll = \doc -> Call1 (Prj doc "querySelectorAll")
    }

data Console

data ConsoleFields
  = ConsoleFields
  { log :: Expr Console -> Expr Text -> Expr ()
  , logValue :: Expr Console -> Expr Value -> Expr ()
  }

console :: Expr Console
console = Var "console"

instance IsObject Console where
  type FieldsFor Console = ConsoleFields
  
  mkFieldsFor =
    ConsoleFields
    { log = \con -> Call1 (Prj con "log")
    , logValue = \con -> Call1 (Prj con "log")
    }

data NodeList

data NodeListFields
  = NodeListFields
  { length :: Expr NodeList -> Expr Int
  , forEach :: Expr NodeList -> Expr (Node -> ()) -> Expr ()
  , forEachIndexed :: Expr NodeList -> Expr ((Node, Int) -> ()) -> Expr ()
  }

instance IsObject NodeList where
  type FieldsFor NodeList = NodeListFields
  
  mkFieldsFor =
    NodeListFields
    { length = \el -> Prj el "length"
    , forEach = \el -> Call1 (Prj el "forEach")
    , forEachIndexed = \el -> Call1 (Prj el "forEach")
    }

data Node

data NodeFields
  = NodeFields
  { addEventListener :: Expr Node -> (Expr Text, Expr (() -> ())) -> Expr ()
  , toString :: Expr Node -> () -> Expr Text
  , appendChild :: Expr Node -> Expr Node -> Expr Node
  , insertBefore :: Expr Node -> (Expr Node, Expr Node) -> Expr Node
  , removeChild :: Expr Node -> Expr Node -> Expr Node
  , parentNode :: Expr Node -> Expr (Nullable Node)
  }

instance IsObject Node where
  type FieldsFor Node = NodeFields
  
  mkFieldsFor =
    NodeFields
    { addEventListener = \el -> Call2 (Prj el "addEventListener")
    , toString = \el () -> Call0 (Prj el "toString")
    , appendChild = \el -> Call1 (Prj el "appendChild")
    , removeChild = \el -> Call1 (Prj el "removeChild")
    , insertBefore = \el -> Call2 (Prj el "insertBefore")
    , parentNode = \el -> Prj el "parentNode"
    }

instance As TextNode Node where
  cast = (.asNode)

instance As Element Node where
  cast = (.asNode)

data PNode
  = PElement (Expr Element)
  | PTextNode (Expr TextNode)
  | POther

instance HasMatch Node where
  type Pattern Node = PNode

  match e f = do
    e' <- Js.const e
    Switch
      (Prj e' "nodeType")
      [ (Prj (Var "Node") "ELEMENT_NODE", f (PElement $ UnsafeCast @Node @Element e'))
      , (Prj (Var "Node") "TEXT_NODE", f (PTextNode $ UnsafeCast @Node @TextNode e'))
      ]
      (f POther)

data Array (a :: Type)

data ArrayFields a
  = ArrayFields
  { index :: Expr (Array a) -> Expr Int -> Expr a
  , forEach :: Expr (Array a) -> Expr (a -> ()) -> Expr ()
  , push :: Expr (Array a) -> Expr a -> Expr Int
  , length :: Expr (Array a) -> Expr Int
  , concat :: Expr (Array a) -> Expr (Array a) -> Expr (Array a)
  }

instance IsObject (Array a) where
  type FieldsFor (Array a) = ArrayFields a
  
  mkFieldsFor =
    ArrayFields
    { index = Index
    , forEach = \self -> Call1 (Prj self "forEach")
    , push = \self -> Call1 (Prj self "push")
    , length = \self -> Prj self "length"
    , concat = \self -> Call1 (Prj self "concat")
    }

instance Semigroup (Expr (Array a)) where
  a <> b = a.concat b

instance Monoid (Expr (Array a)) where
  mempty = Array []

arrayOfLen :: Typeable a => Expr Int -> Expr (Array a)
arrayOfLen = ArrayOfLen

array :: [Expr a] -> Expr (Array a)
array = Array

data Null

data NullOr (a :: Type)

instance As Null (NullOr a)

type family Nullable (a :: Type) where
  Nullable Null = Null
  Nullable (NullOr a) = Nullable a
  Nullable a = NullOr a

null :: Expr Null
null = Var (Ident "null")

nonNull :: Expr a -> Expr (Nullable a)
nonNull = UnsafeCast

class HasMatch (a :: Type) where
  type Pattern a :: Type

  match :: Expr a -> (Pattern a -> Statement r ()) -> Statement r ()

data PNullOr a
  = PNull
  | PNonNull (Expr a)

instance Typeable a => HasMatch (NullOr a) where
  type Pattern (NullOr a) = PNullOr a

  match e f = do
    e' <- Js.const e
    IfThenElse
      (e' `StrictlyEq` Var "null")
      (f PNull)
      (f (PNonNull (UnsafeCast @(NullOr a) @a e')))

data Attr

data AttrFields
  = AttrFields
  { name :: Expr Attr -> Expr Text
  , value :: Expr Attr -> Expr Text
  }

instance IsObject Attr where
  type FieldsFor Attr = AttrFields
  
  mkFieldsFor =
    AttrFields
    { name = (`Prj` "name")
    , value = (`Prj` "value")
    }

{-
mkAttr :: Expr Text -> Expr Text -> Expr Attr
mkAttr key value = UnsafeObject [("name", SomeExpr key), ("value", SomeExpr value)]

data Html
  = HtmlElement Text [Attr] [Html]
  | HtmlText Text

html :: Html -> Expr Html
html (HtmlElement tag attrs children) =
  mkElement (str tag) (array $ fmap attr attrs) (array $ fmap html children)
html (HtmlText value) = mkText (str value)

mkElement :: Expr Text -> Expr (Array Attr) -> Expr (Array Html) -> Expr Html
mkElement tag attrs children =
  UnsafeObject
  [ ("nodeType", SomeExpr "element")
  , ("tag", SomeExpr tag)
  , ("attrs", SomeExpr attrs)
  , ("children", SomeExpr children)
  ]

mkText :: Expr Text -> Expr Html
mkText value =
  UnsafeObject [("nodeType", SomeExpr "text"), ("value", SomeExpr value)]

instance {-# overlaps #-} HasField "nodeType" (Expr Html) (Expr Text) where
  getField e = Prj e "nodeType"

data PHtml
  = PHtmlElement (Expr Text) (Expr (Array Attr)) (Expr (Array Html))
  | PHtmlText (Expr Text)

instance HasMatch Html where
  type Pattern Html = PHtml

  match value f = do
    value' <- Js.const value
    Switch
      value'.nodeType
      [ ("element", f (PHtmlElement (value' `Prj` "tag") (value' `Prj` "attrs") (value' `Prj` "children")))
      , ("text", f (PHtmlText (value' `Prj` "value")))
      ]
      (do
        call console.log "error: invalid nodeType: "
        call console.log value'.nodeType
      )

createNode :: (Expr Html -> Expr Node) -> Expr Html -> Statement Node ()
createNode self h = do
  match
    h
    (\case
      PHtmlElement tag attrs children -> do
        el <- Js.const $ document.createElement tag
        call attrs.forEach . proc $ \a -> do
          call el.setAttribute (a.name, a.value)
        call children.forEach . proc $ \child -> do
          child' <- Js.const $ self child
          call el.append child'
        ret $ UnsafeCast @Element @Node el
      PHtmlText value ->
        ret $ UnsafeCast @TextNode @Node (document.createTextNode value)
    )
-}

-- | @'As' a b@ indicates that a value of type @a@ can used wherever a value of type @b@ is required.
-- In other words, @a@s can be safely cast to @b@s.
class As a b where
  cast :: Expr a -> Expr b
  cast = UnsafeCast

-- | 'as' is 'cast' with the type variable order reversed, for use with explicit type applications:
--
-- @
-- f :: Expr 'Element' -> Expr 'Node'
-- f x = 'as' \@Node x
-- @
as :: forall b a. As a b => Expr a -> Expr b
as = cast

-- | A Javascript expression that represents a Haskell newtype follows Haskell's 'Coercible' rules. 
--
-- This also acts as the no-op cast when @a ~ b@.
instance {-# overlappable #-} Coercible a b => As a b where
  cast = UnsafeCast

instance As Text Value

type family Args (a :: Type) :: Type where
  Args (a, b, c, d, e) = (Expr a, Expr b, Expr c, Expr d, Expr e)
  Args (a, b, c, d) = (Expr a, Expr b, Expr c, Expr d)
  Args (a, b, c) = (Expr a, Expr b, Expr c)
  Args (a, b) = (Expr a, Expr b)
  Args (Solo a) = Expr a

newtype Func a b = Func (forall r. (b `As` r) => Args a -> Expr r)

func1 ::
  (Func (Solo a) b -> Args (Solo a) -> Statement b ()) ->
  Statement r (Func (Solo a) b)
func1 = Func1

func2 ::
  (Func (a, b) c -> Args (a, b) -> Statement c ()) ->
  Statement r (Func (a, b) c)
func2 = Func2

func3 ::
  (Func (a, b, c) d -> Args (a, b, c) -> Statement d ()) ->
  Statement r (Func (a, b, c) d)
func3 = Func3

func5 ::
  (Func (a, b, c, d, e) f -> Args (a, b, c, d, e) -> Statement f ()) ->
  Statement r (Func (a, b, c, d, e) f)
func5 = Func5

data Definition a = Definition Text (forall r. Statement r a)

require :: Definition a -> Statement r a
require (Definition _name sts) =
  -- intention: if some code `require`s the same `Definition` multiple times,
  -- then it will only be "initialized" once.
  sts

createElement :: Definition (Func (Text, Array Attr, Array Node) Element)
createElement =
  Definition "createElement"
  (Js.func3 $ \_self (tag, attrs, children)-> do
    el <- Js.const $ Js.document.createElement tag
    Js.call attrs.forEach . Js.proc $ \a -> do
      Js.call el.setAttribute (a.name, a.value)
    Js.call children.forEach . Js.proc $ \child -> do
      Js.call el.append child
    Js.ret el
  )

createTextNode :: Definition (Func (Solo Text) TextNode)
createTextNode =
  Definition "createTextNode"
  (Js.func1 $ \_self value -> do
    Js.ret $ Js.document.createTextNode value
  )

createAttr :: Definition (Func (Text, Text) Attr)
createAttr =
  Definition "createAttr"
  (func2 $ \_self (name, value) -> do
    attr <- Js.const $ Js.document.createAttribute name
    attr.value .= value
    Js.ret attr
  )

data Window

data WindowFields
  = WindowFields
  { fetch :: Expr Window -> Expr Text -> Expr (Promise Response)
  , fetchWithOptions :: Expr Window -> (Expr Text, Expr FetchOptions) -> Expr (Promise Response)
  }

instance IsObject Window where
  type FieldsFor Window = WindowFields
  mkFieldsFor =
    WindowFields
    { fetch = \el -> Call1 (Prj el "fetch")
    , fetchWithOptions = \el -> Call2 (Prj el "fetch")
    }

data FetchOptions

data FetchOptionsFields
  = FetchOptionsFields
  { body :: Expr FetchOptions -> Expr (Optional Text)
  , method :: Expr FetchOptions -> Expr (Optional Text)
  , headers :: Expr FetchOptions -> Expr (Optional (Object Text))
  }

instance IsObject FetchOptions where
  type FieldsFor FetchOptions = FetchOptionsFields
  mkFieldsFor =
    FetchOptionsFields
    { body = \self -> Prj self "body"
    , method = \self -> Prj self "method"
    , headers = \self -> Prj self "headers"
    }

newFetchOptions :: Expr FetchOptions
newFetchOptions = UnsafeObject []

window :: Expr Window
window = Var (Ident "window")

type family Promise (a :: Type) where
  Promise (PromiseOf a) = Promise a
  Promise (NullOr (PromiseOf a)) = Promise (NullOr a)
  Promise (UndefinedOr (PromiseOf a)) = Promise (UndefinedOr a)
  Promise a = PromiseOf a

data PromiseOf (a :: Type)

data Response

data ResponseFields
  = ResponseFields
  { json :: Expr Response -> () -> Expr (Promise Value)
  , text :: Expr Response -> () -> Expr (Promise Text)
  }

instance IsObject Response where
  type FieldsFor Response = ResponseFields
  mkFieldsFor =
    ResponseFields
    { json = \self () -> Call0 $ Prj self "json"
    , text = \self () -> Call0 $ Prj self "text"
    }

data Object (a :: Type)

instance a ~ Value => IsType (Object a) where
  

instance (KnownSymbol field, ty ~ Optional a) => HasField (field :: Symbol) (Expr (Object a)) (Expr ty) where
  getField self = Prj self (fromString $ symbolVal (Proxy :: Proxy field))

object :: Typeable a => [(Text, Expr a)] -> Expr (Object a)
object entries = UnsafeObject (fmap (\(k, v) -> (k, SomeExpr v)) entries)

data Undefined

data UndefinedOr (a :: Type)

instance As Undefined (UndefinedOr a)

type family Optional (a :: Type) where
  Optional (UndefinedOr a) = Optional a
  Optional a = UndefinedOr a

data PUndefinedOr a
  = PUndefined
  | PDefined (Expr a)

instance Typeable a => HasMatch (UndefinedOr a) where
  type Pattern (UndefinedOr a) = PUndefinedOr a
  match e f = do
    e' <- Js.const e
    IfThenElse
      (e `StrictlyEq` Var (Ident "undefined"))
      (f PUndefined)
      (f (PDefined $ UnsafeCast @(UndefinedOr a) @a e'))

defined :: Expr a -> Expr (Optional a)
defined = UnsafeCast

undefined :: Expr Undefined
undefined = Var (Ident "undefined")

encodeURIComponent :: Expr a -> Expr Text
encodeURIComponent = Call1 (Var $ Ident "encodeURIComponent")

data TextFields
  = TextFields
  { concat :: Expr Text -> Expr Text -> Expr Text
  }

instance IsObject Text where
  type FieldsFor Text = TextFields
  mkFieldsFor =
    TextFields
    { concat = \self -> Call1 (Prj self "concat")
    }

instance Semigroup (Expr Text) where
  a <> b = a.concat b

instance Monoid (Expr Text) where
  mempty = ""

data HTMLElement

instance IsType HTMLElement

data HTMLElementFields
  = HTMLElementFields
  { dataset :: Expr HTMLElement -> Expr DOMStringMap
  }

instance IsObject HTMLElement where
  type FieldsFor HTMLElement = HTMLElementFields
  mkFieldsFor =
    HTMLElementFields
    { dataset = \self -> Prj self "dataset"
    }

data DOMStringMap

instance (KnownSymbol field, ty ~ Optional Text) => HasField (field :: Symbol) (Expr DOMStringMap) (Expr ty) where
  getField self = Prj self (fromString $ symbolVal (Proxy :: Proxy field))

class Typeable a => IsType a where
  typeName :: Text
  typeName = fromString $ tyConName (typeRepTyCon (typeRep @a))

type_ :: forall a. IsType a => Expr (TypeRep a)
type_ = Var (Ident $ typeName @a)

data InstanceOf (a :: Type) (b :: Type)

instanceOf ::
  forall a b.
  IsType b =>
  Expr a ->
  Expr (TypeRep b) ->
  Expr (InstanceOf a b)
instanceOf val _ty = UnsafeCast @a @(InstanceOf a b) val

data PInstanceOf (a :: Type) (b :: Type)
  = PIsInstanceOf (Expr b)
  | PNotInstanceOf

instance IsType b => HasMatch (InstanceOf a b) where
  type Pattern (InstanceOf a b) = PInstanceOf a b
  match e f =
    IfThenElse
      (InstanceOf (UnsafeCast @(InstanceOf a b) @a e) (type_ @b))
      (f (PIsInstanceOf (UnsafeCast @(InstanceOf a b) @b e)))
      (f PNotInstanceOf)

stringify :: Expr a -> Expr Text
stringify = Call1 (Prj (Var $ Ident "JSON") "stringify")

parse :: Expr Text -> Expr Value
parse = Call1 (Prj (Var $ Ident "JSON") "parse")

data HTMLTextAreaElement

instance IsType HTMLTextAreaElement

data HTMLTextAreaElementFields
  = HTMLTextAreaElementFields
  { value :: Expr HTMLTextAreaElement -> Expr Text
  }

instance IsObject HTMLTextAreaElement where
  type FieldsFor HTMLTextAreaElement = HTMLTextAreaElementFields
  mkFieldsFor =
    HTMLTextAreaElementFields
    { value = \self -> Prj self "value"
    }

instance As HTMLTextAreaElement HTMLElement

data Error

instance HasNew Error where
  type ConstructorArgs Error = Args (Solo Text)
  new arg = UnsafeNew (Call1 (Var $ Ident "Error") arg)

data TypeError

class HasNew (object :: Type) where
  type ConstructorArgs object :: Type
  new :: ConstructorArgs object -> Expr object

instance HasNew TypeError where
  type ConstructorArgs TypeError = Args (Solo Text)
  new arg = UnsafeNew (Call1 (Var $ Ident "TypeError") arg)

instance As TypeError Error

throw :: Expr a -> Statement r ()
throw = Throw

just :: Expr a -> Expr (Maybe a)
just a = UnsafeObject [("tag", SomeExpr "Just"), ("value", SomeExpr a)]

nothing :: Expr (Maybe a)
nothing = UnsafeObject [("tag", SomeExpr "Nothing")]

data PMaybe a
  = PNothing
  | PJust (Expr a)

instance HasMatch (Maybe a) where
  type Pattern (Maybe a) = PMaybe a
  match e f = do
    e' <- Js.const e
    Switch
      (Prj e' "tag")
      [ ("Just", f $ PJust (Prj e' "value"))
      , ("Nothing", f PNothing)
      ]
      (throw $ ("invalid tag: " :: Expr Text).concat (e' `Prj` "tag"))

instance Maybe.HasMaybe Expr where
  nothing = Js.nothing
  just = Js.just
  maybe n j ma = Call0 (Proc0 $ match ma (\case; PNothing -> ret n; PJust a -> ret (j a)))

instance Html Expr where
  type Node Expr = Node
  
  text value = as @Node $ document.createTextNode value
  preEscapedText = error "TODO: pre-escaped text"
  
  type Attr Expr = NullOr Js.Attr
  attr name value =
    Call0 . Proc0 $ do
      attr <- Js.const $ document.createAttribute name
      attr.value .= value
      ret $ nonNull attr
  maybeAttr name =
    Maybe.maybe
      (Js.cast Js.null)
      (nonNull . Html.attr name)

  type Attrs Expr = Array (NullOr Attr)
  type Children Expr = Array Node
  el name attrs children =
    Call3
      (Proc3 $ \(name', attrs', children') -> do
        element <- Js.const $ document.createElement name'
        call attrs'.forEach . proc $ \nAttr -> do
          match nAttr
            (\case
              PNull ->
                pure ()
              PNonNull attr ->
                call element.setAttributeNode attr
            )
        call children'.forEach . proc $ call element.append
        ret $ as @Node element
      )
      (name, attrs, children)

instance a ~ Int => Num (Expr a) where
  fromInteger = Int
  (+) = AddInt
  (*) = error "TODO: (*) for Expr Int"
  abs = error "TODO: abs for Expr Int"
  signum = error "TODO: signum for Expr Int"
  negate = error "TODO: negate for Expr Int"

refine :: forall b a r. IsType b => Expr a -> Statement r (Expr b)
refine value = do
  result <- Js.var_
  match
    (value `Js.instanceOf` Js.type_ @b)
    (\case
      PNotInstanceOf ->
        Js.throw $ Js.new @Js.TypeError ("not a " <> str (typeName @b))
      PIsInstanceOf value' ->
        result .= value'
    )
  pure result

class Fallible a where
  type Failure a :: Type
  type Success a :: Type
  
  fallible ::
    Expr a ->
    (Expr (Failure a) -> Statement r ()) ->
    (Expr (Success a) -> Statement r ()) ->
    Statement r ()

class Debug a where
  debug :: Expr a -> Expr Text

instance Debug Null where
  debug _ = "null"

instance Debug Undefined where
  debug _ = "undefined"

instance (Typeable a, Debug a) => Debug (NullOr a) where
  debug value =
    Call0 . proc0 $
    match value
      (\case
        PNull ->
          ret $ debug Js.null
        PNonNull a ->
          ret $ debug a
      )

instance (Typeable a, Debug a) => Debug (UndefinedOr a) where
  debug value =
    Call0 . proc0 $
    match value
      (\case
        PUndefined ->
          ret $ debug Js.undefined
        PDefined a ->
          ret $ debug a
      )

instance (Typeable a, Debug a) => Debug (Maybe a) where
  debug value =
    Call0 . proc0 $
    match value
      (\case
        PNothing ->
          ret "Nothing"
        PJust a ->
          ret $ "Just(" <> debug a <> ")"
      )

instance Debug Text where
  debug = stringify

instance Debug Element where
  debug value = Call0 $ Prj value "toString"

instance Debug Node where
  debug value = Call0 $ Prj value "toString"

unwrap ::
  (Fallible a, Debug a, Typeable a, Typeable (Success a)) =>
  Expr a ->
  Statement r (Expr (Success a))
unwrap value = do
  result <- var_
  fallible value
    (\_ -> throw $ new @Error ("unwrap failed: unexpected " <> debug value))
    (\a -> result .= a)
  pure result
  
instance Typeable a => Fallible (NullOr a) where
  type Failure (NullOr a) = Null
  type Success (NullOr a) = a
  
  fallible e f s =
    match e
      (\case
        PNull -> do
          f Js.null
        PNonNull e' -> do
          s e'
      )
  
instance Typeable a => Fallible (UndefinedOr a) where
  type Failure (UndefinedOr a) = Undefined
  type Success (UndefinedOr a) = a
  
  fallible e f s =
    match e
      (\case
        PUndefined -> do
          f Js.undefined
        PDefined e' -> do
          s e'
      )
  
instance Typeable a => Fallible (Maybe a) where
  type Failure (Maybe a) = ()
  type Success (Maybe a) = a
  
  fallible e f s =
    match e
      (\case
        PNothing -> do
          f unit
        PJust e' -> do
          s e'
      )

unit :: Expr ()
unit = UnsafeObject []
