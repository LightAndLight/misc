module Compiler.Core.Parse where

import Compiler.Core
import Compiler.Kind (Kind (..))
import Compiler.Syntax (Unique (..))
import Control.Applicative (Alternative ((<|>)), many, optional, some)
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Text.Parser.Char (alphaNum, char, lower)
import Text.Parser.Combinators (between)
import Text.Parser.Token
  ( IdentifierStyle (..)
  , TokenParsing
  , commaSep
  , integer
  , natural
  , parens
  , symbol
  , symbolic
  )
import qualified Text.Parser.Token

module_ :: (TokenParsing m, Monad m) => m Module
module_ =
  Module . Vector.fromList <$> many definition

definition :: (TokenParsing m, Monad m) => m Definition
definition =
  binding
  where
    binding =
      Binding
        <$> ident
        <*> optional argTypes
        <* symbolic ':'
        <*> type_
        <* symbolic '='
        <*> expr
        <* symbolic ';'

ident :: (TokenParsing m, Monad m) => m Text
ident =
  Text.Parser.Token.ident
    IdentifierStyle
      { _styleName = "ident"
      , _styleStart = lower <|> char '_'
      , _styleLetter = alphaNum <|> char '_'
      , _styleReserved =
          [ "type"
          , "fn"
          , "begin"
          , "end"
          , "let"
          , "call"
          , "if"
          , "then"
          , "else"
          , "forall"
          , "exists"
          , "i32"
          , "i64"
          , "bool"
          , "Array"
          , "true"
          , "false"
          ]
      , _styleHighlight = undefined
      , _styleReservedHighlight = undefined
      }

typeOrKind :: (TokenParsing m, Monad m) => m TypeOrKind
typeOrKind =
  Kind <$> kind
    <|> Type <$> type_

kind :: (TokenParsing m, Monad m) => m Kind
kind =
  foldr KArrow <$> atom <*> many (symbol "->" *> atom)
  where
    atom = KType <$ symbol "Type"

var :: (TokenParsing m, Monad m) => m Var
var =
  localPrefix
    *> (VUnique . U . fromIntegral <$> natural <|> VNamed <$> ident)

type_ :: (TokenParsing m, Monad m) => m Type
type_ =
  fn
    <|> atom
  where
    fn =
      TFn
        <$ symbol "fn"
        <*> argTypes
        <* symbol "->"
        <*> type_

    atom =
      symbolic '(' *> (type_ <* symbolic ')' <|> TUnit <$ symbolic ')')
        <|> TArray <$ symbol "Array" <*> parens type_
        <|> TI32 <$ symbol "i32"
        <|> TI64 <$ symbol "i64"
        <|> TBool <$ symbol "bool"
        <|> TVar <$> var

expr :: (TokenParsing m, Monad m) => m Expr
expr =
  between (symbol "begin") (symbol "end") (Expr . Vector.fromList <$> some comp <*> value)
    <|> Expr mempty <$> value

localPrefix :: TokenParsing m => m Char
localPrefix = symbolic '%'

comp :: (TokenParsing m, Monad m) => m Comp
comp =
  let_
    <|> call
  where
    let_ =
      Let
        <$ symbol "let"
        <*> var
        <* symbolic ':'
        <*> type_
        <* symbolic '='
        <*> value
        <* symbolic ';'

    call =
      Call
        <$ symbol "call"
        <*> var
        <* symbolic ':'
        <*> type_
        <* symbolic '='
        <*> value
        <*> parens (Vector.fromList <$> commaSep arg)
        <* symbolic ';'

    arg =
      ArgType <$ symbol "type" <*> type_ <* symbolic ':' <*> kind
        <|> ArgValue <$> value <* symbolic ':' <*> type_

argTypes :: (TokenParsing m, Monad m) => m (Vector (Maybe Var, TypeOrKind))
argTypes =
  Vector.fromList
    <$> parens
      ( commaSep $
          (,) <$ symbol "type" <*> fmap Just var <* symbolic ':' <*> fmap Kind kind
            <|> (,) <$> fmap Just var <* symbolic ':' <*> fmap Type type_
            <|> (,) Nothing . Type <$> type_
      )

value :: (TokenParsing m, Monad m) => m Value
value =
  lam
    <|> proj
  where
    lam =
      Lam
        <$ symbolic '\\'
        <*> argTypes
        <* symbol "->"
        <*> expr

    proj = (\e -> maybe e (Proj e)) <$> atom <*> optional (symbolic '.' *> ident)

    global =
      Name <$ symbolic '@' <*> ident

    atom =
      global
        <|> fmap Var var
        <|> (\i f -> f i) <$> integer <* symbolic '@' <*> (mkI32 <$ symbol "i32" <|> mkI64 <$ symbol "i64")
        <|> Bool True <$ symbol "true"
        <|> Bool False <$ symbol "false"

    mkI32 = I32 . fromIntegral
    mkI64 = I64 . fromIntegral
