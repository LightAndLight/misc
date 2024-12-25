module Compiler.Parse where

import Compiler.Kind (Kind (..))
import Compiler.Syntax
import Control.Applicative (Alternative ((<|>)), many, optional)
import Data.Text (Text)
import qualified Data.Vector as Vector
import Text.Parser.Char (alphaNum, char, lower)
import Text.Parser.Combinators (between)
import Text.Parser.Token
  ( IdentifierStyle (..)
  , TokenParsing
  , brackets
  , commaSep
  , integer
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
        <*> fmap Vector.fromList (many pattern)
        <* symbolic ':'
        <*> type_
        <* symbolic '='
        <*> expr
        <* symbolic ';'

pattern :: (TokenParsing m, Monad m) => m Pattern
pattern =
  symbolic '('
    *> ( PUnit <$ symbolic ')'
          <|> PVar <$ symbol "type" <*> fmap Just ident <* symbolic ':' <*> fmap Kind kind <* symbolic ')'
          <|> PVar <$> fmap Just ident <* symbolic ':' <*> fmap Type type_ <* symbolic ')'
       )

ident :: (TokenParsing m, Monad m) => m Text
ident =
  Text.Parser.Token.ident
    IdentifierStyle
      { _styleName = "ident"
      , _styleStart = lower <|> char '_'
      , _styleLetter = alphaNum <|> char '_'
      , _styleReserved =
          [ "type"
          , "begin"
          , "end"
          , "let"
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

type_ :: (TokenParsing m, Monad m) => m Type
type_ =
  forall_
    <|> exists
    <|> arrow
  where
    forall_ =
      uncurry TForall
        <$ symbol "forall"
        <*> parens ((,) <$> ident <* symbolic ':' <*> kind)
        <* symbolic '.'
        <*> type_

    exists =
      uncurry TExists
        <$ symbol "exists"
        <*> parens ((,) <$> ident <* symbolic ':' <*> kind)
        <* symbolic '.'
        <*> type_

    arrow = (\t -> maybe t (TArrow t)) <$> app <*> optional (symbol "->" *> type_)

    app =
      TArray <$ symbol "Array" <*> atom
        <|> atom

    atom =
      symbolic '(' *> (type_ <* symbolic ')' <|> TUnit <$ symbolic ')')
        <|> TI32 <$ symbol "i32"
        <|> TI64 <$ symbol "i64"
        <|> TBool <$ symbol "bool"
        <|> TVar . Src <$> ident

expr :: (TokenParsing m, Monad m) => m Expr
expr =
  ann
    <|> lam
    <|> block
  where
    lam =
      uncurry Lam
        <$ symbolic '\\'
        <*> ( parens ((,) IsType <$ symbol "type" <*> ident)
                <|> (,) IsValue <$> ident
            )
        <* symbol "->"
        <*> expr

    block =
      between (symbol "begin") (symbol "end") statements

    statements =
      let_
        <|> expr

    let_ =
      Let
        <$ symbol "let"
        <*> ident
        <*> optional (symbolic ':' *> type_)
        <* symbolic '='
        <*> expr
        <* symbolic ';'
        <*> statements

    ann = (\e -> maybe e (Ann e)) <$> app <*> optional (symbolic ':' *> type_)

    app = foldl App <$> proj <*> many arg

    arg =
      parens
        ( ArgType <$ symbol "type" <*> type_
            <|> ArgValue <$> expr
        )
        <|> ArgValue <$> proj

    proj = (\e -> maybe e (Proj e)) <$> atom <*> optional (symbolic '.' *> ident)

    atom =
      parens expr
        <|> Var . Src <$> ident
        <|> brackets (Array . Vector.fromList <$> commaSep expr)
        <|> Integer <$> integer
        <|> Bool True <$ symbol "true"
        <|> Bool False <$ symbol "false"
