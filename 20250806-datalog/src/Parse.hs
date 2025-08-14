module Parse where

import Control.Applicative (empty, many, optional, (<|>))
import qualified Data.Char as Char
import qualified Data.Map as Map
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import Syntax
  ( LBExpr (..)
  , LocalBinding (..)
  , Constant (..)
  , Definition (..)
  , Expr (..)
  , Program (..)
  , Relation (..), BExpr (..), Stream (..)
  )
import Text.Parser.Char (satisfy, string)
import Text.Parser.Combinators (sepBy, try, (<?>), eof)
import Text.Parser.Token
  ( TokenParsing
  , braces
  , brackets
  , natural
  , parens
  , stringLiteral
  , symbol
  , symbolic
  , token
  )

program :: (TokenParsing m, Monad m) => m Program
program =
  Program . Vector.fromList <$> many definition <* eof

definition :: (TokenParsing m, Monad m) => m Definition
definition =
  ( \name rest ->
    case rest of
      Left (params', mBody) ->
        case mBody of
          Nothing ->
            error "TODO: parse facts"
          Just (body', bindings') ->
            Rule name params' body' bindings'
      Right body' ->
        Binding name body'
  )
    <$> ident
    <*> (Left <$> rulePart <|> Right <$> bindingPart)
    <* symbolic '.'
  where
    rulePart =
      (,)
      <$> parens params
      <*> optional ((,) <$ symbol ":-" <*> body <*> bindings)

    bindingPart =
      symbolic '=' *> bexpr

    bexpr =
      uncurry BAggregate <$ string "aggregate" <*>
        parens ((,) <$> ident <* symbolic ',' <*> stream)

    stream =
      brackets $
      Stream <$>
        parens (Vector.fromList <$> sepBy expr (symbolic ',')) <* symbol "of" <*>
        relation

    params = Vector.fromList <$> sepBy ident (symbolic ',')

    body =
      fmap Vector.fromList (sepBy relation $ symbolic ',')

    relation =
      Relation <$> ident <*> parens args

    args =
      Vector.fromList
        <$> sepBy expr (symbolic ',')

    bindings =
      Vector.fromList
        <$ symbol "where"
        <*> sepBy (LocalBinding <$> ident <* symbolic '=' <*> lbexpr) (symbolic ',')
        <|> pure Vector.empty

    lbexpr =
      LBKeys <$ string "keys" <*> parens expr
        <|> LBItems <$ string "items" <*> parens expr

ident :: (TokenParsing m, Monad m) => m Text
ident =
  ( token . try $ do
      i <- (\c cs -> Text.pack $ c : cs) <$> start <*> many continue
      if i `elem` keywords
        then empty
        else pure i
  )
    <?> "identifier"
  where
    start = satisfy Char.isAlpha
    continue = satisfy Char.isAlphaNum
    keywords =
      [ fromString "where"
      , fromString "true"
      , fromString "false"
      , fromString "of"
      ]

expr :: (TokenParsing m, Monad m) => m Expr
expr =
  Wild <$ symbolic '_'
    <|> Var <$> ident
    <|> Constant <$> csimple
    <|> braces
      ( let
          loop =
            (\k v (rest, exact) -> ((k, v) : rest, exact))
              <$> constant
              <* symbolic '='
              <*> expr
              <*> (symbolic ',' *> loop <|> pure ([], True))
              <|> ([], False) <$ symbol ".."
              <|> pure ([], True)
        in
          (\(items, exact) -> Map exact (Map.fromList items))
            <$> loop
      )
    <|> brackets (List . Vector.fromList <$> sepBy expr (symbolic ','))

constant :: TokenParsing m => m Constant
constant =
  ( csimple
      <|> braces
        ( CMap . Map.fromList
            <$> sepBy
              ((,) <$> constant <* symbolic '=' <*> constant)
              (symbolic ',')
        )
      <|> brackets (CList . Vector.fromList <$> sepBy constant (symbolic ','))
  )
    <?> "constant"

csimple :: TokenParsing m => m Constant
csimple =
  cbool
    <|> cstring
    <|> cnatural

cbool :: TokenParsing m => m Constant
cbool =
  fmap CBool $
    True <$ symbol "true"
      <|> False <$ symbol "false"

cstring :: TokenParsing m => m Constant
cstring = CString <$> stringLiteral

cnatural :: TokenParsing m => m Constant
cnatural = CNatural . fromIntegral <$> natural
