module Parse where

import Syntax (Program (..), Definition(..), Expr (..), Binding (..), BExpr (..), Relation (..), Constant (..))
import Control.Applicative (many, optional, (<|>), empty)
import qualified Data.Vector as Vector
import Data.Text (Text)
import Text.Parser.Token (parens, TokenParsing, symbol, symbolic, token, brackets, braces, stringLiteral, natural)
import Text.Parser.Combinators (sepBy, try, (<?>))
import Text.Parser.Char (string, satisfy)
import qualified Data.Char as Char
import qualified Data.Text as Text
import Data.String (fromString)
import qualified Data.Map as Map

program :: (TokenParsing m, Monad m) => m Program
program =
  Program . Vector.fromList <$> many definition

definition :: (TokenParsing m, Monad m) => m Definition
definition =
  (\name params' mBody ->
    case mBody of
      Nothing ->
        error "TODO: parse facts"
      Just (body', bindings') ->
        Rule name params' body' bindings'
  ) <$>
    ident <*>
    parens params <*>
    optional ((,) <$ symbol ":-" <*> body <*> bindings) <* symbolic '.'
  where
    params = Vector.fromList <$> sepBy ident (symbolic ',')

    body =
      fmap Vector.fromList (sepBy relation $ symbolic ',')

    relation =
      Relation <$> ident <*> parens args

    args =
      Vector.fromList <$>
      sepBy expr (symbolic ',')

    bindings =
      Vector.fromList <$ symbol "where" <*>
      sepBy (Binding <$> ident <* symbolic '=' <*> bexpr) (symbolic ',') <|>
      pure Vector.empty

    bexpr =
      BKeys <$ string "keys" <*> parens expr <|>
      BItems <$ string "items" <*> parens expr

ident :: (TokenParsing m, Monad m) => m Text
ident =
  (token . try $ do
    i <- (\c cs -> Text.pack $ c : cs) <$> start <*> many continue
    if i `elem` keywords
      then empty
      else pure i
  ) <?> "identifier"
  where
    start = satisfy Char.isAlpha
    continue = satisfy Char.isAlphaNum
    keywords = [fromString "where", fromString "true", fromString "false"]

expr :: (TokenParsing m, Monad m) => m Expr
expr =
  Wild <$ symbolic '_' <|>
  Var <$> ident <|>
  Constant <$> csimple <|>
  braces
    (let
      loop =
        (\k v (rest, exact) -> ((k, v) : rest, exact)) <$>
          constant <* symbolic '=' <*>
          expr <*>
          (symbolic ',' *> loop <|> pure ([], True)) <|>
        ([], False) <$ symbol ".." <|>
        pure ([], True)
      in
        (\(items, exact) -> Map exact (Map.fromList items)) <$>
        loop
    ) <|>
  brackets (List . Vector.fromList <$> sepBy expr (symbolic ','))

constant :: TokenParsing m => m Constant
constant =
  (csimple <|>
    braces
      (CMap . Map.fromList <$>
        sepBy
          ((,) <$> constant <* symbolic '=' <*> constant)
          (symbolic ',')
      ) <|>
    brackets (CList . Vector.fromList <$> sepBy constant (symbolic ','))
  ) <?> "constant"

csimple :: TokenParsing m => m Constant
csimple =
  cbool <|>
    cstring <|>
    cnatural

cbool :: TokenParsing m => m Constant
cbool =
  fmap CBool $
  True <$ symbol "true" <|>
  False <$ symbol "false"

cstring :: TokenParsing m => m Constant
cstring = CString <$> stringLiteral

cnatural :: TokenParsing m => m Constant
cnatural = CNatural . fromIntegral <$> natural
