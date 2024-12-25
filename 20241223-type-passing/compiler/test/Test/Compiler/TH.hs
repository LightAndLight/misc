{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Test.Compiler.TH where

import qualified Compiler.Core as Core
import qualified Compiler.Core.Parse as Core.Parse
import Compiler.Kind (Kind (..))
import qualified Compiler.Parse as Parse
import Compiler.Syntax
import Data.String (fromString)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Language.Haskell.TH (CodeQ, unsafeCodeCoerce)
import Language.Haskell.TH.Syntax (Lift (..))
import Streaming.Chars.Text (StreamText (..))
import Text.Parser.Combinators (eof)
import Text.Sage (parse)

deriving instance Lift Unique
deriving instance Lift Var
instance Lift a => Lift (Vector a) where
  lift xs =
    let xs' = Vector.toList xs
    in [|Vector.fromList xs'|]
deriving instance Lift Type
deriving instance Lift TypeOrKind
deriving instance Lift Kind
deriving instance Lift IsValueOrType
deriving instance Lift Argument
deriving instance Lift Expr
deriving instance Lift Pattern
deriving instance Lift Definition

deriving instance Lift Core.Var
deriving instance Lift Core.Type
deriving instance Lift Core.TypeOrKind
deriving instance Lift Core.Value
deriving instance Lift Core.Argument
deriving instance Lift Core.Comp
deriving instance Lift a => Lift (Core.Expr' a)

inlineType :: String -> CodeQ Type
inlineType input =
  unsafeCodeCoerce $
    case parse (Parse.type_ <* eof) . StreamText $ fromString input of
      Left err -> error $ show err
      Right a -> lift a

inlineCoreType :: String -> CodeQ Core.Type
inlineCoreType input =
  unsafeCodeCoerce $
    case parse (Core.Parse.type_ <* eof) . StreamText $ fromString input of
      Left err -> error $ show err
      Right a -> lift a

inlineExpr :: String -> CodeQ Expr
inlineExpr input =
  unsafeCodeCoerce $
    case parse (Parse.expr <* eof) . StreamText $ fromString input of
      Left err -> error $ show err
      Right a -> lift a

inlineCoreExpr :: String -> CodeQ Core.Expr
inlineCoreExpr input =
  unsafeCodeCoerce $
    case parse (Core.Parse.expr <* eof) . StreamText $ fromString input of
      Left err -> error $ show err
      Right a -> lift a

inlineDefinition :: String -> CodeQ Definition
inlineDefinition input =
  unsafeCodeCoerce $
    case parse (Parse.definition <* eof) . StreamText $ fromString input of
      Left err -> error $ show err
      Right a -> lift a
