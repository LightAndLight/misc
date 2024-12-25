{-# LANGUAGE TemplateHaskell #-}

module Test.Compiler.ParseSpec where

import Compiler.Parse (expr, type_)
import Compiler.Syntax
import Streaming.Chars.Text (StreamText (..))
import Test.Compiler.TH (inlineExpr)
import Test.Hspec (Spec, it, shouldBe)
import Text.Parser.Combinators (eof)
import Text.Sage (parse)

spec :: Spec
spec = do
  it "\\(type a) x -> x" $ do
    let expected = Right $ Lam IsType "a" $ Lam IsValue "x" $ Var $ Src "x"
    let actual = parse (expr <* eof) $ StreamText "\\(type a) -> \\x -> x"
    actual `shouldBe` expected

  it "i32 -> i32 -> i32" $ do
    let expected = Right $ TArrow TI32 (TArrow TI32 TI32)
    let actual = parse (type_ <* eof) $ StreamText "i32 -> i32 -> i32"
    actual `shouldBe` expected

  it "f x y" $ do
    let expected = Right $ App (App (Var $ Src "f") (ArgValue . Var $ Src "x")) (ArgValue . Var $ Src "y")
    let actual = parse (expr <* eof) $ StreamText "f x y"
    actual `shouldBe` expected

  it "\\f -> \\x -> f x 1" $ do
    let actual = $$(inlineExpr "\\f -> \\x -> f x 1")
    let expected =
          Lam IsValue "f" $
            Lam IsValue "x" $
              App (App (Var $ Src "f") (ArgValue . Var $ Src "x")) (ArgValue $ Integer 1)
    actual `shouldBe` expected
