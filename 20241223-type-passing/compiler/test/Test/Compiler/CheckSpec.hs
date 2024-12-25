{-# LANGUAGE TemplateHaskell #-}

module Test.Compiler.CheckSpec where

import Compiler.Check (runCheckT, withGlobals)
import qualified Compiler.Check as Check
import qualified Compiler.Core as Core
import Compiler.Kind (Kind (..))
import Compiler.Syntax
import Data.Functor.Identity (runIdentity)
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Test.Compiler.TH (inlineCoreExpr, inlineCoreType, inlineExpr)
import Test.Hspec (Spec, it, shouldBe)

checkExpr ::
  HashMap Text Core.Type ->
  Core.Type ->
  Expr ->
  Either Check.CheckError Core.Expr
checkExpr ctx ty e =
  runIdentity . runCheckT . withGlobals ctx $ Check.checkExpr ty e

inferExpr ::
  HashMap Text Core.Type ->
  Expr ->
  Either Check.CheckError (Core.Type, Core.Expr)
inferExpr ctx e =
  runIdentity . runCheckT . withGlobals ctx $ Check.inferExpr e

checkType :: Kind -> Type -> Either Check.CheckError Core.Type
checkType ki ty =
  runIdentity . runCheckT $ Check.checkType ki ty

spec :: Spec
spec = do
  it "1 : i32" $ do
    let expected = Right . pure $ Core.I32 1
    let actual = checkExpr mempty Core.TI32 $$(inlineExpr "1")
    actual `shouldBe` expected

  it "1 : i64" $ do
    let expected = Right . pure $ Core.I64 1
    let actual = checkExpr mempty Core.TI64 $$(inlineExpr "1")
    actual `shouldBe` expected

  it "(\\(type a) -> \\x -> a) : forall (a : Type). a -> a" $ do
    let
      expected =
        Right $$(inlineCoreExpr "\\(type %a : Type, %x : %a) -> %x")

      actual =
        checkExpr
          mempty
          $$(inlineCoreType "fn(type %a : Type, %x : %a) -> %a")
          $$(inlineExpr "\\(type a) -> \\x -> x")
    actual `shouldBe` expected

  it "(\\f -> \\x -> f x) : (i32 -> i32) -> i32 -> i32" $ do
    let
      expected =
        Right
          $$( inlineCoreExpr $
                unlines
                  [ "\\(%f : fn(i32) -> i32, %x : i32) ->"
                  , "begin"
                  , "  call %0 : i32 = %f(%x : i32);"
                  , "  %0"
                  , "end"
                  ]
            )

      actual =
        checkExpr
          mempty
          $$(inlineCoreType "fn(fn(i32) -> i32, i32) -> i32")
          $$(inlineExpr "\\f -> \\x -> f x")

    actual `shouldBe` expected

  it "(\\f -> \\x -> f x 1) : (i32 -> i32 -> i32) -> i32 -> i32" $ do
    let
      expected =
        Right
          $$( inlineCoreExpr $
                unlines
                  [ "\\(%f : fn(i32, i32) -> i32, %x : i32) ->"
                  , "begin"
                  , "  call %0 : i32 = %f(%x : i32, 1@i32 : i32);"
                  , "  %0"
                  , "end"
                  ]
            )

      actual =
        checkExpr
          mempty
          $$(inlineCoreType "fn(fn(i32, i32) -> i32, i32) -> i32")
          $$(inlineExpr "\\f -> \\x -> f x 1")

    actual `shouldBe` expected

  it "f : i32 -> i32 -> i32 |- f 5 : i32 -> i32" $ do
    let
      expected =
        Right
          $$( inlineCoreExpr $
                unlines
                  [ "\\(%0 : i32) ->"
                  , "begin"
                  , "  call %1 : i32 = @f(5@i32 : i32, %0 : i32);"
                  , "  %1"
                  , "end"
                  ]
            )

      actual =
        checkExpr
          [("f", $$(inlineCoreType "fn(i32, i32) -> i32"))]
          $$(inlineCoreType "fn(i32) -> i32")
          $$(inlineExpr "f 5")

    actual `shouldBe` expected

  it "f : i32 -> i32 -> i32, x : i32, y : i32 |- f x y : i32 (check)" $ do
    let
      expected =
        Right
          $$( inlineCoreExpr $
                unlines
                  [ "begin"
                  , "  call %0 : i32 = @f(@x : i32, @y : i32);"
                  , "  %0"
                  , "end"
                  ]
            )

      actual =
        checkExpr
          [ ("f", $$(inlineCoreType "fn(i32, i32) -> i32"))
          , ("x", $$(inlineCoreType "i32"))
          , ("y", $$(inlineCoreType "i32"))
          ]
          $$(inlineCoreType "i32")
          $$(inlineExpr "f x y")

    actual `shouldBe` expected

  it "f : i32 -> i32 -> i32, x : i32, y : i32 |- f x y : i32 (infer)" $ do
    let
      expected =
        Right
          ( $$(inlineCoreType "i32")
          , $$( inlineCoreExpr $
                  unlines
                    [ "begin"
                    , "  call %0 : i32 = @f(@x : i32, @y : i32);"
                    , "  %0"
                    , "end"
                    ]
              )
          )

      actual =
        inferExpr
          [ ("f", $$(inlineCoreType "fn(i32, i32) -> i32"))
          , ("x", $$(inlineCoreType "i32"))
          , ("y", $$(inlineCoreType "i32"))
          ]
          $$(inlineExpr "f x y")

    actual `shouldBe` expected

  it "abort : forall (a : Type). a |- abort (i32 -> i32 -> i32) : i32 -> i32 -> i32" $ do
    let
      expected =
        Right
          $$( inlineCoreExpr $
                unlines
                  [ "begin"
                  , "  call %2 : fn(i32, i32) -> i32 = @abort(type fn(i32, i32) -> i32 : Type);"
                  , "  %2"
                  , "end"
                  ]
            )

      actual =
        checkExpr
          [("abort", $$(inlineCoreType "fn(type %a : Type) -> %a"))]
          $$(inlineCoreType "fn(i32, i32) -> i32")
          $$(inlineExpr "abort (type i32 -> i32 -> i32)")

    actual `shouldBe` expected

  it "abort : forall (a : Type). a |- abort (i32 -> i32 -> i32) 5 : i32 -> i32" $ do
    let
      expected =
        Right
          $$( inlineCoreExpr $
                unlines
                  [ "begin"
                  , "  call %2 : fn(i32, i32) -> i32 = @abort(type fn(i32, i32) -> i32 : Type);"
                  , "  \\(%3 : i32) ->"
                  , "  begin"
                  , "    call %4 : i32 = %2(5@i32 : i32, %3 : i32);"
                  , "    %4"
                  , "  end"
                  , "end"
                  ]
            )

      actual =
        checkExpr
          [("abort", $$(inlineCoreType "fn(type %a : Type) -> %a"))]
          $$(inlineCoreType "fn(i32) -> i32")
          $$(inlineExpr "abort (type i32 -> i32 -> i32) 5")

    actual `shouldBe` expected

  it
    "abort : forall (a : Type). a |- (let f : (i32 -> i32) -> i32 = \\_ -> 1 in f (abort (i32 -> i32 -> i32) 5)) : i32"
    $ do
      let
        expected =
          Right
            $$( inlineCoreExpr $
                  unlines
                    [ "begin"
                    , "  let %f : fn(fn(i32) -> i32) -> i32 = \\(%g : fn(i32) -> i32) -> 1@i32;"
                    , "  call %4 : fn(i32, i32) -> i32 = @abort(type fn(i32, i32) -> i32 : Type);"
                    , "  call %9 : i32 = %f("
                    , "    \\(%5 : i32) ->"
                    , "    begin"
                    , "      call %6 : i32 = %4(5@i32 : i32, %5 : i32);"
                    , "      %6"
                    , "    end"
                    , "    : fn(i32) -> i32"
                    , "  );"
                    , "  %9"
                    , "end"
                    ]
              )

        actual =
          checkExpr
            [("abort", $$(inlineCoreType "fn(type %a : Type) -> %a"))]
            $$(inlineCoreType "i32")
            $$( inlineExpr $
                  unlines
                    [ "begin"
                    , "  let f : (i32 -> i32) -> i32 = \\g -> 1;"
                    , "  f (abort (type i32 -> i32 -> i32) 5)"
                    , "end"
                    ]
              )

      actual `shouldBe` expected

  it
    "id : forall (a : Type). a -> a |- (id (type i32 -> i32) (id (type i32)) 100) : i32"
    $ do
      let
        expected =
          Right
            $$( inlineCoreExpr $
                  unlines
                    [ "begin"
                    , "  call %4 : fn(i32) -> i32 = @id("
                    , "    type fn(i32) -> i32 : Type,"
                    , "    \\(%x : i32) ->"
                    , "    begin"
                    , "      call %1 : i32 = @id(type i32 : Type, %x : i32);"
                    , "      %1"
                    , "    end"
                    , "    : fn(i32) -> i32"
                    , "  );"
                    , "  call %5 : i32 = %4(100@i32 : i32);"
                    , "  %5"
                    , "end"
                    ]
              )

        actual =
          checkExpr
            [("id", $$(inlineCoreType "fn(type %a : Type, %x : %a) -> %a"))]
            $$(inlineCoreType "i32")
            $$(inlineExpr "id (type i32 -> i32) (id (type i32)) 100")

      actual `shouldBe` expected

  it "id : forall (a : Type). a -> a |- id (type bool) true : bool" $ do
    let
      ctx = [("id", $$(inlineCoreType "fn(type %a : Type, %x : %a) -> %a"))]
      actual = checkExpr ctx $$(inlineCoreType "bool") $$(inlineExpr "id (type bool) true")

    let
      expected =
        Right $$(inlineCoreExpr "begin call %0 : bool = @id(type bool : Type, true : bool); %0 end")

    actual `shouldBe` expected
