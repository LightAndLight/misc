{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Test.Compiler.CodegenSpec where

import Compiler.Check (checkDefinition, checkExpr, runCheckT)
import qualified Compiler.Check as Check
import Compiler.Codegen
import Compiler.Core (Definition, Expr, Type (..), TypeOrKind (..), Var (..))
import Compiler.Fresh (runFreshT)
import qualified Compiler.Syntax as Syntax
import Data.Functor.Identity (runIdentity)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Text (Text)
import qualified Data.Text.Lazy as Lazy
import Data.Text.Lazy.Builder (Builder)
import Test.Compiler.TH (inlineCoreExpr, inlineCoreType, inlineDefinition, inlineExpr)
import Test.Hspec (Spec, it, shouldBe)

import Compiler.Codegen.Builder

checkedExpr ::
  HashMap Text Type ->
  [(Var, (TypeOrKind, a))] ->
  Type ->
  Syntax.Expr ->
  Expr
checkedExpr ctx ls ty expr =
  let
    result =
      runIdentity
        . runCheckT
        . Check.withGlobals ctx
        . Check.withLocals (HashMap.fromList $ fmap (\(a, (b, _)) -> (a, b)) ls)
        $ checkExpr ty expr
  in
    case result of
      Left err -> error $ show err
      Right a -> a

checkedDefinition ::
  HashMap Text Type ->
  Syntax.Definition ->
  Definition
checkedDefinition ctx definition =
  let
    result =
      runIdentity
        . runCheckT
        . Check.withGlobals ctx
        $ checkDefinition definition
  in
    case result of
      Left err -> error $ show err
      Right a -> a

codegenExpr ::
  HashMap Text Type ->
  [(Var, (TypeOrKind, Builder))] ->
  Expr ->
  (Lazy.Text, Lazy.Text, ())
codegenExpr ctx ls =
  runIdentity
    . runFreshT
    . printBlockT
    . genExpr env "test_result"
  where
    env =
      withLocals
        (fmap (\(a, (b, c)) -> (a, b, c)) ls)
        defaultEnv{globals = ctx}

codegenDefinition :: HashMap Text Type -> Definition -> (Lazy.Text, ())
codegenDefinition ctx =
  runIdentity . runFreshT . runModuleT . genDefinition defaultEnv{globals = ctx}

printBlockT :: Monad m => BlockT m a -> m (Lazy.Text, Lazy.Text, a)
printBlockT ma = do
  (m, sa) <- runModuleT $ runBlockT 0 ma
  (s, a) <- runSectionT sa
  pure (m, s, a)

spec :: Spec
spec = do
  it "begin call %0 : i32 = @f(%x : i32, %y : i32); %0 end" $ do
    let
      ctx = [("f", $$(inlineCoreType "fn(i32, i32) -> i32"))]
      ls =
        [ (VNamed "x", (Type TI32, "my_x"))
        , (VNamed "y", (Type TI32, "my_y"))
        ]
      actual =
        codegenExpr
          ctx
          ls
          $$( inlineCoreExpr $
                unlines
                  [ "begin"
                  , "  call %0 : i32 = @f(%x : i32, %y : i32);"
                  , "  %0"
                  , "end"
                  ]
            )

    let
      expected =
        ( mempty
        , foldMap @[]
            (<> "\n")
            [ "int32_t arg_0;"
            , "Type_move(&Type_i32, &arg_0, &my_x);"
            , "int32_t arg_1;"
            , "Type_move(&Type_i32, &arg_1, &my_y);"
            , "int32_t v_0 = f(arg_0, arg_1);"
            , "Type_move(&Type_i32, &test_result, &v_0);"
            ]
        , ()
        )

    actual `shouldBe` expected

  it "begin call %0 : i32 = @f(%x : i32, %y : i32, %x : i32); %0 end" $ do
    let
      ctx = [("f", $$(inlineCoreType "fn(i32, i32, i32) -> i32"))]
      ls =
        [ (VNamed "x", (Type TI32, "my_x"))
        , (VNamed "y", (Type TI32, "my_y"))
        ]
      actual =
        codegenExpr
          ctx
          ls
          $$( inlineCoreExpr $
                unlines
                  [ "begin"
                  , "  call %0 : i32 = @f(%x : i32, %y : i32, %x : i32);"
                  , "  %0"
                  , "end"
                  ]
            )

    let
      expected =
        ( mempty
        , foldMap @[]
            (<> "\n")
            [ "int32_t arg_0;"
            , "Type_copy(&Type_i32, &arg_0, &my_x);"
            , "int32_t arg_1;"
            , "Type_move(&Type_i32, &arg_1, &my_y);"
            , "int32_t arg_2;"
            , "Type_move(&Type_i32, &arg_2, &my_x);"
            , "int32_t v_0 = f(arg_0, arg_1, arg_2);"
            , "Type_move(&Type_i32, &test_result, &v_0);"
            ]
        , ()
        )

    actual `shouldBe` expected

  it "begin call %0 : i32 = @f(%x : i32, %y : i32); call %1 : i32 = @g(%x : i32, %z : i32); %1 end" $ do
    let
      ctx =
        [ ("f", $$(inlineCoreType "fn(i32, i32) -> i32"))
        , ("g", $$(inlineCoreType "fn(i32, i32) -> i32"))
        ]

      ls =
        [ (VNamed "x", (Type TI32, "my_x"))
        , (VNamed "y", (Type TI32, "my_y"))
        , (VNamed "z", (Type TI32, "my_z"))
        ]

      actual =
        codegenExpr
          ctx
          ls
          $$( inlineCoreExpr $
                unlines
                  [ "begin"
                  , "  call %0 : i32 = @f(%x : i32, %y : i32);"
                  , "  call %1 : i32 = @g(%x : i32, %z : i32);"
                  , "  %1"
                  , "end"
                  ]
            )

    let
      expected =
        ( mempty
        , foldMap @[]
            (<> "\n")
            [ "int32_t arg_0;"
            , "Type_copy(&Type_i32, &arg_0, &my_x);"
            , "int32_t arg_1;"
            , "Type_move(&Type_i32, &arg_1, &my_y);"
            , "int32_t v_0 = f(arg_0, arg_1);"
            , "int32_t arg_2;"
            , "Type_move(&Type_i32, &arg_2, &my_x);"
            , "int32_t arg_3;"
            , "Type_move(&Type_i32, &arg_3, &my_z);"
            , "int32_t v_1 = g(arg_2, arg_3);"
            , "Type_move(&Type_i32, &test_result, &v_1);"
            ]
        , ()
        )

    actual `shouldBe` expected

  it "f : i32 -> i32 -> i32, x : i32 |- f x : i32" $ do
    let
      ctx = [("f", TFn [(Nothing, Type TI32), (Nothing, Type TI32)] TI32)]
      ls = [(VNamed "x", (Type TI32, "my_x"))]
      actual =
        codegenExpr ctx ls $
          checkedExpr ctx ls $$(inlineCoreType "fn(i32) -> i32") $$(inlineExpr "f x")

    let
      expected =
        ( foldMap @[]
            (<> "\n")
            [ "typedef struct { FP_Closure_apply apply; int32_t l_x; } Closure_0;"
            , ""
            , "int32_t closure_code_1(const Closure_0* self_2, int32_t v_0) {"
            , "  int32_t result_3;"
            , "  int32_t arg_4;"
            , "  Type_move(&Type_i32, &arg_4, &self_2->l_x);"
            , "  int32_t arg_5;"
            , "  Type_move(&Type_i32, &arg_5, &v_0);"
            , "  int32_t v_1 = f(arg_4, arg_5);"
            , "  Type_move(&Type_i32, &result_3, &v_1);"
            , "  return result_3;"
            , "}"
            ]
        , foldMap @[]
            (<> "\n")
            [ "test_result = Rc_alloc(alignof(Closure_0), sizeof(Closure_0));"
            , "{"
            , "  Closure_0* data_6 = Rc_data(test_result);"
            , "  data_6->apply = ((FP_Closure_apply)&closure_code_1);"
            , "  Type_move(&Type_i32, &data_6->l_x, &my_x);"
            , "}"
            ]
        , ()
        )

    actual `shouldBe` expected

  it "id (type a : Type) (x : a) : a = x;" $ do
    let
      ctx = []
      actual =
        codegenDefinition ctx $
          checkedDefinition ctx $$(inlineDefinition "id (type a : Type) (x : a) : a = x;")

    let
      expected =
        ( foldMap @[]
            (<> "\n")
            [ "void id(void* result_0, const Type* l_a, void* l_x) {"
            , "  Type_move(l_a, result_0, l_x);"
            , "}"
            ]
        , ()
        )

    actual `shouldBe` expected

  it "id : forall (a : Type). a -> a |- id (type bool) true : bool" $ do
    let
      ctx = [("id", $$(inlineCoreType "fn(type %a : Type, %x : %a) -> %a"))]
      ls = []
      actual =
        codegenExpr ctx ls $
          checkedExpr ctx ls $$(inlineCoreType "bool") $$(inlineExpr "id (type bool) true")

    let
      expected =
        ( mempty
        , foldMap @[]
            (<> "\n")
            [ "const Type* arg_0;"
            , "arg_0 = &Type_bool;"
            , "bool arg_1;"
            , "arg_1 = true;"
            , "void* v_0 = aligned_alloca(Type_alignment(&Type_bool), Type_size(&Type_bool));"
            , "id(v_0, arg_0, &arg_1);"
            , "Type_move(&Type_bool, &test_result, &*v_0);"
            ]
        , ()
        )

    actual `shouldBe` expected

  it "[1, 2, 3, 4, 5] : Array i32" $ do
    let
      ctx = []
      ls = []
      actual =
        codegenExpr ctx ls $
          checkedExpr ctx ls $$(inlineCoreType "Array(i32)") $$(inlineExpr "[1:i32, 2, 3, 4, 5]")

    let
      expected =
        ( mempty
        , foldMap @[]
            (<> "\n")
            [ "test_result = Array_alloc(&Type_i32, 5);"
            , "{"
            , "  char* data_0 = Rc_data(test_result.data);"
            , "  data_0[0] = 1;"
            , "  data_0[4] = 2;"
            , "  data_0[8] = 3;"
            , "  data_0[12] = 4;"
            , "  data_0[16] = 5;"
            , "}"
            ]
        , ()
        )

    actual `shouldBe` expected
