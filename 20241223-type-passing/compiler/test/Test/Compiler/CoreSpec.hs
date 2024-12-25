{-# LANGUAGE TemplateHaskell #-}

module Test.Compiler.CoreSpec where

import Compiler.Core (Multiplicity (..), Var (..), freeVarsExpr, toListFreeVars)
import Test.Compiler.TH (inlineCoreExpr)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "FreeVars" $ do
    it "begin call %0 : i32 = @f(%x : i32, %y : i32); %0 end" $ do
      let actual = freeVarsExpr Just $$(inlineCoreExpr "begin call %0 : i32 = @f(%x : i32, %y : i32); %0 end")
      let expected = [(VNamed "x", One), (VNamed "y", One)]
      toListFreeVars actual `shouldBe` expected
