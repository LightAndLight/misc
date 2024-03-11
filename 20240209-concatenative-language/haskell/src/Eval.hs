{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Eval where

import Data.List (intercalate)
import Data.Text (Text)
import qualified Data.Text as Text
import Lib

data Values (ctx :: Ctx) where
  VNil :: Values Nil
  VSnoc :: Values ctx -> Value a -> Values (ctx :. a)

getValue :: Index ctx a -> Values ctx -> Value a
getValue Z (VSnoc _ a) = a
getValue (S ix) (VSnoc ctx _) = getValue ix ctx

newtype Eval (ctx :: Ctx) (ctx' :: Ctx) = Eval {unEval :: Values ctx -> Values ctx'}

instance Cat Eval where
  id = Eval (\x -> x)

  compose (Eval f) (Eval g) = Eval (\x -> g (f x))

  var ix = Eval $ \ctx -> VSnoc VNil (getValue ix ctx)

  drop = Eval $ \(VSnoc ctx _) -> ctx

  fix f = Eval $ \ctx -> unEval (f (fix f)) ctx

  bind f = Eval $ \(VSnoc ctx a) -> unEval (f (Eval $ \ctx' -> VSnoc ctx' a)) ctx

  fn f = Eval $ \ctx -> VSnoc ctx (VClosure ctx f)

  app = Eval $ \(VSnoc (VSnoc ctx (VClosure env f)) x) -> let VSnoc VNil b = unEval f (VSnoc env x) in VSnoc ctx b

  inl = Eval $ \(VSnoc ctx a) -> VSnoc ctx (VLeft a)

  inr = Eval $ \(VSnoc ctx b) -> VSnoc ctx (VRight b)

  matchSum l r = Eval $ \(VSnoc ctx s) ->
    case s of
      VLeft a -> unEval l (VSnoc ctx a)
      VRight b -> unEval r (VSnoc ctx b)

  pair = Eval $ \(VSnoc (VSnoc ctx a) b) -> VSnoc ctx (VPair a b)

  unpair = Eval $ \(VSnoc ctx (VPair a b)) -> VSnoc (VSnoc ctx a) b

  par f g = Eval $ \ctx ->
    let
      a = unEval f ctx
      VSnoc VNil b = unEval g ctx
     in
      VSnoc a b

  true = Eval $ \ctx -> VSnoc ctx (VBool True)

  false = Eval $ \ctx -> VSnoc ctx (VBool False)

  ifte f g = Eval $ \(VSnoc ctx (VBool b)) -> if b then unEval f ctx else unEval g ctx

  char c = Eval $ \ctx -> VSnoc ctx (VChar c)

  matchChar fs g = Eval $ \(VSnoc ctx (VChar c)) ->
    Prelude.foldr
      (\(c', f) rest -> if c == c' then unEval f ctx else rest)
      (unEval g ctx)
      fs

  eqChar = Eval $ \(VSnoc (VSnoc ctx (VChar a)) (VChar b)) -> VSnoc ctx (VBool $ a == b)
  string s = Eval $ \ctx -> VSnoc ctx (VString s)
  uncons = Eval $ \(VSnoc ctx (VString s)) ->
    case Text.uncons s of
      Nothing -> VSnoc ctx VNothing
      Just (c, cs) -> VSnoc ctx (VJust (VPair (VString cs) (VChar c)))
  consString = Eval $ \(VSnoc (VSnoc ctx (VString cs)) (VChar c)) -> VSnoc ctx (VString $ Text.cons c cs)
  int i = Eval $ \ctx -> VSnoc ctx (VInt i)
  add = Eval $ \(VSnoc (VSnoc ctx (VInt a)) (VInt b)) -> VSnoc ctx (VInt $ a + b)
  mul = Eval $ \(VSnoc (VSnoc ctx (VInt a)) (VInt b)) -> VSnoc ctx (VInt $ a * b)
  nothing = Eval $ \ctx -> VSnoc ctx VNothing
  just = Eval $ \(VSnoc ctx a) -> VSnoc ctx (VJust a)
  matchMaybe f g = Eval $ \(VSnoc ctx a) ->
    case a of
      VNothing -> unEval f ctx
      VJust x -> unEval g (VSnoc ctx x)
  nil = Eval $ \ctx -> VSnoc ctx VListNil
  cons = Eval $ \(VSnoc (VSnoc ctx xs) x) -> VSnoc ctx (VListCons x xs)
  matchList f g = Eval $ \(VSnoc ctx xs) ->
    case xs of
      VListNil -> unEval f ctx
      VListCons a as -> unEval g (VSnoc (VSnoc ctx as) a)

eval :: Eval ctx ctx' -> Values ctx -> Values ctx'
eval = unEval

printValue :: Value ty -> String
printValue v =
  case v of
    VInt n -> show n
    VBool b -> if b then "true" else "false"
    VChar c -> show c
    VString s -> show s
    VClosure{} -> "<<function>>"
    VLeft v' -> "inr(" <> printValue v' <> ")"
    VRight v' -> "inl(" <> printValue v' <> ")"
    VPair a b -> "pair(" <> printValue a <> ", " <> printValue b <> ")"
    VNothing -> "nothing"
    VJust a -> "just(" <> printValue a <> ")"
    VListNil -> printList v
    VListCons{} -> printList v

printList :: Value (TList a) -> String
printList = ("[" <>) Prelude.. (<> "]") Prelude.. intercalate ", " Prelude.. go
 where
  go :: Value (TList a) -> [String]
  go VListNil = []
  go (VListCons a as) = printValue a : go as

printValues :: Values ctx -> String
printValues = intercalate ", " Prelude.. go
 where
  go :: Values ctx -> [String]
  go VNil = []
  go (VSnoc vs v) = go vs <> [printValue v]

data Value (ty :: Ty) where
  VInt :: Int -> Value TInt
  VChar :: Char -> Value TChar
  VString :: Text -> Value TString
  VBool :: Bool -> Value TBool
  VClosure :: Values ctx -> Eval (ctx :. a) (Nil :. b) -> Value (TExp b a)
  VLeft :: Value a -> Value (TSum a b)
  VRight :: Value b -> Value (TSum a b)
  VPair :: Value a -> Value b -> Value (TProd a b)
  VNothing :: Value (TMaybe a)
  VJust :: Value a -> Value (TMaybe a)
  VListNil :: Value (TList a)
  VListCons :: Value a -> Value (TList a) -> Value (TList a)

class Reflect a where
  type ReflectTy a :: Ty
  reflect :: a -> Value (ReflectTy a)

instance Reflect Text where
  type ReflectTy Text = TString
  reflect = VString

instance Reflect Int where
  type ReflectTy Int = TInt
  reflect = VInt

instance Reflect Bool where
  type ReflectTy Bool = TBool
  reflect = VBool

instance (Reflect a) => Reflect (Maybe a) where
  type ReflectTy (Maybe a) = TMaybe (ReflectTy a)
  reflect Nothing = VNothing
  reflect (Just a) = VJust (reflect a)