module Eval (
  -- * Evaluation
  Eval,
  eval,

  -- * Values
  Value (..),
  printValue,
  Values (..),
  printValues,
  Reflect (..),
) where

import Data.List (intercalate)
import Data.Text (Text)
import qualified Data.Text as Text
import Lib
import Lib.Ty

data Values (ctx :: [Ty]) where
  VNil :: Values '[]
  VCons :: Value a -> Values ctx -> Values (a ': ctx)

getValue :: Index ctx a -> Values ctx -> Value a
getValue Z (VCons a _) = a
getValue (S ix) (VCons _ ctx) = getValue ix ctx

newtype Eval (ctx :: [Ty]) (ctx' :: [Ty]) = Eval {unEval :: Values ctx -> Values ctx'}

instance Cat Eval where
  id = Eval (\x -> x)

  compose (Eval g) (Eval f) = Eval (\x -> g (f x))

  var ix = Eval $ \ctx -> VCons (getValue ix ctx) VNil

  drop = Eval $ \(VCons _ ctx) -> ctx

  fix f = Eval $ \ctx -> unEval (f (fix f)) ctx

  bind f = Eval $ \(VCons a ctx) -> unEval (f (Eval $ VCons a)) ctx

  fn f = Eval $ \ctx -> VCons (VClosure ctx f) ctx

  app = Eval $ \(VCons (VClosure env f) (VCons x ctx)) -> let VCons b VNil = unEval f (VCons x env) in VCons b ctx

  inl = Eval $ \(VCons a ctx) -> VCons (VLeft a) ctx

  inr = Eval $ \(VCons b ctx) -> VCons (VRight b) ctx

  matchSum l r = Eval $ \(VCons s ctx) ->
    case s of
      VLeft a -> unEval l (VCons a ctx)
      VRight b -> unEval r (VCons b ctx)

  pair = Eval $ \(VCons a (VCons b ctx)) -> VCons (VPair a b) ctx

  unpair = Eval $ \(VCons (VPair a b) ctx) -> VCons a (VCons b ctx)

  par f g = Eval $ \ctx ->
    let
      VCons a VNil = unEval f ctx
      b = unEval g ctx
    in
      VCons a b

  true = Eval $ \ctx -> VCons (VBool True) ctx

  false = Eval $ \ctx -> VCons (VBool False) ctx

  ifte f g = Eval $ \(VCons (VBool b) ctx) -> if b then unEval f ctx else unEval g ctx

  char c = Eval $ \ctx -> VCons (VChar c) ctx

  matchChar fs g = Eval $ \(VCons (VChar c) ctx) ->
    Prelude.foldr
      (\(c', f) rest -> if c == c' then unEval f ctx else rest)
      (unEval g ctx)
      fs

  eqChar = Eval $ \(VCons (VChar a) (VCons (VChar b) ctx)) -> VCons (VBool $ a == b) ctx
  string s = Eval $ \ctx -> VCons (VString s) ctx
  uncons = Eval $ \(VCons (VString s) ctx) ->
    case Text.uncons s of
      Nothing -> VCons VNothing ctx
      Just (c, cs) -> VCons (VJust (VPair (VChar c) (VString cs))) ctx
  consString = Eval $ \(VCons (VChar c) (VCons (VString cs) ctx)) -> VCons (VString $ Text.cons c cs) ctx
  int i = Eval $ \ctx -> VCons (VInt i) ctx
  add = Eval $ \(VCons (VInt a) (VCons (VInt b) ctx)) -> VCons (VInt $ a + b) ctx
  mul = Eval $ \(VCons (VInt a) (VCons (VInt b) ctx)) -> VCons (VInt $ a * b) ctx
  nothing = Eval $ \ctx -> VCons VNothing ctx
  just = Eval $ \(VCons a ctx) -> VCons (VJust a) ctx
  matchMaybe f g = Eval $ \(VCons a ctx) ->
    case a of
      VNothing -> unEval f ctx
      VJust x -> unEval g (VCons x ctx)
  nil = Eval $ \ctx -> VCons VListNil ctx
  cons = Eval $ \(VCons x (VCons xs ctx)) -> VCons (VListCons x xs) ctx
  matchList f g = Eval $ \(VCons xs ctx) ->
    case xs of
      VListNil -> unEval f ctx
      VListCons a as -> unEval g (VCons a (VCons as ctx))

-- | Evaluate a 'Cat' expression.
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
  go (VCons v vs) = printValue v : go vs

data Value (ty :: Ty) where
  VInt :: Int -> Value TInt
  VChar :: Char -> Value TChar
  VString :: Text -> Value TString
  VBool :: Bool -> Value TBool
  VClosure :: Values ctx -> Eval (a ': ctx) '[b] -> Value (TExp b a)
  VLeft :: Value a -> Value (TSum a b)
  VRight :: Value b -> Value (TSum a b)
  VPair :: Value a -> Value b -> Value (TProd a b)
  VNothing :: Value (TMaybe a)
  VJust :: Value a -> Value (TMaybe a)
  VListNil :: Value (TList a)
  VListCons :: Value a -> Value (TList a) -> Value (TList a)

-- | Lift Haskell values into 'Value'.
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

instance (Reflect a, Reflect b) => Reflect (a, b) where
  type ReflectTy (a, b) = TProd (ReflectTy a) (ReflectTy b)
  reflect (a, b) = VPair (reflect a) (reflect b)