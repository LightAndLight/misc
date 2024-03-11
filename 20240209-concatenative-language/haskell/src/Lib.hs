{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -ddump-simpl
    -dsuppress-idinfo
    -dsuppress-coercions
    -dsuppress-type-applications
    -dsuppress-uniques
    -dsuppress-module-prefixes 
    -ddump-to-file #-}

module Lib where

import Data.Kind (Type)
import Data.List (intercalate)
import Data.Text (Text)
import qualified Data.Text as Text
import Prelude hiding (drop, filter, foldl, foldr, id, last, map, sum, (.))
import qualified Prelude

data Ty
  = TString
  | TInt
  | TChar
  | TBool
  | TSum Ty Ty
  | TProd Ty Ty
  | TExp Ty Ty
  | TList Ty
  | TMaybe Ty

data Ctx = Nil | (:.) Ctx Ty

infixl 5 :.

data Index :: Ctx -> Ty -> Type where
  Z :: Index (ctx :. a) a
  S :: Index ctx a -> Index (ctx :. b) a

class Cat (t :: Ctx -> Ctx -> Type) where
  id :: t a a
  compose :: t a b -> t b c -> t a c

  var :: Index x a -> t x (Nil :. a)
  drop :: t (ctx :. a) ctx

  fix :: (t a b -> t a b) -> t a b
  bind :: ((forall x'. t x' (x' :. a)) -> t x b) -> t (x :. a) b

  fn :: t (ctx :. a) (Nil :. b) -> t ctx (ctx :. TExp b a)
  app :: t (ctx :. TExp b a :. a) (ctx :. b)

  inl :: t (ctx :. a) (ctx :. TSum a b)
  inr :: t (ctx :. b) (ctx :. TSum a b)
  matchSum :: t (ctx :. a) ctx' -> t (ctx :. b) ctx' -> t (ctx :. TSum a b) ctx'

  pair :: t (ctx :. a :. b) (ctx :. TProd a b)
  unpair :: t (ctx :. TProd a b) (ctx :. a :. b)
  par :: t ctx a -> t ctx (Nil :. b) -> t ctx (a :. b)

  true :: t ctx (ctx :. TBool)
  false :: t ctx (ctx :. TBool)
  ifte :: t ctx ctx' -> t ctx ctx' -> t (ctx :. TBool) ctx'

  char :: Char -> t a (a :. TChar)
  matchChar :: [(Char, t ctx ctx')] -> t ctx ctx' -> t (ctx :. TChar) ctx'
  eqChar :: t (ctx :. TChar :. TChar) (ctx :. TBool)

  string :: Text -> t a (a :. TString)
  uncons :: t (ctx :. TString) (ctx :. TMaybe (TProd TString TChar))
  consString :: t (ctx :. TString :. TChar) (ctx :. TString)

  int :: Int -> t a (a :. TInt)
  add :: t (x :. TInt :. TInt) (x :. TInt)
  mul :: t (x :. TInt :. TInt) (x :. TInt)

  nothing :: t x (x :. TMaybe a)
  just :: t (x :. a) (x :. TMaybe a)
  matchMaybe :: t ctx ctx' -> t (ctx :. a) ctx' -> t (ctx :. TMaybe a) ctx'

  nil :: t x (x :. TList a)
  cons :: t (x :. TList a :. a) (x :. TList a)
  matchList :: t ctx ctx' -> t (ctx :. TList a :. a) ctx' -> t (ctx :. TList a) ctx'

(.) :: (Cat t) => t a b -> t b c -> t a c
(.) = compose

infixr 5 .

mapSum ::
  (Cat t) =>
  t (Nil :. a) (Nil :. a') ->
  t (Nil :. b) (Nil :. b') ->
  t (ctx :. TSum a b) (ctx :. TSum a' b')
mapSum f g =
  matchSum
    (par drop (var Z . f . inl))
    (par drop (var Z . g . inr))

mapProduct ::
  (Cat t) =>
  t (Nil :. a) (Nil :. a') ->
  t (Nil :. b) (Nil :. b') ->
  t (ctx :. TProd a b) (ctx :. TProd a' b')
mapProduct f g =
  unpair
    . par
      ( par
          (drop . drop)
          (var (S Z) . f)
      )
      (var Z . g)
    . pair

split :: (Cat t) => t (ctx :. TString :. TChar) (ctx :. TSum TString (TProd TString TString))
split =
  fix $ \self ->
    bind $ \c ->
      bind $ \str ->
        str
          . uncons
          . matchMaybe
            (str . inl)
            ( unpair
                . bind
                  ( \c' ->
                      c
                        . c'
                        . eqChar
                        . ifte
                          (string mempty . pair . inr)
                          (c . self . mapSum (c' . consString) (mapProduct id (c' . consString)))
                  )
            )

splits :: (Cat t) => t (ctx :. TString :. TChar) (ctx :. TList TString)
splits =
  fix $ \self ->
    bind $ \c ->
      c
        . split
        . matchSum
          (bind $ \str -> nil . str . cons)
          (unpair . par (drop . c . self) (var Z) . cons)

chars :: (Cat t) => t (ctx :. TString) (ctx :. TList TChar)
chars =
  fix $ \self ->
    uncons
      . matchMaybe
        nil
        (unpair . (bind $ \c -> self . c) . cons)

decimalDigit :: (Cat t) => t (ctx :. TChar) (ctx :. TMaybe TInt)
decimalDigit =
  matchChar
    [ ('0', int (0) . just)
    , ('1', int (1) . just)
    , ('2', int (2) . just)
    , ('3', int (3) . just)
    , ('4', int (4) . just)
    , ('5', int (5) . just)
    , ('6', int (6) . just)
    , ('7', int (7) . just)
    , ('8', int (8) . just)
    , ('9', int (9) . just)
    ]
    nothing

{-# INLINEABLE filterMap #-}
filterMap :: (Cat t) => t (ctx :. TList a :. TExp (TMaybe b) a) (ctx :. TList b)
filterMap =
  fix $ \self ->
    bind $ \f ->
      matchList
        nil
        ( (bind $ \a -> f . a . app)
            . matchMaybe
              (f . self)
              ( par
                  (drop . f . self)
                  ((var Z))
                  . cons
              )
        )

first :: (Cat t) => t (ctx :. TList a) (ctx :. a)
first =
  matchList
    undefined
    (par (drop . drop) (var Z))

last :: (Cat t) => t (ctx :. TList a) (ctx :. a)
last =
  fix $ \self ->
    matchList
      undefined
      ( bind $ \x ->
          bind $ \xs ->
            xs . matchList x (drop . drop . xs . self)
      )

{-# INLINEABLE map #-}
map :: (Cat t) => t (ctx :. TList a :. TExp b a) (ctx :. TList b)
map =
  bind $ \f ->
    fn (var Z . unpair . par (var (S Z)) (var Z . bind (\a -> f . a) . app) . cons)
      . nil
      . foldr

foldr :: (Cat t) => t (ctx :. TList a :. TExp b (TProd b a) :. b) (ctx :. b)
foldr =
  fix $ \self ->
    bind $ \z ->
      bind $ \f ->
        matchList
          z
          ( bind $ \a ->
              (f . z . self)
                . (bind (\b -> f . b) . a . pair)
                . app
          )

foldl :: (Cat t) => t (ctx :. TList a :. TExp b (TProd a b) :. b) (ctx :. b)
foldl =
  fix $ \self ->
    bind $ \z ->
      bind $ \f ->
        matchList
          z
          ( bind (\a -> f . a . z)
              . pair
              . app
              . bind (\z' -> f . z')
              . self
          )

sum :: (Cat t) => t (ctx :. TList TInt) (ctx :. TInt)
sum = fn (var Z . unpair . add) . int 0 . foldl

isPrefixOf :: (Cat t) => t (ctx :. TList TChar :. TList TChar) (ctx :. TBool)
isPrefixOf =
  fix $ \self ->
    matchList
      (drop . true)
      ( bind $ \x -> bind $ \xs ->
          matchList
            false
            ( bind $ \x' -> bind $ \xs' ->
                (x . x' . eqChar)
                  . ifte (xs' . xs . self) false
            )
      )

orElse :: (Cat t) => t (ctx :. TMaybe a :. TMaybe a) (ctx :. TMaybe a)
orElse = bind $ \x -> x . matchMaybe id (drop . drop . x)

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