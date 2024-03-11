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
import Data.Text (Text)
import Prelude hiding (drop, filter, foldl, foldr, id, last, map, sum, (.))

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
