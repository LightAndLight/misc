{- | = Argument ordering

Composition is associative, and data flows right-to-left:

@f . (a . b) . f = f . (a . b)@

When @f@ is a function, the left side of this equation reads as uncurried application
(passing @a@ and @b@ to @f@),
and the right side reads as curried application (passing @b@ to the partially-applied @f . a@).
Arguments are ordered to maximise the usefulness of partial applications.

For example, it seems more likely that a programs will split different strings using the same
character, than splitting the same string using different characters. Therefore @split@ has type

@
Cat t => t (TChar ': TString ': ctx) (TList TString ': ctx)
@

so that @split . char '\n'@ has type

@
Cat t => t (TString ': ctx) (TList TString ': ctx)
@
-}
module Lib.Prelude where

import Lib
import Lib.Ty
import Prelude hiding (drop, foldl, foldr, id, (.))

mapSum ::
  (Cat t, TyC t a, TyC t a', TyC t b, TyC t b', CtxC t ctx) =>
  t '[a] '[a'] ->
  t '[b] '[b'] ->
  t (TSum a b ': ctx) (TSum a' b' ': ctx)
mapSum f g =
  matchSum
    (par (inl . f . var Z) drop)
    (par (inr . g . var Z) drop)

mapProduct ::
  (Cat t, TyC t a, TyC t a', TyC t b, TyC t b', CtxC t ctx) =>
  t '[a] '[a'] ->
  t '[b] '[b'] ->
  t (TProd a b ': ctx) (TProd a' b' ': ctx)
mapProduct f g =
  pair
    . par
      (f . var Z)
      ( par
          (g . var (S Z))
          (drop . drop)
      )
    . unpair

split ::
  (Cat t, CtxC t ctx) =>
  t (TChar ': TString ': ctx) (TSum TString (TProd TString TString) ': ctx)
split =
  fix $ \self ->
    bind $ \c ->
      bind $ \str ->
        matchMaybe
          (inl . str)
          ( bind
              ( \c' ->
                  ifte
                    (inr . pair . string "")
                    (mapSum (consString . c') (mapProduct (consString . c') id) . self . c)
                    . eqChar
                    . c'
                    . c
              )
              . unpair
          )
          . uncons
          . str

splits ::
  (Cat t, CtxC t ctx) =>
  t (TChar ': TString ': ctx) (TList TString ': ctx)
splits =
  fix $ \self ->
    bind $ \c ->
      matchSum
        (bind $ \str -> cons . str . nil)
        (cons . par (var Z) (self . c . drop) . unpair)
        . split
        . c

chars ::
  (Cat t, CtxC t ctx) =>
  t (TString ': ctx) (TList TChar ': ctx)
chars =
  fix $ \self ->
    matchMaybe
      nil
      (cons . bind (\c -> c . self) . unpair)
      . uncons

decimalDigit ::
  (Cat t, CtxC t ctx) =>
  t (TChar ': ctx) (TMaybe TInt ': ctx)
decimalDigit =
  matchChar
    [ ('0', just . int 0)
    , ('1', just . int 1)
    , ('2', just . int 2)
    , ('3', just . int 3)
    , ('4', just . int 4)
    , ('5', just . int 5)
    , ('6', just . int 6)
    , ('7', just . int 7)
    , ('8', just . int 8)
    , ('9', just . int 9)
    ]
    nothing

{-# INLINEABLE filterMap #-}
filterMap ::
  (Cat t, TyC t b, TyC t a, CtxC t ctx) =>
  t (TExp (TMaybe b) a ': TList a ': ctx) (TList b ': ctx)
filterMap =
  fix $ \self ->
    bind $ \f ->
      matchList
        nil
        ( matchMaybe
            (self . f)
            ( cons
                . par
                  (var Z)
                  (self . f . drop)
            )
            . app
            . f
        )

first ::
  (Cat t, TyC t a, CtxC t ctx) =>
  t (TList a ': ctx) (a ': ctx)
first =
  matchList
    undefined
    (par (var Z) (drop . drop))

last ::
  (Cat t, TyC t a, CtxC t ctx) =>
  t (TList a ': ctx) (a ': ctx)
last =
  fix $ \self ->
    matchList
      undefined
      ( bind $ \x ->
          bind $ \xs ->
            matchList x (self . xs . drop . drop) . xs
      )

foldr ::
  (Cat t, TyC t b, TyC t a, CtxC t ctx) =>
  t (TExp b (TProd a b) ': b ': TList a ': ctx) (b ': ctx)
foldr =
  fix $ \self ->
    bind $ \f ->
      bind $ \z ->
        matchList
          z
          (app . f . pair . par (var Z) (self . f . z . drop))

{-# INLINEABLE map #-}
map ::
  (Cat t, TyC t b, TyC t a, CtxC t ctx) =>
  t (TExp b a ': TList a ': ctx) (TList b ': ctx)
map =
  bind $ \f ->
    foldr
      . fn (cons . app . f . unpair . var Z)
      . nil

foldl ::
  (Cat t, TyC t b, TyC t a, CtxC t ctx) =>
  t (TExp b (TProd b a) ': b ': TList a ': ctx) (b ': ctx)
foldl =
  fix $ \self ->
    bind $ \f ->
      bind $ \z ->
        matchList
          z
          (self . f . app . f . pair . z)

sum ::
  (Cat t, CtxC t ctx) =>
  t (TList TInt ': ctx) (TInt ': ctx)
sum = foldl . fn (add . unpair . var Z) . int 0

isPrefixOf ::
  (Cat t, CtxC t ctx) =>
  t (TList TChar ': TList TChar ': ctx) (TBool ': ctx)
isPrefixOf =
  fix $ \self ->
    matchList
      (true . drop)
      ( bind $ \x ->
          bind $ \xs ->
            matchList
              false
              ( bind $ \x' ->
                  bind $ \xs' ->
                    ifte (self . xs . xs') false
                      . (eqChar . x . x')
              )
      )

orElse ::
  (Cat t, TyC t a, CtxC t ctx) =>
  t (TMaybe a ': TMaybe a ': ctx) (TMaybe a ': ctx)
orElse = bind $ \x -> matchMaybe id (x . drop . drop) . x