{- | = Argument ordering

Composition is associative:

@(a . b) . f = a . (b . f)@

When @f@ is a function, the left side of this equation reads as uncurried application
(passing @a@ and @b@ to @f@),
and the right side reads as curried application (passing @a@ to the partially-applied @b . f@).
Arguments are ordered to maximise the usefulness of partial applications.

For example, it seems more likely that a programs will split different strings using the same
character, than splitting the same string using different characters. Therefore @split@ has type

@
Cat t => t (ctx :. TString :. TChar) (ctx :. TList TString)
@

so that @char '\n' . split@ has type

@
Cat t => t (ctx :. TString) (ctx :. TList TString)
@
-}
module Lib.Prelude where

import Lib
import Lib.Ty
import Prelude hiding (drop, foldl, foldr, id, (.))

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