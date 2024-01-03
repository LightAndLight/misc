module Parser where

open import Data.Char hiding (_≤_)
open import Data.Nat
open import Data.Nat.Properties
open import Data.List
open import Data.Product
open import Relation.Binary.PropositionalEquality

_ : drop 1 ('a' ∷ 'b' ∷ 'c' ∷ []) ≡ ('b' ∷ 'c' ∷ [])
_ = refl

data Drop {a : Set} : ℕ → List a → List a → Set where
  zero : ∀{@0 xs} → Drop zero xs xs
  suc : ∀{@0 n x xs ys} → Drop n xs ys → Drop (suc n) (x ∷ xs) ys

data Suffix {a : Set} : List a → List a → Set where
  nil : (xs : List a) → Suffix xs xs
  cons : (x : a) → ∀{@0 xs ys} → Suffix xs ys → Suffix (x ∷ xs) ys

Suffix-∷ys : ∀{a} {y : a} {xs ys} → Suffix xs (y ∷ ys) → Suffix xs ys
Suffix-∷ys (nil (y ∷ ys)) = cons y (nil ys)
Suffix-∷ys (cons x prf) = cons x (Suffix-∷ys prf)

Suffix-xs∷ys : ∀{a} {x : a} {xs y ys} → Suffix (x ∷ xs) (y ∷ ys) → Suffix xs ys
Suffix-xs∷ys (nil (x ∷ xs)) = nil xs
Suffix-xs∷ys (cons x prf) = Suffix-∷ys prf

Suffix-Drop : ∀{a : Set} {xs ys : List a} → Suffix xs ys → Σ[ n ∈ ℕ ] (Drop n xs ys)
Suffix-Drop (nil rest) = zero , zero
Suffix-Drop (cons x suffix) with Suffix-Drop suffix
... | n , prf = suc n , suc prf

data Suffix' {a : Set} : List a → List a → Set where
  nil : ∀{@0 xs} → Suffix' xs xs
  cons : ∀{@0 x xs ys} → Suffix' xs ys → Suffix' (x ∷ xs) ys

Suffix'-Drop : ∀{a : Set} → (xs ys : List a) → @0 Suffix' xs ys → Σ[ n ∈ ℕ ] (Drop n xs ys)
Suffix'-Drop [] .[] nil = zero , zero
Suffix'-Drop (x ∷ xs) ys prf = {!!}

Drop-∷ : ∀{a : Set} {x y : a} {xs ys} → (n : ℕ) → Drop n xs ys → Drop n (x ∷ xs) (y ∷ ys)
Drop-∷ zero zero = {!!}
Drop-∷ (suc n) prf = {!!}

undrop : ∀{a : Set} → {@0 output input : List a} → Suffix input output → Σ[ n ∈ ℕ ] (Drop n input output)
undrop (nil xs) = zero , zero
undrop (cons x xs) with undrop xs
... | n , prf = suc n , (suc prf)

m≤n⇒m≤1+n : ∀{m n} → m ≤ n → m ≤ 1 + n
m≤n⇒m≤1+n z≤n       = z≤n
m≤n⇒m≤1+n (s≤s m≤n) = s≤s (m≤n⇒m≤1+n m≤n)

Drop-length : ∀{a} → (input : List a) → Drop (length input) input []
Drop-length [] = zero
Drop-length (x ∷ xs) = suc (Drop-length xs)

_ : Σ[ output ∈ List Char ] Σ[ input ∈ List Char ] (drop (length input ∸ length output) input ≢ output)
_ = 'a' ∷ 'b' ∷ 'c' ∷ [] , 'x' ∷ 'y' ∷ 'z' ∷ 'w' ∷ [] , λ ()

Drop-∸ : ∀{a} → (output input : List a) → @0 Suffix input output → Drop (length input ∸ length output) input output
Drop-∸ [] [] (nil .[]) = zero
Drop-∸ [] (x ∷ xs) (cons .x prf) = suc (Drop-∸ [] xs prf)
Drop-∸ (x ∷ xs) (y ∷ ys) prf = {!!}

undrop' : ∀{a : Set} → (output : List a) → (input : List a) → @0 Suffix input output  → Σ[ n ∈ ℕ ] (Drop n input output)
undrop' output input prf = length input ∸ length output , Drop-∸ output input prf

{-

-- | @undrop (drop n input) input == n@
undrop :: (HasCallStack) => [a] -> [a] -> Nat
undrop a b = go a b
 where
  go [] [] = Z
  go [] (x : xs) = S (go [] xs)
  go (x : xs) [] = error "trying to increase size by dropping"
  go (x : xs) (y : ys) = go xs ys

newtype Parser a = Parser {unParser :: String -> (Nat, Maybe a)}
  deriving (Functor)

instance Applicative Parser where
  pure a = Parser $ \input -> (Z, Just a)
  Parser pf <*> Parser pa = Parser $ \input ->
    let (n, mf) = pf input
     in case mf of
          Nothing ->
            (n, Nothing)
          Just f ->
            let (n', ma) = pa (drop n input)
             in (n `plus` n', f <$> ma)

instance Monad Parser where
  Parser pa >>= f = Parser $ \input ->
    let (n, ma) = pa input
     in case ma of
          Nothing ->
            (n, Nothing)
          Just a ->
            let (n', b) = unParser (f a) (drop n input)
             in (n `plus` n', b)

instance Alternative Parser where
  empty = Parser $ \_ -> (Z, Nothing)
  Parser pa <|> Parser pb = Parser $ \input ->
    case pa input of
      (Z, Nothing) ->
        pb input
      a ->
        a

  some pa = fixP (\self -> (:) <$> pa <*> (self <|> pure []))

  many pa = fixP (\self -> ((:) <$> pa <*> self) <|> pure [])

char :: Char -> Parser ()
char c = Parser $ \input ->
  case input of
    [] ->
      (Z, Nothing)
    x : xs ->
      if x == c
        then (S Z, Just ())
        else (Z, Nothing)

string :: String -> Parser ()
string s = Parser $ go s Z
 where
  go :: String -> Nat -> String -> (Nat, Maybe ())
  go [] n input = (n, Just ())
  go (x : xs) n [] = (Z, Nothing)
  go (x : xs) n (i : is) =
    if x == i
      then go xs (S n) is
      else (Z, Nothing)

eof :: Parser ()
eof = Parser $ \input ->
  case input of
    [] -> (Z, Just ())
    _ : _ -> (Z, Nothing)

fixP :: forall a. (Parser a -> Parser a) -> Parser a
fixP f = Parser (go Z)
 where
  go :: Nat -> String -> (Nat, Maybe a)
  go (S _) [] = (Z, Nothing)
  go (S n) (x : xs) = go n xs
  go Z input@[] =
    unParser
      (f (Parser (\input' -> let !_ = undrop input' [] in undefined)))
      input
  go Z input@(x : xs) =
    unParser
      (f (Parser (\input' -> go (undrop input' xs) xs)))
      input

fixPF :: forall a b. ((a -> Parser b) -> (a -> Parser b)) -> (a -> Parser b)
fixPF f pa = Parser (go pa Z)
 where
  go :: a -> Nat -> String -> (Nat, Maybe b)
  go a (S _) [] = (Z, Nothing)
  go a (S n) (x : xs) = go a n xs
  go a Z input@[] =
    unParser
      (f (\a' -> Parser (\input' -> let !_ = undrop input' [] in undefined)) a)
      input
  go a Z input@(x : xs) =
    unParser
      (f (\a' -> Parser (\input' -> go a' (undrop input' xs) xs)) a)
      input

fixP2 :: forall a b. ((Parser a, Parser b) -> (Parser a, Parser b)) -> (Parser a, Parser b)
fixP2 f = (Parser (go1 Z), Parser (go2 Z))
 where
  go1 :: Nat -> String -> (Nat, Maybe a)
  go1 (S _) [] = (Z, Nothing)
  go1 (S n) (x : xs) = go1 n xs
  go1 Z input@[] =
    unParser
      ( fst
          $ f
            ( Parser (\input' -> let !_ = undrop input' [] in undefined)
            , Parser (go2 Z)
            )
      )
      input
  go1 Z input@(x : xs) =
    unParser
      ( fst
          $ f
            ( Parser (\input' -> go1 (undrop input' xs) xs)
            , Parser (go2 Z)
            )
      )
      input

  go2 :: Nat -> String -> (Nat, Maybe b)
  go2 (S _) [] = (Z, Nothing)
  go2 (S n) (x : xs) = go2 n xs
  go2 Z input@[] =
    unParser
      ( snd
          $ f
            ( Parser (go1 Z)
            , Parser (\input' -> let !_ = undrop input' [] in undefined)
            )
      )
      input
  go2 Z input@(x : xs) =
    unParser
      ( snd
          $ f
            ( Parser (go1 Z)
            , Parser (\input' -> go2 (undrop input' xs) xs)
            )
      )
      input

fixPF2 ::
  forall a b c d.
  ((a -> Parser b, c -> Parser d) -> (a -> Parser b, c -> Parser d)) ->
  (a -> Parser b, c -> Parser d)
fixPF2 f = (Parser . go1 Z, Parser . go2 Z)
 where
  go1 :: Nat -> a -> String -> (Nat, Maybe b)
  go1 (S _) a [] = (Z, Nothing)
  go1 (S n) a (x : xs) = go1 n a xs
  go1 Z a input@[] =
    unParser
      ( fst
          ( f
              ( \a' -> Parser (\input' -> let !_ = undrop input' [] in undefined)
              , Parser . go2 Z
              )
          )
          a
      )
      input
  go1 Z a input@(x : xs) =
    unParser
      ( fst
          ( f
              ( \a' -> Parser (\input' -> go1 (undrop input' xs) a' xs)
              , Parser . go2 Z
              )
          )
          a
      )
      input

  go2 :: Nat -> c -> String -> (Nat, Maybe d)
  go2 (S _) c [] = (Z, Nothing)
  go2 (S n) c (x : xs) = go2 n c xs
  go2 Z c input@[] =
    unParser
      ( snd
          ( f
              ( Parser . go1 Z
              , \c' -> Parser (\input' -> let !_ = undrop input' [] in undefined)
              )
          )
          c
      )
      input
  go2 Z c input@(x : xs) =
    unParser
      ( snd
          ( f
              ( Parser . go1 Z
              , \c' -> Parser (\input' -> go2 (undrop input' xs) c' xs)
              )
          )
          c
      )
      input

some', many' :: Parser a -> Parser [a]
(some', many') =
  fixPF2
    ( \(some_self, many_self) ->
        ( \pa -> (:) <$> pa <*> many_self pa
        , \pa -> some_self pa <|> pure []
        )
    )

-}
