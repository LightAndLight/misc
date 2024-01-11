module Parser where

open import Data.Char hiding (_≤_; _<_)
open import Data.Nat
open import Data.Bool
open import Data.Unit
open import Data.Empty
open import Data.Maybe
open import Data.Nat.Properties
open import Data.List
open import Data.Product hiding (_<*>_)
open import Relation.Binary.PropositionalEquality

case_of_ : ∀{a b : Set} → a → (a → b) → b
case_of_ x f = f x

_ : drop 1 ('a' ∷ 'b' ∷ 'c' ∷ []) ≡ ('b' ∷ 'c' ∷ [])
_ = refl

data Drop {a : Set} : ℕ → List a → List a → Set where
  zero : ∀{@0 xs} → Drop zero xs xs
  suc : ∀{@0 n x xs ys} → Drop n xs ys → Drop (suc n) (x ∷ xs) ys

data Suffix {a : Set} : @0 List a → @0 List a → Set where
  zero : ∀{@0 xs} → Suffix xs xs
  suc : ∀{@0 x xs ys} → Suffix xs ys → Suffix (x ∷ xs) ys

record Exists (a : Set) (P : a → Set) : Set where
  field
    @0 {index} : a
    value : P index

Suffix-Drop : ∀{a : Set} {@0 xs ys : List a} → Suffix xs ys → Exists ℕ (λ n → Drop n xs ys)
Suffix-Drop zero = record { value = zero }
Suffix-Drop (suc suffix) with Suffix-Drop suffix
... | record { value = value } = record { value = suc value }

Drop-Suffix : ∀{a : Set} {@0 xs ys : List a} → Exists ℕ (λ n → Drop n xs ys) → Suffix xs ys
Drop-Suffix record { index = .zero ; value = zero } = zero
Drop-Suffix record { index = .(suc _) ; value = (suc prf) } = suc (Drop-Suffix (record { value = prf }))

Suffix-trans : ∀{a : Set} {@0 xs ys zs : List a} → Suffix xs ys → Suffix ys zs → Suffix xs zs
Suffix-trans zero index' = index'
Suffix-trans (suc index) index' = suc (Suffix-trans index index')

record Subset (a : Set) (P : a → Set) : Set where
  field
    value : a
    @0 prf : P value

Suffix-drop : ∀{a : Set} {@0 ys} → (xs : List a) → Suffix xs ys → Subset (List a) (λ x → x ≡ ys)
Suffix-drop xs zero = record { value = xs ; prf = refl }
Suffix-drop (x ∷ xs) (suc index) = Suffix-drop xs index

Suffix-un∷ᵣ : ∀{a : Set} {@0 xs : List a} {@0 y ys} → Suffix xs (y ∷ ys) → Suffix xs ys
Suffix-un∷ᵣ zero = suc zero
Suffix-un∷ᵣ (suc index) = suc (Suffix-un∷ᵣ index)

Suffix-xs-ys++xs-⊥ : ∀{a : Set} {@0 y} → (ys xs : List a) → Suffix xs ((y ∷ ys) ++ xs) → ⊥
Suffix-xs-ys++xs-⊥ [] (x ∷ xs) (suc index) = Suffix-xs-ys++xs-⊥ (x ∷ []) xs index
Suffix-xs-ys++xs-⊥ (y ∷ ys) xs index = Suffix-xs-ys++xs-⊥ ys xs (Suffix-un∷ᵣ index)

Suffix-xs-x∷xs-⊥ : ∀{a : Set} {@0 x : a} → (xs : List a) → Suffix xs (x ∷ xs) → ⊥
Suffix-xs-x∷xs-⊥ xs index = Suffix-xs-ys++xs-⊥ [] xs index

Drop-length : ∀{a} → (input : List a) → Drop (length input) input []
Drop-length [] = zero
Drop-length (x ∷ xs) = suc (Drop-length xs)

_ : Σ[ output ∈ List Char ] Σ[ input ∈ List Char ] (drop (length input ∸ length output) input ≢ output)
_ = 'a' ∷ 'b' ∷ 'c' ∷ [] , 'x' ∷ 'y' ∷ 'z' ∷ 'w' ∷ [] , λ ()

subst-erased : ∀{a : Set} {@0 x y : a} → (@0 P : a → Set) → @0 x ≡ y → P x → P y
subst-erased P refl Px = Px

data _≤'_ : ℕ → ℕ → Set where
  z≤n : ∀{@0 n}                 → zero  ≤' n
  s≤s : ∀{@0 m n} (m≤n : m ≤' n) → suc m ≤' suc n

_<'_ : ℕ → ℕ → Set
m <' n = suc m ≤' n

infix 4 _≤'_ _<'_

m≤'n⇒m≤'1+n : ∀{@0 m n} → m ≤' n → m ≤' 1 + n
m≤'n⇒m≤'1+n z≤n       = z≤n
m≤'n⇒m≤'1+n (s≤s m≤n) = s≤s (m≤'n⇒m≤'1+n m≤n)

≤'-refl : (n : ℕ) → n ≤' n
≤'-refl zero = z≤n
≤'-refl (suc n) = s≤s (≤'-refl n)

Suffix-length-≤ : ∀{a : Set} {@0 ys : List a} → (xs : List a) → Suffix xs ys → length ys ≤' length xs
Suffix-length-≤ xs zero = ≤'-refl (length xs)
Suffix-length-≤ (x ∷ xs) (suc prf) = m≤'n⇒m≤'1+n (Suffix-length-≤ xs prf)

∸-suc : ∀{@0 m n} → m ≤' n → suc n ∸ m ≡ suc (n ∸ m)
∸-suc z≤n = refl
∸-suc (s≤s m≤n) = ∸-suc m≤n

Drop-∸ : ∀{a} → {@0 input output : List a} → Suffix input output → Drop (length input ∸ length output) input output
Drop-∸ {output = output} zero =
  subst-erased (λ x → Drop x output output) (sym (n∸n≡0 (length output))) zero
Drop-∸ {input = x ∷ xs} {output = output} (suc prf) =
  subst-erased (λ a → Drop a (x ∷ xs) output) (sym p1) (suc (Drop-∸ prf))
  where
    @0 p1 : length (x ∷ xs) ∸ length output ≡ suc (length xs ∸ length output)
    p1 = ∸-suc {m = length output} {n = length xs} (Suffix-length-≤ xs prf)

undrop : ∀{a : Set} → (output input : List a) → Suffix input output → Exists ℕ (λ n → Drop n input output)
undrop output input offset = record { value = Drop-∸ offset }

1+n≰'n : (n : ℕ) → (1 + n ≤' n) → ⊥
1+n≰'n zero ()
1+n≰'n (suc n) (s≤s prf) = 1+n≰'n n prf

⊥-elim-erased : ∀{a : Set} → @0 ⊥ → a
⊥-elim-erased ()

Suffix-un∷ₗ : ∀{a : Set} {@0 x : a} {@0 xs ys} → @0 length ys ≤' length xs → Suffix (x ∷ xs) ys → Suffix xs ys
Suffix-un∷ₗ {x = x} {xs = xs} prf zero = ⊥-elim-erased (1+n≰'n (length xs) prf)
Suffix-un∷ₗ prf (suc index) = index

record Result (a : Set) (input : List Char) : Set where
  field
    @0 {suffix} : List Char
    consumed : Suffix input suffix
    value : Maybe a

data IsSuc {a : Set} : ∀{@0 input output : List a} → Suffix input output → Set where
  is-suc : ∀{@0 x : a} {@0 input output} {@0 suffix : Suffix input output} → IsSuc (suc {x = x} suffix)

data IsZero {a : Set} : ∀{@0 input output : List a} → Suffix input output → Set where
  is-zero : ∀{@0 input} {@0 suffix : Suffix input input} → IsZero {input = input} {output = input} zero

record Result'' (consumes : Bool) (a : Set) (input : List Char) : Set where
  field
    @0 {suffix} : List Char
    consumed : Suffix input suffix
    value : Maybe a
    @0 {consumes-suc} : IsSuc consumed → (consumes ≡ true)
    @0 {consumes-zero} : IsZero consumed → (consumes ≡ false)

if-consumed : ∀{@0 consumes a input} {r : Bool → Set} → Result'' consumes a input → (@0 consumes ≡ false → r consumes) → (@0 consumes ≡ true → r consumes) → r consumes
if-consumed {consumes = consumes} {a = a} {r = r} result case-false case-true =
  go (Result''.consumed result) (Result''.consumes-suc result) (Result''.consumes-zero result)
  where
    go :
      ∀{@0 input output} →
      (consumed : Suffix input output) →
      @0 (IsSuc consumed → (consumes ≡ true)) →
      @0 (IsZero consumed → (consumes ≡ false)) →
      r consumes
    go consumed prf-suc-bwd prf-zero-bwd with consumed
    ... | zero = case-false (prf-zero-bwd (is-zero { suffix = consumed }))
    ... | suc res = case-true (prf-suc-bwd is-suc)

data Result' (a : Set) : Bool → List Char → Set where
  failed :
    ∀{@0 input con} {@0 suffix : List Char} →
    (consumed : Suffix input suffix) →
    Result' a con input
  must-consume :
    ∀{@0 x xs} {@0 suffix : List Char} →
    (consumed : Suffix xs suffix) →
    (value : a) →
    Result' a true (x ∷ xs)
  may-consume :
    ∀{@0 input} {@0 suffix : List Char} →
    (consumed : Suffix input suffix) →
    (value : a) →
    Result' a false input

@0 suffix' : ∀{@0 a consumed input} → Result' a consumed input → List Char
suffix' (failed {suffix = suffix} consumed) = suffix
suffix' (must-consume {suffix = suffix} consumed value) = suffix
suffix' (may-consume {suffix = suffix} consumed value) = suffix

consumed' : ∀{@0 a consumed input} → (result : Result' a consumed input) → Suffix input (suffix' result)
consumed' (failed consumed) = consumed
consumed' (must-consume consumed value) = suc consumed
consumed' (may-consume consumed value) = consumed

value' : ∀{@0 a consumed input} → (result : Result' a consumed input) → Maybe a
value' (failed consumed) = nothing
value' (must-consume consumed value) = just value
value' (may-consume consumed value) = just value

Parser : Set → Set
Parser a = (input : List Char) → Result a input

Parser' : Bool → Set → Set
Parser' consumes a = (input : List Char) → Result' a consumes input

Parser'' : Bool → Set → Set
Parser'' consumes a = (input : List Char) → Result'' consumes a input

pure : ∀{a} → a → Parser a
pure a = λ input → record { consumed = zero ; value = just a }

pure' : ∀{a} → a → Parser' false a
pure' a = λ input → may-consume zero a

pure'' : ∀{a} → a → Parser'' false a
pure'' a = λ input → record { consumed = zero ; value = just a ; consumes-suc = λ () ; consumes-zero = λ is-zero → refl }

_<*>_ : ∀{a b} → Parser (a → b) → Parser a → Parser b
pf <*> pa = λ input →
  let rf = pf input in
  case Result.value rf of λ{
    nothing → record { consumed = Result.consumed rf ; value = nothing } ;
    (just f) →
      let record { value = input' ; prf = input'≡suffix-rf } = Suffix-drop input (Result.consumed rf) in
      let ra = pa input' in
      record {
        consumed =
          Suffix-trans
            (Result.consumed rf)
            (subst-erased (λ x → Suffix x (Result.suffix ra)) input'≡suffix-rf (Result.consumed ra))
        ; value = case Result.value ra of λ{
            nothing → nothing ;
            (just a) → just (f a)
          }
      }
  }

infixl 5 _<*>_

_<*>'_ : ∀{a b con con'} → Parser' con (a → b) → Parser' con' a → Parser' (con ∨ con') b
_<*>'_ {a = a} {b = b} {con = con} {con' = con'} pf pa = λ input →
  let rf = pf input in
  go input rf
  where
    go2 : ∀{@0 suffix} → (a → b) → (input : List Char) → Suffix input suffix → Result' b con' input
    go2 {suffix = suffix} f input fconsumed = {!!}
      where
        x : Subset (List Char) (λ x → x ≡ suffix)
        x = Suffix-drop input fconsumed

        input' : List Char
        input' = Subset.value x

        @0 input'≡suffix-rf : input' ≡ suffix
        input'≡suffix-rf = Subset.prf x

        ra : Result' a con' input'
        ra = pa input'

    go : (input : List Char) → Result' (a → b) con input → Result' b (con ∨ con') input
    go input (failed consumed) = failed consumed
    go (x ∷ xs) (must-consume {suffix = suffix} fconsumed f) =
      case value' ra of λ{
        nothing → failed (Suffix-trans (suc fconsumed) (subst-erased (λ x → Suffix x (suffix' ra)) input'≡suffix-rf (consumed' ra))) ;
        (just a) → must-consume (Suffix-trans fconsumed ((subst-erased (λ x → Suffix x (suffix' ra)) input'≡suffix-rf (consumed' ra)))) (f a)
      }
      where
        p1 : Subset (List Char) (λ x → x ≡ suffix)
        p1 = Suffix-drop (x ∷ xs) (suc fconsumed)

        input' : List Char
        input' = Subset.value p1

        @0 input'≡suffix-rf : input' ≡ suffix
        input'≡suffix-rf = Subset.prf p1

        ra : Result' a con' input'
        ra = pa input'
    go input (may-consume {suffix = suffix} fconsumed f) =
      case value' ra of λ{
        nothing → failed (Suffix-trans fconsumed (subst-erased (λ x → Suffix x (suffix' ra)) input'≡suffix-rf (consumed' ra))) ;
        (just a) → {!!}
      }
      where
        p1 : Subset (List Char) (λ x → x ≡ suffix)
        p1 = Suffix-drop input fconsumed

        input' : List Char
        input' = Subset.value p1

        @0 input'≡suffix-rf : input' ≡ suffix
        input'≡suffix-rf = Subset.prf p1

        ra : Result' a con' input'
        ra = pa input'
    {-
  case value' rf of λ{
    nothing → record { consumed = consumed' rf ; value = nothing } ;
    (just f) →
      let record { value = input' ; prf = input'≡suffix-rf } = Suffix-drop input (consumed' rf) in
      let ra = pa input' in
      record {
        consumed =
          Suffix-trans
            (consumed' rf)
            (subst-erased (λ x → Suffix x (suffix' ra)) input'≡suffix-rf (consumed' ra))
        ; value = case value' ra of λ{
            nothing → nothing ;
            (just a) → just (f a)
          }
      }
  }
    -}



_<*>''_ : ∀{a b con con'} → Parser'' con (a → b) → Parser'' con' a → Parser'' (con ∨ con') b
_<*>''_ {con = con} {con' = con'} pf pa = λ input →
  let rf = pf input in
  case Result''.value rf of λ{
    nothing →
      record {
        consumed = Result''.consumed rf
        ; value = nothing
        ; consumes-suc = λ x → let y = Result''.consumes-suc rf x in subst (λ x → x ∨ con' ≡ true) (sym y) refl
        ; consumes-zero = λ x → let y = Result''.consumes-zero rf x in subst (λ x → x ∨ con' ≡ false) (sym y) {!!}
        } ;
    (just f) →
      let record { value = input' ; prf = input'≡suffix-rf } = Suffix-drop input (Result''.consumed rf) in
      let ra = pa input' in
      {-
          Suffix-trans
            (Result''.consumed rf)
            (subst-erased (λ x → Suffix x (Result''.suffix ra)) input'≡suffix-rf (Result''.consumed ra))
      -}
        case Result''.value ra of λ{
            nothing → record {
              consumed = 
                Suffix-trans
                  (Result''.consumed rf)
                  (subst-erased (λ x → Suffix x (Result''.suffix ra)) input'≡suffix-rf (Result''.consumed ra))
              ; value = nothing
              ; consumes-suc = {!!}
              ; consumes-zero = {!!}
            } ;
            (just a) → record {
              consumed = 
                Suffix-trans
                  (Result''.consumed rf)
                  (subst-erased (λ x → Suffix x (Result''.suffix ra)) input'≡suffix-rf (Result''.consumed ra))
              ; value = just (f a)
              ; consumes-suc = {!!}
              ; consumes-zero = {!!}
              }
        }
  }

infixl 5 _<*>''_

{-

{-

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

-}

char : Char → Parser' true ⊤
char c = λ{ [] → failed zero ; (x ∷ xs) → 
  if c Data.Char.== x
  then must-consume zero tt
  else failed zero
  }

{-

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

-}

□ : (List Char → Set) → List Char → Set
□ a input = ((input' : List Char) → @0 length input' <' length input → Suffix input input' → a input')

infix 6 □_

□-extract : ∀{a : List Char → Set} {@0 x xs} → (input : List Char) → input ≡ x ∷ xs → □ a input → a xs
□-extract (x ∷ xs) refl □-a = □-a xs (≤'-refl (length (x ∷ xs))) (suc zero)

_⇒_ : (List Char → Set) → (List Char → Set) → List Char → Set
(a ⇒ b) input = a input → b input

infixr 5 _⇒_

∀[_] : (List Char → Set) → Set
∀[ a ] = {input : List Char} → a input

_$_ : ∀{a b : List Char → Set} → ∀[ a ⇒ b ] → ∀[ a ] → ∀[ b ]
(f $ x) {input} = f {input} (x {input})

infixl 5 _$_

fixₚ :
  ∀{a : Set} →
  -- ((input : List Char) → ((input' : List Char) → @0 length input' <' length input → Suffix input input' → Result a input') → Result a input) →
  ∀[ □ (Result' a true) ⇒ Result' a true ] →
  Parser' true a
fixₚ {a} f = λ input → go input zero
  where
    go : ∀{@0 output} → (input : List Char) → Suffix input output → Result' a true output
    go [] zero = f {[]} (λ input' ())
    go (x ∷ xs) zero = f {x ∷ xs} (λ{ input' (s≤s input'≤xs) suffix → go xs (Suffix-un∷ₗ input'≤xs suffix) })
    go (x ∷ xs) (suc index) = go xs index

fixₚ' :
  ∀{a : List Char → Set} →
  -- ((input : List Char) → ((input' : List Char) → @0 length input' <' length input → Suffix input input' → Result a input') → Result a input) →
  ∀[ □ a ⇒ a ] →
  ∀[ a ]
fixₚ' {a} f {input} = go input zero
  where
    go : ∀{@0 output} → (input : List Char) → Suffix input output → a output
    go [] zero = f {[]} (λ input' ())
    go (x ∷ xs) zero = f {x ∷ xs} (λ{ input' (s≤s input'≤xs) suffix → go xs (Suffix-un∷ₗ input'≤xs suffix) })
    go (x ∷ xs) (suc index) = go xs index

↓_ : ∀{a : Set} → Parser a → ∀[ Result a ]
(↓ p) {input} = p input

infix 6 ↓_

↓'_ : ∀{a : Set} {con} → Parser' con a → ∀[ Result' a con ]
(↓' p) {input} = {!!}

infix 6 ↓'_

↓?_ : ∀{a : Set} {con} → Parser' con a → ∀[ Result a ]
(↓? p) {input} = {!!}

infix 6 ↓?_

_<*>ᵣ_ : ∀{a b : Set} {con} → ∀[ Result' (a → b) true ⇒ □ (Result' a con) ⇒ Result' b true ]
_<*>ᵣ_ {a = a} {b = b} {con = con} {input = input} rf □-Result-a =
  case rf of λ{
    (must-consume {suffix = suffix} consumed f) →
          let
            record { value = input' ; prf = prf } = Suffix-drop input (suc consumed)

            ra : Result' a con suffix
            ra =
              subst-erased (λ x → Result' a con x) (prf) (
              □-Result-a
                input'
                (s≤s (subst-erased (λ x → length x ≤' _) (sym prf) (Suffix-length-≤ _ consumed)))
                (subst-erased (λ x → Suffix (_ ∷ _) x) (sym prf) (suc consumed))
              )
          in
          go consumed f ra
      ;
    (failed consumed) → failed consumed
  }
  where
    go : ∀{@0 suffix b x xs} → Suffix xs suffix → (a → b) → Result' a con suffix → Result' b true (x ∷ xs)
    go fconsumed f result with value' result
    ... | nothing = failed (suc fconsumed)
    ... | just a = must-consume (Suffix-trans fconsumed (consumed' result)) (f a)

infixl 5 _<*>ᵣ_

box : ∀{a : List Char → Set} → ∀[ a ] → ∀[ □ a ]
box {a} pa {input} = λ input' length-input'<length-input suffix → go input suffix
  where
    go : ∀{@0 output} → (input : List Char) → Suffix input output → a output
    go input zero = pa {input}
    go (x ∷ xs) (suc index) = go xs index

infix 6 ↑_

_<|>ᵣ_ : ∀{a : Set} {con con'} → ∀[ □ (Result' a con) ⇒ □ (Result' a con') ⇒ □ (Result' a (con ∨ con')) ]
_<|>ᵣ_ = {!!}

infixr 4 _<|>ᵣ_

some : ∀{a} → Parser' true a → Parser' true (List a)
some pa =
  fixₚ (λ self → ↓' (pure' _∷_ <*>' pa) <*>ᵣ (self <|>ᵣ box (↓' pure' [])))

_ : some (char 'c') ('c' ∷ 'c' ∷ 'c' ∷ []) ≡ must-consume (suc (suc zero)) (tt ∷ tt ∷ tt ∷ [])
_ = refl
{-
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

-}
