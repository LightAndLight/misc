{-# OPTIONS --safe --erasure #-}
module Parser where

open import Agda.Builtin.Strict using (primForce) 
open import Data.Bool using (Bool; false; true; if_then_else_; _∨_; _∧_; T)
open import Data.Char using (Char; isLower)
open import Data.Empty using (⊥; ⊥-elim)
open import Data.List as List using (List; []; _∷_; [_]; _++_; concat)
open import Data.List.Relation.Unary.All using (All)
open import Data.Maybe using (Maybe; nothing; just)
open import Data.Nat using (ℕ; suc; zero; _<_; _≤_)
open import Data.Product using (Σ-syntax; _×_; _,_) renaming (proj₁ to fst; proj₂ to snd)
import Data.String as String
open import Data.Sum using (_⊎_) renaming (inj₁ to inl; inj₂ to inr)
open import Data.Unit using (⊤; tt)
open import Function using (_∘_; case_of_; _↔_)
open import Relation.Binary.PropositionalEquality using (_≡_; refl; subst; inspect) renaming ([_] to [_]ᵢ)




module _ where
  open Spec (Char)

  Parses : (∀{i : ℕ} → Parser i) → Lang → Set
  Parses p l = (input : List Char) → T (parse input p) ↔ l input

  ValidParser : Spec.Lang Char → Set
  ValidParser l = Σ[ p ∈ (∀{i} → Parser i) ] ((input : List Char) → T (parse input p) ↔ l input)

  ValidParser-ν : ∀{P : Lang} → (p : ValidParser P) → T (nullable (fst p {i = 0})) ↔ ν P
  ValidParser-ν (p , valid) = valid []

  ValidParser-δ :
    ∀{P : Lang} →
    (p : ValidParser P) →
    (c : Char) →
    Parses (consume (fst p) c) (δ P c)
  ValidParser-δ {P} (p , valid) c input = valid (c ∷ input)

module NeverProofs where
  open import Function using (_↔_; mk↔)

  open Spec (Char)

  parse-never≡false : (input : List Char) → parse input never ≡ false
  parse-never≡false [] = refl
  parse-never≡false (x ∷ xs) rewrite parse-never≡false xs = refl

  parse→ø : (input : List Char) → T (parse input never) → ø input
  parse→ø [] = ⊥-elim
  parse→ø (x ∷ xs) = parse→ø xs

  correct : Parses never ø
  correct input =
    mk↔
      {to = parse→ø input}
      {from = from input}
      (
        (λ{ refl → to-from input })
        ,
        (λ{ refl → from-to input })
      )
    where
      from : (input : List Char) → ø input → T (parse input never)
      from [] = λ x → x
      from (x ∷ xs) = from xs

      to-from : (input : List Char) → {x : ø input} → parse→ø input (from input x) ≡ x
      to-from [] = refl
      to-from (x ∷ xs) = to-from xs

      from-to : (input : List Char) → {x : T (parse input never)} → from input (parse→ø input x) ≡ x
      from-to [] = refl
      from-to (x ∷ xs) = from-to xs


module AlwaysProofs where
  open import Function using (_↔_; mk↔)

  open Spec (Char)

  parse→ε : (input : List Char) → T (parse input always) → ε input
  parse→ε [] = λ{ tt → refl }
  parse→ε (x ∷ xs) rewrite NeverProofs.parse-never≡false xs = ⊥-elim

  correct : Parses always ε
  correct input =
    mk↔
      {to = parse→ε input}
      {from = from input}
      (
        (λ{ refl → to-from input })
        ,
        (λ{ refl → from-to input })
      )
    where
      from : (input : List Char) → ε input → T (parse input always)
      from [] = λ{ refl → tt }
      from (x ∷ xs) = λ ()

      to-from : (input : List Char) → {x : ε input} → parse→ε input (from input x) ≡ x
      to-from [] {x = refl} = refl
      to-from (c ∷ cs) {x = ()}

      from-to : (input : List Char) → {x : T (parse input always)} → from input (parse→ε input x) ≡ x
      from-to [] = refl
      from-to (c ∷ cs) {x} =
        let
          x' : T false
          x' = subst (λ hole → T hole) (NeverProofs.parse-never≡false cs) x
        in
          ⊥-elim x'


module CharProofs where
  open import Function using (_↔_; mk↔)

  open Spec (Char)

  correct : (c : Char) → Parses (char c) (‘ c)
  correct c input =
    mk↔
      {to = to input}
      {from = from input}
      (
        (λ{ refl → to-from input })
        ,
        (λ{ refl → from-to input })
      )
    where
      open import Data.Char using (_==_; _≟_)
      open import Relation.Nullary.Decidable using (Dec; yes; no; isYes)
      open import Relation.Nullary.Reflects using (Reflects; ofʸ; ofⁿ)

      to-help :
        ∀{c c' : Char} →
        (cs : List Char) →
        (prf : Dec (c ≡ c')) →
        T (parse cs (if isYes prf then always else never)) →
        (‘ c) (c' ∷ cs)
      to-help cs (yes refl) prf rewrite AlwaysProofs.parse→ε cs prf = refl
      to-help cs (no _) prf = ⊥-elim (NeverProofs.parse→ø cs prf)

      to : (input : List Char) → T (parse input (char c)) → (‘ c) input
      to [] = ⊥-elim
      to (c' ∷ cs) prf = to-help cs (c ≟ c') prf

      from-help :
        ∀{c c' : Char} →
        (cs : List Char) →
        (prf : Dec (c ≡ c')) →
        (‘ c) (c' ∷ cs) →
        T (parse cs (if isYes prf then always else never))
      from-help cs (yes refl) refl = tt
      from-help cs (no ¬prf) refl = ¬prf refl

      from : (input : List Char) → (‘ c) input → T (parse input (char c))
      from [] ()
      from (c' ∷ cs) prf = from-help cs (c ≟ c') prf

      to-from : (input : List Char) → {x : (‘ c) input} → to input (from input x) ≡ x
      to-from [] {()}
      to-from (c' ∷ cs) {x} = to-from-help (c ≟ c') x
        where
          to-from-help :
            (prf : Dec (c ≡ c')) →
            (x : (‘ c) (c' ∷ cs)) →
            to-help cs prf (from-help cs prf x) ≡ x
          to-from-help (yes refl) refl = refl
          to-from-help (no ¬prf) refl = ⊥-elim (¬prf refl)

      from-to : (input : List Char) → {x : T (parse input (char c))} → from input (to input x) ≡ x
      from-to [] {()}
      from-to (c' ∷ cs) {x} = from-to-help (c ≟ c') x
        where
          from-to-help :
            (prf : Dec (c ≡ c')) →
            (x : T (parse cs (if isYes prf then always else never))) →
            from-help cs prf (to-help cs prf x) ≡ x
          from-to-help (yes refl) x = case AlwaysProofs.parse→ε cs x of λ{ refl → refl }
          from-to-help (no ¬prf) x = ⊥-elim (NeverProofs.parse→ø cs x)


module UnionProofs where
  open import Function using (_↔_; mk↔)

  open Spec (Char) renaming (_∪_ to _∪ₛ_)

  data These (A B : Set) : Set where
    this : A → These A B
    that : B → These A B
    these : A → B → These A B

  help-nullable :
    ∀{i : ℕ} →
    (a b : Parser i) →
    T (nullable (a ∪ b)) →
    These (T (nullable a)) (T (nullable b))
  help-nullable {_} a b with nullable a | nullable b
  ... | false | false = ⊥-elim
  ... | false | true = that
  ... | true | false = this
  ... | true | true = λ{ tt → these tt tt }

  help-consume :
    (c : Char) →
    (cs : List Char) →
    (a b : Parser (suc (List.length cs))) →
    T (parse cs (consume (a ∪ b) c)) →
    These (T (parse cs (consume a c))) (T (parse cs (consume b c)))
  help-consume c cs a b prf = {!consume (a ∪ b) c!}

  help :
    (a b : ∀{i : ℕ} → Parser i) →
    (input : List Char) →
    T (parse input (a ∪ b)) →
    These (T (parse input a)) (T (parse input b))
  help a b [] = help-nullable {i = 0} a b
  help a b (c ∷ cs) = {!!}

  parse→∪ :
    ∀{a b : ∀{i : ℕ} → Parser i} →
    ∀{aₛ bₛ : Lang} →
    ((s : List Char) → T (parse s a) → aₛ s) →
    ((s : List Char) → T (parse s b) → bₛ s) →
    (input : List Char) →
    T (parse input (a ∪ b)) → (aₛ ∪ₛ bₛ) input
  parse→∪ parse→a parse→b input prf = {!!}

  correct :
    ∀{P Q : Lang} →
    (p : ValidParser P) →
    (q : ValidParser Q) →
    Parses (fst p ∪ fst q) (P ∪ₛ Q)
  correct {aₛ} {bₛ} p q input =
    mk↔
      {to = parse→∪ {!!} {!!} input}
      {from = from input}
      (
        (λ{ refl → to-from input })
        ,
        (λ{ refl → from-to input })
      )
    where
      from : (input : List Char) → (aₛ ∪ₛ bₛ) input → T (parse input (fst p ∪ fst q))
      from [] = {!!}
      from (x ∷ xs) = {!!}

      to-from : (input : List Char) → {x : (aₛ ∪ₛ bₛ) input} → parse→∪ {!!} {!!} input (from input x) ≡ x
      to-from [] = {!!}
      to-from (c ∷ cs) = {!!}

      from-to : (input : List Char) → {x : T (parse input (fst p ∪ fst q))} → from input (parse→∪ {!!} {!!} input x) ≡ x
      from-to [] = {!!}
      from-to (c ∷ cs) {x} = {!!}


ident : ∀{@0 i : ℕ} → Parser i
ident = satisfy isLower · (satisfy isLower ⋆)

arrow : ∀{@0 i : ℕ} → Parser i
arrow = char '-' · char '>'

spaces : ∀{@0 i : ℕ} → Parser i
spaces = char ' ' ⋆

token : ∀{@0 i : ℕ} → Parser i → Parser i
token p = p · spaces

expr : ∀{@0 i : ℕ} → Parser i
expr = fix go
  where
    go : ∀{@0 i : ℕ} → Guard Parser i → Parser i
    go {i} self = lam ∪ app
      where
        lam : Parser i
        lam = token (char '\\') · token ident · (token arrow ·-g self)

        atom : Parser i
        atom =
          token ident ∪
          (token (char '(') ·-g self · token (char ')'))

        app : Parser i
        app = atom · (atom ⋆)

_ : parse (String.toList "\\x -> y") expr ≡ true
_ = refl

_ : parse (String.toList "x (hello) x") expr ≡ true
_ = refl

_ : parse (String.toList "x (hello x") expr ≡ false
_ = refl

_ : parse (String.toList "\\x x -> x") expr ≡ false
_ = refl

_ : parse (String.toList "\\x -> x x") expr ≡ true
_ = refl
