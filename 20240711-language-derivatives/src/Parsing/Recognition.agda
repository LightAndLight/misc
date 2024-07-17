{-# OPTIONS --safe --erasure #-}
module Parsing.Recognition where

open import Data.Bool using (Bool; true; false; if_then_else_; _∨_)
open import Data.Char using (Char)
open import Data.List as List using (List; []; _∷_)
open import Data.Nat using (ℕ; suc)
open import Relation.Binary.PropositionalEquality using (_≡_; refl)

data Parser : @0 ℕ → Set where
  parser :
    ∀{@0 i : ℕ} →
    Bool →
    (∀{@0 j : ℕ} → @0 suc j ≡ i → Char → Parser j) →
    Parser i

nullable : {@0 i : ℕ} → Parser i → Bool
nullable (parser b _) = b

consume : {@0 i : ℕ} → Parser (suc i) → Char → Parser i
consume (parser _ f) c = f refl c

parse : (input : List Char) → Parser (List.length input) → Bool
parse [] p = nullable p
parse (x ∷ xs) p = parse xs (consume p x)


never : {@0 i : ℕ} → Parser i
never {i} = parser false λ{ {j} refl c → never {j} }

always : {@0 i : ℕ} → Parser i
always {i} = parser true λ{ {j} refl c → never {j} }

char : {@0 i : ℕ} → Char → Parser i
char {i} c = parser false λ{ {j} refl c' → if c == c' then always {j} else never {j} }
  where
    open import Data.Char using (_==_)

satisfy : {@0 i : ℕ} → (Char → Bool) → Parser i
satisfy f = parser false λ{ {j} refl c → if f c then always {j} else never {j} }

_∪_ : ∀{@0 i : ℕ} → Parser i → Parser i → Parser i
a ∪ b =
  parser
    (nullable a ∨ nullable b)
    (λ{ {j} refl c → consume a c ∪ consume b c })

weaken : ∀{@0 i : ℕ} → Parser (suc i) → Parser i
weaken (parser b f) = parser b λ{ {j} refl c → weaken {j} (f refl c) }

_·_ : ∀{@0 i : ℕ} → Parser i → Parser i → Parser i
_·_ a b with nullable a
... | false = parser false λ{ refl c → consume a c · weaken b }
... | true = parser (nullable b) λ{ refl c → consume b c ∪ (consume a c · weaken b) }
  {-
  The above verision is essential for performance.

  Original:

  parser
    (nullable a ∧ nullable b)
    (λ{ {j} refl c →
      (consume a c · weaken b)
      ∪
      ((if nullable a then always else never) · consume b c)
    })
  -}

infixl 50 _·_

_⋆ : ∀{@0 i : ℕ} → Parser i → Parser i
a ⋆ = parser true λ{ {j} refl c → consume a c · (weaken a ⋆) }


Guard : (@0 ℕ → Set) → @0 ℕ → Set
Guard A i = ∀{@0 j : ℕ} → @0 suc j ≡ i → A j

extract : ∀{A : @0 ℕ → Set} → (∀{@0 i : ℕ} → Guard A i) → ∀{@0 i : ℕ} → A i
extract g {i} = g {i = suc i} {j = i} refl

fix-guard : (∀{@0 i : ℕ} → Guard Parser i → Parser i) → ∀{@0 i : ℕ} → Guard Parser i
fix-guard f {i} {j} refl = f {i = j} (fix-guard f {j})

fix : (∀{@0 i : ℕ} → Guard Parser i → Parser i) → ∀{@0 i : ℕ} → Parser i
fix f = extract (fix-guard f)

_·-g_ : ∀{@0 i : ℕ} → Parser i → Guard Parser i → Parser i
_·-g_ a b = parser (nullable a) λ{ {j} refl c → consume a c · b refl }

infix 55 _·-g_

module Tests where
  import Data.String as String

  _ : parse (String.toList "hello") always ≡ false
  _ = refl

  _ : parse (String.toList "") always ≡ true
  _ = refl

  language₁ : ∀{i : ℕ} → Parser i
  language₁ = char 'h' · char 'e' · char 'l' · char 'l' · char 'o'

  _ : parse (String.toList "hello") language₁ ≡ true
  _ = refl

  _ : parse (String.toList "hell") language₁ ≡ false
  _ = refl

  _ : parse (String.toList "helloo") language₁ ≡ false
  _ = refl

  language₂ : ∀{i : ℕ} → Parser i
  language₂ = char 'h' · char 'e' · char 'l' · char 'l' · (always ∪ char 'o')

  _ : parse (String.toList "hello") language₂ ≡ true
  _ = refl

  _ : parse (String.toList "hell") language₂ ≡ true
  _ = refl

  _ : parse (String.toList "helloo") language₂ ≡ false
  _ = refl

  language₃ : ∀{i : ℕ} → Parser i
  language₃ = char 'h' · char 'e' · char 'l' · char 'l' · (char 'o' ⋆)

  _ : parse (String.toList "hello") language₃ ≡ true
  _ = refl

  _ : parse (String.toList "hell") language₃ ≡ true
  _ = refl

  _ : parse (String.toList "helloo") language₃ ≡ true
  _ = refl

  _ : parse (String.toList "herloo") language₃ ≡ false
  _ = refl

  _ : parse (String.toList "helloo!") language₃ ≡ false
  _ = refl

  language₄ : ∀{i : ℕ} → Parser i
  language₄ = (char 'h' · char 'e' · char 'l' · char 'l' · char 'o') ⋆

  _ : parse (String.toList "") language₄ ≡ true
  _ = refl

  _ : parse (String.toList "hello") language₄ ≡ true
  _ = refl

  _ : parse (String.toList "hellohello") language₄ ≡ true
  _ = refl

  _ : parse (String.toList "hellohellohello") language₄ ≡ true
  _ = refl

  _ : parse (String.toList "helloherrohello") language₄ ≡ false
  _ = refl

  _ : parse (String.toList "hellohellohellx") language₄ ≡ false
  _ = refl

  language₅ : ∀{i : ℕ} → Parser i
  language₅ = (fix λ self → always ∪ (char 'a' ·-g self)) · char 'b'

  _ : parse (String.toList "b") language₅ ≡ true
  _ = refl

  _ : parse (String.toList "ab") language₅ ≡ true
  _ = refl

  _ : parse (String.toList "aaab") language₅ ≡ true
  _ = refl

  _ : parse (String.toList "aaac") language₅ ≡ false
  _ = refl

  language₆ : ∀{i : ℕ} → Parser i
  language₆ = (fix λ self → always ·-g self)

  _ : parse (String.toList "") language₆ ≡ true
  _ = refl
