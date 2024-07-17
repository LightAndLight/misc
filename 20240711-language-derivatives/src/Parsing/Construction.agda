{-# OPTIONS --safe --erasure #-}
module Parsing.Construction where

open import Data.Bool using (Bool; true; false; if_then_else_; _∨_)
open import Data.Char using (Char)
open import Data.List as List using (List; []; _∷_; [_]; _++_)
open import Data.Maybe using (Maybe; just; nothing)
open import Data.Nat using (ℕ; suc)
open import Data.Product using (_×_; _,_) renaming (proj₁ to fst; proj₂ to snd)
open import Data.Unit using (⊤; tt)
open import Relation.Binary.PropositionalEquality using (_≡_; refl)

open import Codata.Indexed.Thunk using (■; delay; undelay)
import Equality.Erased as Erased

data Parser (A : Set) (@0 i : ℕ) : Set where
  parser :
    (results : List A) →
    ■ (λ j → Char → Parser A j) i →
    Parser A i

nullable : {A : Set} {@0 i : ℕ} → Parser A i → List A
nullable (parser b _) = b

consume : {A : Set} {@0 i : ℕ} → Parser A (suc i) → Char → Parser A i
consume (parser _ f) c = undelay f Erased.refl c

parse : {A : Set} → (input : List Char) → Parser A (List.length input) → List A
parse [] p = nullable p
parse (x ∷ xs) p = parse xs (consume p x)

never : {A : Set} {@0 i : ℕ} → Parser A i
never {i} = parser [] (delay λ{ {j} Erased.refl c → never {i = j} })

always : {A : Set} {@0 i : ℕ} → A → Parser A i
always {i} a = parser [ a ] (delay λ{ {j} Erased.refl c → never {i = j} })

char : {@0 i : ℕ} → Char → Parser ⊤ i
char {i} c = parser [] (delay λ{ {j} Erased.refl c' → if c == c' then always {i = j} tt else never {i = j} })
  where
    open import Data.Char using (_==_)

satisfy : {@0 i : ℕ} → (Char → Bool) → Parser Char i
satisfy f = parser [] (delay λ{ {j} Erased.refl c → if f c then always {i = j} c else never {i = j} })

_∪_ : {A : Set} {@0 i : ℕ} → Parser A i → Parser A i → Parser A i
a ∪ b =
  parser
    (nullable a ++ nullable b)
    (delay (λ{ {j} Erased.refl c → consume a c ∪ consume b c }))

weaken : {A : Set} {@0 i : ℕ} → Parser A (suc i) → Parser A i
weaken (parser b f) = parser b (delay λ{ {j} Erased.refl c → weaken {i = j} (undelay f Erased.refl c) })

map :
  {A B : Set} {@0 i : ℕ} →
  (A → B) →
  Parser A i →
  Parser B i
map func (parser b f) = 
  parser (List.map func b) (delay λ{ {j} Erased.refl c → map {i = j} func (undelay f Erased.refl c) })

_·_ : {A B : Set} {@0 i : ℕ} → Parser A i → Parser B i → Parser (A × B) i
_·_ a b with nullable a
... | [] = parser [] (delay λ{ Erased.refl c → consume a c · weaken b })
... | a-result ∷ a-results =
  parser
    (List.cartesianProduct (nullable a) (nullable b))
    (delay λ{ Erased.refl c →
      let b' = consume b c in
      List.foldl
        (λ acc a-result → acc ∪ map (λ b → a-result , b) b')
        (map (λ b → a-result , b) b')
        a-results
      ∪
      (consume a c · weaken b)
    })

infixl 50 _·_

_*>_ : {A B : Set} {@0 i : ℕ} → Parser A i → Parser B i → Parser B i
_*>_ a b with nullable a
... | [] = parser [] (delay λ{ Erased.refl c → map snd (consume a c · weaken b) })
... | _ ∷ _ =
  parser
    (nullable b)
    (delay λ{ Erased.refl c → (consume b c) ∪ map snd (consume a c · weaken b) })

infixl 50 _*>_

_<*_ : {A B : Set} {@0 i : ℕ} → Parser A i → Parser B i → Parser A i
_<*_ a b with nullable a
... | [] = parser [] (delay λ{ Erased.refl c → map fst (consume a c · weaken b) })
... | a-result ∷ a-results =
  parser
    (nullable a)
    (delay λ{ Erased.refl c →
      let b' = consume b c in
      List.foldl
        (λ acc a-result → acc ∪ map (λ _ → a-result) b')
        (map (λ _ → a-result) b')
        a-results
      ∪
      map fst (consume a c · weaken b)
    })

infixl 50 _<*_

_⋆ : {A : Set} {@0 i : ℕ} → Parser A i → Parser (List A) i
a ⋆ = parser [ [] ] (delay λ{ {j} Erased.refl c → map (λ{ (x , xs) → x ∷ xs}) (consume a c · (weaken a ⋆)) })


Guard : (@0 ℕ → Set) → @0 ℕ → Set
Guard A i = ∀{@0 j : ℕ} → suc j Erased.≡ i → A j

extract : {A : @0 ℕ → Set} → (∀{@0 i : ℕ} → Guard A i) → {@0 i : ℕ} → A i
extract g {i} = g {i = suc i} {j = i} Erased.refl

fix-guard : {A : Set} → ({@0 i : ℕ} → Guard (Parser A) i → Parser A i) → {@0 i : ℕ} → Guard (Parser A) i
fix-guard f {i} {j} Erased.refl = f {i = j} (fix-guard f {j})

fix : {A : Set} → ({@0 i : ℕ} → Guard (Parser A) i → Parser A i) → {@0 i : ℕ} → Parser A i
fix f = extract (fix-guard f)

_·-g_ : {A B : Set} {@0 i : ℕ} → Parser A i → Guard (Parser B) i → Parser (A × Maybe B) i
_·-g_ a b = parser (List.map (_, nothing) (nullable a)) (delay λ{ {j} Erased.refl c → consume a c · map just (b Erased.refl) })

infix 55 _·-g_

_‘·-g_ : {B : Set} {@0 i : ℕ} → Char → Guard (Parser B) i → Parser B i
_‘·-g_ c b = parser [] (delay λ{ {j} Erased.refl c' → if c == c' then b Erased.refl else never })
  where
    open import Data.Char using (_==_)

infix 55 _‘·-g_

private module Tests where
  import Data.String as String

  _ : parse (String.toList "hello") (always 99) ≡ []
  _ = refl

  _ : parse (String.toList "") (always 99) ≡ [ 99 ]
  _ = refl

  language₁ : ∀{i : ℕ} → Parser ⊤ i
  language₁ = char 'h' *> char 'e' *> char 'l' *> char 'l' *> char 'o'

  _ : parse (String.toList "hello") language₁ ≡ [ tt ]
  _ = refl

  _ : parse (String.toList "hell") language₁ ≡ []
  _ = refl

  _ : parse (String.toList "helloo") language₁ ≡ []
  _ = refl

  language₂ : ∀{i : ℕ} → Parser ⊤ i
  language₂ = char 'h' *> char 'e' *> char 'l' *> char 'l' *> (always tt ∪ char 'o')

  _ : parse (String.toList "hello") language₂ ≡ [ tt ]
  _ = refl

  _ : parse (String.toList "hell") language₂ ≡ [ tt ]
  _ = refl

  _ : parse (String.toList "helloo") language₂ ≡ []
  _ = refl

  language₃ : ∀{i : ℕ} → Parser ⊤ i
  language₃ = char 'h' *> char 'e' *> char 'l' *> char 'l' <* (char 'o' ⋆)

  _ : parse (String.toList "hello") language₃ ≡ [ tt ]
  _ = refl

  _ : parse (String.toList "hell") language₃ ≡ [ tt ]
  _ = refl

  _ : parse (String.toList "helloo") language₃ ≡ [ tt ]
  _ = refl

  _ : parse (String.toList "herloo") language₃ ≡ []
  _ = refl

  _ : parse (String.toList "helloo!") language₃ ≡ []
  _ = refl

  language₄ : ∀{i : ℕ} → Parser (List ⊤) i
  language₄ = (char 'h' *> char 'e' *> char 'l' *> char 'l' *> char 'o') ⋆

  _ : parse (String.toList "") language₄ ≡ [ [] ]
  _ = refl

  _ : parse (String.toList "hello") language₄ ≡ [ tt ∷ [] ]
  _ = refl

  _ : parse (String.toList "hellohello") language₄ ≡ [ tt ∷ tt ∷ [] ]
  _ = refl

  _ : parse (String.toList "hellohellohello") language₄ ≡ [ tt ∷ tt ∷ tt ∷ [] ]
  _ = refl

  _ : parse (String.toList "helloherrohello") language₄ ≡ []
  _ = refl

  _ : parse (String.toList "hellohellohellx") language₄ ≡ []
  _ = refl

  language₅ : ∀{i : ℕ} → Parser ⊤ i
  language₅ = (fix λ self → always tt ∪ (map (λ{ (x , y) → x }) (char 'a' ·-g self))) *> char 'b'

  _ : parse (String.toList "b") language₅ ≡ [ tt ]
  _ = refl

  _ : parse (String.toList "ab") language₅ ≡ [ tt ]
  _ = refl

  _ : parse (String.toList "aaab") language₅ ≡ [ tt ]
  _ = refl

  _ : parse (String.toList "aaac") language₅ ≡ []
  _ = refl

  language₆ : ∀{i : ℕ} → Parser ⊤ i
  language₆ = (fix λ self → map (λ{ (x , y) → x }) (always tt ·-g self))

  _ : parse (String.toList "") language₆ ≡ [ tt ]
  _ = refl
