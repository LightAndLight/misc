{-# OPTIONS --safe --erasure #-}
module Category.Indexed where

open import Data.Nat using (ℕ)

_⇒_ : (@0 ℕ → Set) → (@0 ℕ → Set) → (@0 ℕ → Set)
_⇒_ A B i = A i → B i

infixr 50 _⇒_

∀[_] : (@0 ℕ → Set) → Set
∀[ A ] = {@0 i : ℕ} → A i

id : {A : @0 ℕ → Set} → ∀[ A ⇒ A ]
id x = x

_∘_ : {A B C : @0 ℕ → Set} → ∀[ B ⇒ C ] → ∀[ A ⇒ B ] → ∀[ A ⇒ C ]
_∘_ f g x = f (g x)
