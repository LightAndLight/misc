{-# OPTIONS --safe --erasure #-}
open import Data.Nat using (ℕ)

module Functor.Indexed (F : (@0 ℕ → Set) → (@0 ℕ → Set)) where

open import Category.Indexed using (∀[_]; _⇒_)

record IFunctor : Set1 where
  field
    ifmap :
      {A B : @0 ℕ → Set} →
      ∀[ A ⇒ B ] →
      ∀[ F A ⇒ F B ]

record IFunctorEquiv {F-IFunctor : IFunctor} : Set1 where
  field
    Equiv : {A : @0 ℕ → Set} → ({@0 i : ℕ} → A i → A i → Set) → {@0 i : ℕ} → F A i → F A i → Set
