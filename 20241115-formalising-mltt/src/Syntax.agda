{-# OPTIONS --safe --without-K #-}

module Syntax where

open import Data.Nat using (ℕ; zero; suc)
open import Relation.Binary.PropositionalEquality using (_≡_)

open import Presyntax

data _∋_ : Ctx → Term → Set where
  here :
    {Γ : Ctx} {A A' : Term} →
    (r : rename suc A ≡ A') →
    Γ , A ∋ A'

  there :
    {Γ : Ctx} {A A' B : Term} →
    (r : rename suc A ≡ A') →
    Γ ∋ A →
    (Γ , B) ∋ A'

∋-to-ℕ : {Γ : Ctx} {A : Term} → Γ ∋ A → ℕ
∋-to-ℕ (here _) = zero
∋-to-ℕ (there _ ix) = suc (∋-to-ℕ ix)

mutual
  data ⊢_ctx : Ctx → Set where
    ⊢-◆ : ⊢ ◆ ctx
    ⊢-, :
      {Γ : Ctx} {A : Term} →
      ⊢ Γ ctx →
      Γ ⊢ A type →
      ⊢ Γ , A ctx

  infix 50 ⊢_ctx

  data _⊢_type : Ctx → Term → Set where
    ⊢-El :
      {Γ : Ctx} {A : Term} →
      Γ ⊢ A ∶ Type →
      Γ ⊢ A type

    ⊢-Type :
      {Γ : Ctx} →
      Γ ⊢ Type type

  infix 50 _⊢_type

  data _⊢_∶_ (Γ : Ctx) : Term → Term → Set where
    ⊢-var :
      {A : Term} →
      (ix : Γ ∋ A) →
      Γ ⊢ var (∋-to-ℕ ix) ∶ A

    ⊢-Void : Γ ⊢ Void ∶ Type

    ⊢-Unit : Γ ⊢ Unit ∶ Type

    ⊢-Pi :
      {A B : Term} →
      (Atype : Γ ⊢ A type) →
      (Btype : Γ , A ⊢ B type) →
      Γ ⊢ Pi A B ∶ Type

    ⊢-Sigma :
      {A B : Term} →
      (Atype : Γ ⊢ A type) →
      (Btype : Γ , A ⊢ B type) →
      Γ ⊢ Sigma A B ∶ Type

    ⊢-lam :
      {A B e : Term} →
      (Atype : Γ ⊢ A type) →
      (e∶B : Γ , A ⊢ e ∶ B) →
      Γ ⊢ lam A e ∶ Pi A B

    ⊢-app :
      {A B B' f x : Term} →
      B [ x ] ≡ B' →
      Γ ⊢ f ∶ Pi A B →
      Γ ⊢ x ∶ A →
      Γ ⊢ app f x ∶ B'

  infix 50 _⊢_∶_
