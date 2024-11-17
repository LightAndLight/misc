{-# OPTIONS --safe --without-K #-}

module Syntax.Properties where

open import Function using (_∘_)
open import Data.Nat using (ℕ; suc)
open import Relation.Binary.PropositionalEquality as Eq using (_≡_; refl; cong; sym; subst)
open import Relation.Nullary using (¬_)
open Eq.≡-Reasoning

open import Presyntax
open import Presyntax.Properties
open import Presyntax.Properties.Rename
open import Syntax

weaken-∋ :
  {Γ Δ : Ctx} {A X : Term} {f : ℕ → ℕ} →
  ({A : Term} → Γ ∋ A → Δ ∋ rename f A) →
  (Γ' : Ctx) →
  (ix : (Γ ++ Γ') ∋ A) → (Δ , X ++ rename-ctx (suc ∘ f) Γ') ∋ rename (underₙ Γ' (suc ∘ f)) A
weaken-∋ {A = A} g ◆ ix = there (rename-∘ A) (g ix)
weaken-∋ {A = A} {f = f} g (ctx , Y) (here r) =
  here (
    begin
      rename suc (rename (underₙ ctx (suc ∘ f)) Y)
    ≡⟨ rename-∘ Y ⟩
      rename (suc ∘ underₙ ctx (suc ∘ f)) Y
    ≡⟨⟩
      rename (under (underₙ ctx (suc ∘ f)) ∘ suc) Y
    ≡⟨ rename-cong (λ ix → sym (underₙ-under-comm ctx (suc ix))) Y ⟩
      rename (underₙ ctx (under (suc ∘ f)) ∘ suc) Y
    ≡⟨ sym (rename-∘ Y) ⟩
      rename (underₙ ctx (under (suc ∘ f))) (rename suc Y)
    ≡⟨ cong (λ x → rename (underₙ ctx (under (suc ∘ f))) x) r ⟩
      rename (underₙ ctx (under (suc ∘ f))) A
    ∎
  )
weaken-∋ {X = X} {f = f} g (ctx , Y) (there {A} {A'} {B} r ix) = 
  let rec = weaken-∋ {X = X} g ctx ix in
  there
    (
      begin
        rename suc (rename (underₙ ctx (suc ∘ f)) A')
      ≡⟨ rename-∘ A' ⟩
        rename (suc ∘ underₙ ctx (suc ∘ f)) A'
      ≡⟨⟩
        rename (under (underₙ ctx (suc ∘ f)) ∘ suc) A'
      ≡⟨ rename-cong (λ ix → sym (underₙ-under-comm ctx (suc ix))) A' ⟩
        rename (underₙ ctx (under (suc ∘ f)) ∘ suc) A'
      ≡⟨ sym (rename-∘ A')  ⟩
        rename (underₙ ctx (under (suc ∘ f))) (rename suc A')
      ≡⟨ cong (rename (underₙ ctx (under (suc ∘ f)))) r ⟩
        rename (underₙ ctx (under (suc ∘ f))) B
      ∎
    )
    rec

weaken1-∋ :
  {Γ : Ctx} {A X : Term} →
  Γ ∋ A →
  (Γ , X) ∋ rename suc A
weaken1-∋ ix = there refl ix

under-∋ :
  {Γ Δ : Ctx} {f : ℕ → ℕ} {A X : Term}  →
  ({A : Term} → Γ ∋ A → Δ ∋ rename f A) →
  Γ , X ∋ A →
  Δ , rename f X ∋ rename (under f) A
under-∋ {f = f} g (here {A = Y} {A' = Y'} r) =
  here
    (
      begin
        rename suc (rename f Y)
      ≡⟨ rename-∘ {f = suc} Y ⟩
        rename (suc ∘ f) Y
      ≡⟨⟩
        rename (under f ∘ suc) Y
      ≡⟨ sym (rename-∘ {f = under f} Y) ⟩
        rename (under f) (rename suc Y)
      ≡⟨ cong (rename (under f)) r ⟩
        rename (under f) Y'
      ∎
    )
under-∋ {f = f} g (there {A = Y} {A' = Y'} r ix) =
  there
    (begin
      rename suc (rename f Y)
      ≡⟨ rename-∘ {f = suc} Y ⟩
        rename (suc ∘ f) Y
      ≡⟨⟩
        rename (under f ∘ suc) Y
      ≡⟨ sym (rename-∘ {f = under f} Y) ⟩
        rename (under f) (rename suc Y)
      ≡⟨ cong (rename (under f)) r ⟩
        rename (under f) Y'
      ∎
    )
    (g ix)

module _ where
  _ : ◆ , {- A : -} Type {- Type -}, {- x : -} var 0 {- A(#0) -} ∋ Type {- Type -}
  _ = there refl (here refl) {- A(#1) -}

  _ : ◆ , {- A : -} Type {- Type -}, {- x : -} var 0 {- A(#0) -} ∋ var 1 {- A(#1) -}
  _ = here refl {- x(#0) -}

  _ : ◆ , {- A : -} Type {- Type -}, {- x : -} var 0 {- A(#0) -}, {- y : -} var 1 {-  A(#1) -} ∋ Type {- Type -}
  _ = there refl (there refl (here refl)) {- A(#2) -}

  _ : ◆ , {- A : -} Type {- Type -}, {- x : -} var 0 {- A(#0) -}, {- y : -} var 1 {-  A(#1) -} ∋ var 2 {- A(#2) -}
  _ = there refl (here refl) {- x(#1) -}

∋-to-ℕ-≡ :
  {Γ : Ctx} {A B : Term} →
  (prf : A ≡ B) →
  (ix : Γ ∋ A) →
  ∋-to-ℕ (subst (λ x → Γ ∋ x) prf ix) ≡ ∋-to-ℕ ix
∋-to-ℕ-≡ refl ix = refl

¬Type∶Type : {Γ : Ctx} → ¬ (Γ ⊢ Type ∶ Type)
¬Type∶Type ()
