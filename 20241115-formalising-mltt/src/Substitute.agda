{-# OPTIONS --safe --without-K #-}

module Substitute where

open import Function using (_∘_)
open import Data.Nat using (ℕ; zero; suc; pred)
open import Data.Sum using (_⊎_; inj₁; inj₂)
open import Relation.Binary.PropositionalEquality as Eq using (_≡_; refl; cong; subst; sym)
open Eq.≡-Reasoning

open import Presyntax
open import Presyntax.Properties
open import Presyntax.Properties.Substitute
open import Rename
open import Syntax

record Substitution (Γ Δ : Ctx) : Set where
  field
    f : ℕ → Term
    run : {A : Term} → (ix : Γ ∋ A) → Δ ⊢ f (∋-to-ℕ ix) ∶ substitute f A

Substitution-id : {Γ : Ctx} → Substitution Γ Γ
Substitution-id {Γ} =
  record {
    f = var
    ;
    run = λ {A} ix →
      subst
        (λ x → Γ ⊢ var (∋-to-ℕ ix) ∶ x)
        (sym (substitute-var (λ _ → refl) A))
        (⊢-var ix)
  }

Substitution-inst1 :
  {Γ : Ctx} {a A : Term} →
  Γ ⊢ a ∶ A →
  Substitution (Γ , A) Γ
Substitution-inst1 {Γ} {a} {A} a∶A =
  record {
    f = inst1 a
    ;
    run = λ {B} → λ {
      (here r) →
        subst
          (λ x → Γ ⊢ a ∶ x)
          (sym (
            begin
              substitute (inst1 a) B
            ≡⟨ cong (substitute (inst1 a)) (sym r) ⟩
              substitute (inst1 a) (rename suc A)
            ≡⟨ sym (substitute∘rename (λ _ → refl) A) ⟩
              substitute (inst1 a ∘ suc) A
            ≡⟨⟩
              substitute var A
            ≡⟨ substitute-var (λ _ → refl) A ⟩
              A
            ∎))
          a∶A
      ;
      (there {X} {X'} {Y} r ix) → 
        subst
          (λ x → Γ ⊢ var (∋-to-ℕ ix) ∶ x)
          (sym (
            begin
              substitute (inst1 a) B
            ≡⟨ cong (substitute (inst1 a)) (sym r) ⟩
              substitute (inst1 a) (rename suc X')
            ≡⟨ sym (substitute∘rename (λ _ → refl) X') ⟩
              substitute (inst1 a ∘ suc) X'
            ≡⟨⟩
              substitute var X'
            ≡⟨ substitute-var (λ _ → refl) X' ⟩
              X'
            ∎))
          (⊢-var ix)
      }
  }

Substitution-under :
  {Γ Δ : Ctx} {X : Term} →
  (s : Substitution Γ Δ) →
  Substitution (Γ , X) (Δ , substitute (Substitution.f s) X)
Substitution-under {Δ = Δ} {X} s =
  record {
    f = sunder (Substitution.f s)
    ;
    run = λ {A} → λ {
      (here r) → ⊢-var (here (
        begin
          rename suc (substitute (Substitution.f s) X)
        ≡⟨ rename∘substitute X ⟩
          substitute (rename suc ∘ Substitution.f s) X
        ≡⟨⟩
          substitute (sunder (Substitution.f s) ∘ suc) X
        ≡⟨ substitute∘rename (λ _ → refl) X ⟩
          substitute (sunder (Substitution.f s)) (rename suc X)
        ≡⟨ cong (substitute (sunder (Substitution.f s))) r ⟩
          substitute (sunder (Substitution.f s)) A
        ∎))
      ;
      (there {Y} {Y'} r ix) → 
        let
          tm' = Substitution.run s ix

          tm'' :
            Δ , substitute (Substitution.f s) X ⊢
            rename suc (Substitution.f s (∋-to-ℕ ix)) ∶
            rename suc (substitute (Substitution.f s) Y')
          tm'' = ⊢-weaken1 {X = substitute (Substitution.f s) X} tm'

          goal :
            Δ , substitute (Substitution.f s) X ⊢
            rename suc (Substitution.f s (∋-to-ℕ ix)) ∶
            substitute (sunder (Substitution.f s)) A
          goal =
            subst
              (λ x →
                Δ , substitute (Substitution.f s) X ⊢
                rename suc (Substitution.f s (∋-to-ℕ ix)) ∶
                x
              )
              (begin
                rename suc (substitute (Substitution.f s) Y')
              ≡⟨ rename∘substitute Y' ⟩
                substitute (rename suc ∘ Substitution.f s) Y'
              ≡⟨⟩
                substitute (sunder (Substitution.f s) ∘ suc) Y'
              ≡⟨ substitute∘rename (λ _ → refl) Y' ⟩
                substitute (sunder (Substitution.f s)) (rename suc Y')
              ≡⟨ cong (substitute (sunder (Substitution.f s))) r ⟩
                substitute (sunder (Substitution.f s)) A
              ∎)
              tm''
        in
        goal
    }
  }

mutual
  ⊢-type-substitute :
    {Γ Δ : Ctx} {A : Term} →
    (s : Substitution Γ Δ) →
    Γ ⊢ A type →
    Δ ⊢ substitute (Substitution.f s) A type
  ⊢-type-substitute s (⊢-El tm) = ⊢-El (⊢-substitute s tm)
  ⊢-type-substitute s ⊢-Type = ⊢-Type

  ⊢-substitute :
    {Γ Δ : Ctx} {a A : Term} →
    (s : Substitution Γ Δ) →
    Γ ⊢ a ∶ A →
    Δ ⊢ substitute (Substitution.f s) a ∶ substitute (Substitution.f s) A
  ⊢-substitute s (⊢-var ix) = Substitution.run s ix
  ⊢-substitute s ⊢-Void = ⊢-Void
  ⊢-substitute s ⊢-Unit = ⊢-Unit
  ⊢-substitute s (⊢-Pi Atype Btype) =
    ⊢-Pi (⊢-type-substitute s Atype) (⊢-type-substitute (Substitution-under s) Btype)
  ⊢-substitute s (⊢-Sigma Atype Btype) =
    ⊢-Sigma (⊢-type-substitute s Atype) (⊢-type-substitute (Substitution-under s) Btype)
  ⊢-substitute s (⊢-lam Atype e∶B) =
    ⊢-lam (⊢-type-substitute s Atype) (⊢-substitute (Substitution-under s) e∶B)
  ⊢-substitute s (⊢-app {A} {B} {B'} {x = x} prf f∶PiAB x∶A) = 
    ⊢-app s' (⊢-substitute s f∶PiAB) (⊢-substitute s x∶A)
    where
      s' : (substitute (sunder (Substitution.f s)) B [ substitute (Substitution.f s) x ]) ≡ substitute (Substitution.f s) B'
      s' = 
        begin
          (substitute (sunder (Substitution.f s)) B [ substitute (Substitution.f s) x ])
        ≡⟨ substitute-a[] B ⟩
          substitute (Substitution.f s) (B [ x ])
        ≡⟨ cong (substitute (Substitution.f s)) prf ⟩
          substitute (Substitution.f s) B'
        ∎

Substitution-∘ : {Γ Γ' Γ'' : Ctx} → Substitution Γ' Γ'' → Substitution Γ Γ' → Substitution Γ Γ''
Substitution-∘ {Γ'' = Γ''} s2 s1 =
  record {
    f = λ ix → substitute (Substitution.f s2) (Substitution.f s1 ix)
    ;
    run = λ {A} ix →
      let
        tm' = Substitution.run s1 ix

        tm'' :
          Γ'' ⊢
          substitute (Substitution.f s2) (Substitution.f s1 (∋-to-ℕ ix)) ∶
          substitute (Substitution.f s2) (substitute (Substitution.f s1) A)
        tm'' = ⊢-substitute s2 tm'

        goal :
          Γ'' ⊢
          substitute (Substitution.f s2) (Substitution.f s1 (∋-to-ℕ ix)) ∶
          substitute (substitute (Substitution.f s2) ∘ Substitution.f s1) A
        goal = 
          subst
            (λ x → Γ'' ⊢ substitute (Substitution.f s2) (Substitution.f s1 (∋-to-ℕ ix)) ∶ x)
            (substitute-∘ A)
            tm''
      in
      goal
  }
