{-# OPTIONS --safe --without-K #-}

module Rename where

open import Function using (_∘_)
open import Data.Nat using (ℕ; zero; suc)
open import Relation.Binary.PropositionalEquality as Eq using (_≡_; refl; cong; sym; subst)
open Eq.≡-Reasoning

open import Presyntax
open import Presyntax.Properties
open import Presyntax.Properties.Rename
open import Presyntax.Properties.Substitute
open import Syntax
open import Syntax.Properties

record Renaming (Γ Δ : Ctx) : Set where
  field
    f : ℕ → ℕ
    run : {A : Term} → Γ ∋ A → Δ ∋ rename f A
    correct : {A : Term} → (ix : Γ ∋ A) → ∋-to-ℕ (run ix) ≡ f (∋-to-ℕ ix)

∋-to-ℕ-weaken :
  {Γ Δ : Ctx} {A X : Term} →
  (r : Renaming Γ Δ) →
  (Γ' : Ctx) →
  (ix : (Γ ++ Γ') ∋ A) →
  ∋-to-ℕ (weaken-∋ {Γ} {X = X} (Renaming.run r) Γ' ix) ≡ underₙ Γ' (suc ∘ Renaming.f r) (∋-to-ℕ ix)
∋-to-ℕ-weaken r ◆ ix = cong suc (Renaming.correct r ix)
∋-to-ℕ-weaken r (ctx , x) (here prf) = sym (underₙ-zero refl ctx)
∋-to-ℕ-weaken r (ctx , x) (there prf ix) =
  begin
    suc (∋-to-ℕ (weaken-∋ (Renaming.run r) ctx ix))
  ≡⟨ cong suc ((∋-to-ℕ-weaken r ctx ix)) ⟩
    suc (underₙ ctx (suc ∘ Renaming.f r) (∋-to-ℕ ix))
  ≡⟨⟩
    under (underₙ ctx (suc ∘ Renaming.f r)) (suc (∋-to-ℕ ix))
  ≡⟨ sym (underₙ-under-comm ctx (suc (∋-to-ℕ ix))) ⟩
    underₙ ctx (under (suc ∘ Renaming.f r)) (suc (∋-to-ℕ ix))
  ∎

∋-to-ℕ-under :
  {Γ Δ : Ctx} {A X : Term} →
  (r : Renaming Γ Δ) →
  (ix : Γ , X ∋ A) →
  ∋-to-ℕ (under-∋ (Renaming.run r) ix) ≡ under (Renaming.f r) (∋-to-ℕ ix)
∋-to-ℕ-under r (here _) = refl
∋-to-ℕ-under r (there _ ix) = cong suc (Renaming.correct r ix)

Renaming-id : {Γ : Ctx} → Renaming Γ Γ
Renaming-id {Γ} =
  record {
    f = λ x → x
    ;
    run = λ {A} tm → subst (λ x → Γ ∋ x) (sym (rename-id refl A)) tm
    ;
    correct = λ {A} → ∋-to-ℕ-≡ (sym (rename-id refl A))
  }

Renaming-∘ : {Γ Γ' Γ'' : Ctx} → Renaming Γ' Γ'' → Renaming Γ Γ' → Renaming Γ Γ''
Renaming-∘ {Γ'' = Γ''} r2 r1 =
  record {
    f = Renaming.f r2 ∘ Renaming.f r1
    ;
    run = λ {A} ix →
      let
        ix' = Renaming.run r1 ix
        ix'' = Renaming.run r2 ix'
      in
        subst (λ x → Γ'' ∋ x) (rename-∘ {f = Renaming.f r2} {g = Renaming.f r1} A) ix''
    ;
    correct = λ {A} ix → 
      begin
        ∋-to-ℕ (subst (λ x → Γ'' ∋ x) (rename-∘ A) (Renaming.run r2 (Renaming.run r1 ix)))
      ≡⟨ ∋-to-ℕ-≡ (rename-∘ A) _ ⟩
        ∋-to-ℕ (Renaming.run r2 (Renaming.run r1 ix))
      ≡⟨ Renaming.correct r2 _ ⟩
        Renaming.f r2 (∋-to-ℕ (Renaming.run r1 ix))
      ≡⟨ cong (Renaming.f r2) (Renaming.correct r1 _) ⟩
        Renaming.f r2 (Renaming.f r1 (∋-to-ℕ ix))
      ∎
  }

Renaming-under :
  {Γ Δ : Ctx} {X : Term} →
  (r : Renaming Γ Δ) →
  Renaming (Γ , X) (Δ , rename (Renaming.f r) X)
Renaming-under r =
  record {
    f = under (Renaming.f r)
    ;
    run = under-∋ (Renaming.run r)
    ;
    correct = ∋-to-ℕ-under r
  }

Renaming-weaken :
  {Γ Δ : Ctx} {X : Term} →
  (r : Renaming Γ Δ) →
  (Γ' : Ctx) →
  Renaming (Γ ++ Γ') ((Δ , X) ++ rename-ctx (suc ∘ Renaming.f r) Γ')
Renaming-weaken {Γ} {X} r Γ' = 
  record {
    f = underₙ Γ' (suc ∘ Renaming.f r)
    ;
    run = weaken-∋ (Renaming.run r) Γ'
    ;
    correct = ∋-to-ℕ-weaken r Γ'
  }

Renaming-weaken1 : {Γ : Ctx} {X : Term} → Renaming Γ (Γ , X)
Renaming-weaken1 = Renaming-weaken Renaming-id ◆

mutual
  ⊢-type-rename :
    {Γ Δ : Ctx} {A : Term} →
    (r : Renaming Γ Δ) →
    Γ ⊢ A type →
    Δ ⊢ rename (Renaming.f r) A type
  ⊢-type-rename r (⊢-El tm) = ⊢-El (⊢-rename r tm)
  ⊢-type-rename r ⊢-Type = ⊢-Type

  ⊢-rename :
    {Γ Δ : Ctx} {a A : Term} →
    (r : Renaming Γ Δ) →
    Γ ⊢ a ∶ A →
    Δ ⊢ rename (Renaming.f r) a ∶ rename (Renaming.f r) A
  ⊢-rename {Δ = Δ} {A = A} r (⊢-var ix) = 
    let tm' = ⊢-var (Renaming.run r ix) in
    subst (λ x → Δ ⊢ var x ∶ rename (Renaming.f r) A) (Renaming.correct r ix) tm'
  ⊢-rename r ⊢-Void = ⊢-Void
  ⊢-rename r ⊢-Unit = ⊢-Unit
  ⊢-rename r (⊢-Pi A B) =
    ⊢-Pi (⊢-type-rename r A) (⊢-type-rename (Renaming-under r) B)
  ⊢-rename r (⊢-Sigma A B) =
    ⊢-Sigma (⊢-type-rename r A) (⊢-type-rename (Renaming-under r) B)
  ⊢-rename r (⊢-lam A e) =
    ⊢-lam (⊢-type-rename r A) (⊢-rename (Renaming-under r) e)
  ⊢-rename r (⊢-app {A} {B} {B'} {a} {b} s Atype Btype) =
    ⊢-app s' (⊢-rename r Atype) (⊢-rename r Btype)
    where
      s' : (rename (under (Renaming.f r)) B [ rename (Renaming.f r) b ]) ≡ rename (Renaming.f r) B'
      s' = 
        begin
          (rename (under (Renaming.f r)) B [ rename (Renaming.f r) b ])
        ≡⟨ rename-a[] B ⟩
          rename (Renaming.f r) (B [ b ])
        ≡⟨ cong (rename (Renaming.f r)) s ⟩
          rename (Renaming.f r) B'
        ∎

⊢-weaken :
  {Γ : Ctx} {a A X : Term} →
  (Γ' : Ctx) →
  Γ ++ Γ' ⊢ a ∶ A →
  (Γ , X) ++ rename-ctx suc Γ' ⊢ rename (underₙ Γ' suc) a ∶ rename (underₙ Γ' suc) A
⊢-weaken Γ' = ⊢-rename (Renaming-weaken Renaming-id Γ')

⊢-weaken1 :
  {Γ : Ctx} {a A X : Term} →
  Γ ⊢ a ∶ A →
  Γ , X ⊢ rename suc a ∶ rename suc A
⊢-weaken1 = ⊢-weaken ◆
