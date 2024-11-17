{-# OPTIONS --safe --without-K #-}

module Presyntax.Properties.Rename where

open import Function using (_∘_)
open import Data.Nat using (ℕ; zero; suc; pred)
open import Data.Nat.Properties using (suc-injective)
open import Data.Product using (Σ-syntax; _×_) renaming (_,_ to _,,_)
open import Relation.Binary.PropositionalEquality as Eq using (_≡_; refl; cong; cong₂; trans; sym)
open Eq.≡-Reasoning
open import Relation.Nullary using (¬_)

open import Presyntax
open import Presyntax.Properties

under-id : {f : ℕ → ℕ} → ({ix : ℕ} → f ix ≡ ix) → {ix : ℕ} → under f ix ≡ ix
under-id f {zero} = refl
under-id f {suc ix} = cong suc f

under-∘ : {f g : ℕ → ℕ} → (ix : ℕ) → under f (under g ix) ≡ under (f ∘ g) ix
under-∘ zero = refl
under-∘ (suc ix) = refl

under-cong : {f g : ℕ → ℕ} → ((ix : ℕ) → f ix ≡ g ix) → (ix : ℕ) → under f ix ≡ under g ix
under-cong f zero = refl
under-cong f (suc ix) = cong suc (f ix)

under-injective :
  {f g : ℕ → ℕ} →
  ({ix ix' : ℕ} → f ix ≡ g ix' → ix ≡ ix') →
  {ix ix' : ℕ} → under f ix ≡ under g ix' → ix ≡ ix'
under-injective prf {zero} {zero} f-ix≡g-ix = refl
under-injective prf {suc ix} {suc ix'} f-ix≡g-ix = cong suc (prf (suc-injective f-ix≡g-ix))

underₙ-under-comm :
  {f : ℕ → ℕ} →
  (Γ : Ctx) → (ix : ℕ) →
  underₙ Γ (under f) ix ≡ under (underₙ Γ f) ix
underₙ-under-comm ◆ ix = refl
underₙ-under-comm {f} (ctx , x) ix = underₙ-under-comm {f = under f} ctx ix

underₙ-zero : {f : ℕ → ℕ} → (f zero ≡ zero) → (Γ : Ctx) → underₙ Γ f zero ≡ zero
underₙ-zero f ◆ = f
underₙ-zero f (ctx , x) = underₙ-zero refl ctx

underₙ-injective :
  {f g : ℕ → ℕ} →
  ({ix ix' : ℕ} → f ix ≡ g ix' → ix ≡ ix') →
  (Γ : Ctx) →
  {ix ix' : ℕ} →
  underₙ Γ f ix ≡ underₙ Γ g ix' →
  ix ≡ ix'
underₙ-injective prf ◆ underₙ-ix≡ix' = prf underₙ-ix≡ix'
underₙ-injective prf (ctx , x) underₙ-ix≡ix' = 
  underₙ-injective (under-injective prf) ctx underₙ-ix≡ix'

under-underₙ-ctx-length-ctx :
  {f : ℕ → ℕ} →
  (ctx : Ctx) →
  under (underₙ ctx f) (length ctx) ≡ length ctx
under-underₙ-ctx-length-ctx ◆ = refl
under-underₙ-ctx-length-ctx (ctx , x) = cong suc (trans (underₙ-under-comm ctx (length ctx)) (under-underₙ-ctx-length-ctx ctx))

rename-id : {f : ℕ → ℕ} → ({ix : ℕ} → f ix ≡ ix) → (x : Term) → rename f x ≡ x
rename-id f (var ix) = cong var f
rename-id f Type = refl
rename-id f Void = refl
rename-id f Unit = refl
rename-id f (Pi A B) = cong₂ Pi (rename-id f A) (rename-id (under-id f) B)
rename-id f (Sigma A B) = cong₂ Sigma (rename-id f A) (rename-id (under-id f) B)
rename-id f (lam A e) = cong₂ lam (rename-id f A) (rename-id (under-id f) e)
rename-id f (app a b) = cong₂ app (rename-id f a) (rename-id f b)

rename-cong : {f g : ℕ → ℕ} → ((ix : ℕ) → f ix ≡ g ix) → (x : Term) → rename f x ≡ rename g x
rename-cong f (var ix) = cong var (f ix)
rename-cong f Type = refl
rename-cong f Void = refl
rename-cong f Unit = refl
rename-cong f (Pi A B) = cong₂ Pi (rename-cong f A) (rename-cong (under-cong f) B)
rename-cong f (Sigma A B) = cong₂ Sigma (rename-cong f A) (rename-cong (under-cong f) B)
rename-cong f (lam A e) = cong₂ lam (rename-cong f A) (rename-cong (under-cong f) e)
rename-cong f (app a b) = cong₂ app (rename-cong f a) (rename-cong f b)

rename-∘ : {f g : ℕ → ℕ} → (x : Term) → rename f (rename g x) ≡ rename (f ∘ g) x
rename-∘ (var ix) = refl
rename-∘ Type = refl
rename-∘ Void = refl
rename-∘ Unit = refl
rename-∘ (Pi A B) = cong₂ Pi (rename-∘ A) (trans (rename-∘ B) (rename-cong under-∘ B))
rename-∘ (Sigma A B) = cong₂ Sigma (rename-∘ A) (trans (rename-∘ B) (rename-cong under-∘ B))
rename-∘ (lam A e) = cong₂ lam (rename-∘ A) (trans (rename-∘ e) (rename-cong under-∘ e))
rename-∘ (app a b) = cong₂ app (rename-∘ a) (rename-∘ b)

rename-f-≡-zero-⊥ :
  {f : ℕ → ℕ} →
  ({ix : ℕ} → ¬(f ix ≡ zero)) →
  (a : Term) → ¬(rename f a ≡ var zero)
rename-f-≡-zero-⊥ f (var ix) prf = f (var-injective prf)

rename-suc-≡-zero-⊥ : (a : Term) → ¬(rename suc a ≡ var zero)
rename-suc-≡-zero-⊥ (var _) ()
rename-suc-≡-zero-⊥ Type ()
rename-suc-≡-zero-⊥ Void ()
rename-suc-≡-zero-⊥ Unit ()
rename-suc-≡-zero-⊥ (Pi A B) ()
rename-suc-≡-zero-⊥ (Sigma A B) ()
rename-suc-≡-zero-⊥ (lam A e) ()
rename-suc-≡-zero-⊥ (app a b) ()

rename-var :
  {f g : ℕ → ℕ} →
  ({ix ix' : ℕ} → f ix ≡ g ix' → ix ≡ ix') →
  {ix : ℕ} →
  (a : Term) →
  rename f a ≡ var (g ix) →
  a ≡ var ix
rename-var prf (var x) rename-a≡var = cong var (prf (var-injective rename-a≡var))

rename-under-a-≡-zero :
  {f : ℕ → ℕ} →
  (a : Term) →
  rename (under f) a ≡ var zero →
  a ≡ var zero
rename-under-a-≡-zero (var zero) prf = prf

rename-Type :
  {f : ℕ → ℕ} →
  (a : Term) →
  rename f a ≡ Type →
  a ≡ Type
rename-Type Type refl = refl

rename-Void :
  {f g : ℕ → ℕ} →
  ({ix ix' : ℕ} → f ix ≡ g ix' → ix ≡ ix') →
  (a : Term) →
  rename f a ≡ Void →
  a ≡ Void
rename-Void prf Void refl = refl

rename-Unit :
  {f g : ℕ → ℕ} →
  ({ix ix' : ℕ} → f ix ≡ g ix' → ix ≡ ix') →
  (a : Term) →
  rename f a ≡ Unit →
  a ≡ Unit
rename-Unit prf Unit refl = refl

rename-Pi :
  {f g : ℕ → ℕ} →
  ({ix ix' : ℕ} → f ix ≡ g ix' → ix ≡ ix') →
  {A B : Term} →
  (a : Term) →
  rename f a ≡ Pi A B →
  Σ[ A' ∈ Term ] (Σ[ B' ∈ Term ] ((a ≡ Pi A' B') × (rename f A' ≡ A) × (rename (under f) B' ≡ B)))
rename-Pi prf (Pi A' B') rename-a≡Pi =
  A' ,, B' ,, refl ,, Pi-injective₁ rename-a≡Pi ,, Pi-injective₂ rename-a≡Pi

rename-Sigma :
  {f g : ℕ → ℕ} →
  ({ix ix' : ℕ} → f ix ≡ g ix' → ix ≡ ix') →
  {A B : Term} →
  (a : Term) →
  rename f a ≡ Sigma A B →
  Σ[ A' ∈ Term ] (Σ[ B' ∈ Term ] ((a ≡ Sigma A' B') × (rename f A' ≡ A) × (rename (under f) B' ≡ B)))
rename-Sigma prf (Sigma A' B') rename-a≡Sigma =
  A' ,, B' ,, refl ,, Sigma-injective₁ rename-a≡Sigma ,, Sigma-injective₂ rename-a≡Sigma

rename-lam :
  {f g : ℕ → ℕ} →
  ({ix ix' : ℕ} → f ix ≡ g ix' → ix ≡ ix') →
  {A e : Term} →
  (a : Term) →
  rename f a ≡ lam A e →
  Σ[ A' ∈ Term ] (Σ[ e' ∈ Term ] ((a ≡ lam A' e') × (rename f A' ≡ A) × (rename (under f) e' ≡ e)))
rename-lam prf (lam A' e') rename-a≡lam =
  A' ,, e' ,, refl ,, lam-injective₁ rename-a≡lam ,, lam-injective₂ rename-a≡lam

rename-app :
  {f g : ℕ → ℕ} →
  ({ix ix' : ℕ} → f ix ≡ g ix' → ix ≡ ix') →
  {a b : Term} →
  (x : Term) →
  rename f x ≡ app a b →
  Σ[ a' ∈ Term ] (Σ[ b' ∈ Term ] ((x ≡ app a' b') × (rename f a' ≡ a) × (rename f b' ≡ b)))
rename-app prf (app a' b') rename-x≡app =
  a' ,, b' ,, refl ,, app-injective₁ rename-x≡app ,, app-injective₂ rename-x≡app

rename-injective :
  {f g : ℕ → ℕ} →
  ({ix ix' : ℕ} → f ix ≡ g ix' → ix ≡ ix') →
  (a b : Term) → 
  rename f a ≡ rename g b →
  a ≡ b
rename-injective prf a (var ix) rename-a≡rename-b = rename-var prf a rename-a≡rename-b
rename-injective prf a Type rename-a≡rename-b = rename-Type a rename-a≡rename-b
rename-injective prf a Void rename-a≡rename-b = rename-Void prf a rename-a≡rename-b
rename-injective prf a Unit rename-a≡rename-b = rename-Unit prf a rename-a≡rename-b
rename-injective prf a (Pi A B) rename-a≡rename-b with rename-Pi prf a rename-a≡rename-b
... | A' ,, B' ,, refl ,, rename-A≡A' ,, rename-B≡B' =
  cong₂
    Pi
    (rename-injective prf A' A rename-A≡A')
    (rename-injective (under-injective prf) B' B rename-B≡B')
rename-injective prf a (Sigma A B) rename-a≡rename-b with rename-Sigma prf a rename-a≡rename-b
... | A' ,, B' ,, refl ,, rename-A≡A' ,, rename-B≡B' =
  cong₂
    Sigma
    (rename-injective prf A' A rename-A≡A')
    (rename-injective (under-injective prf) B' B rename-B≡B')
rename-injective prf a (lam A e) rename-a≡rename-b with rename-lam prf a rename-a≡rename-b
... | A' ,, e' ,, refl ,, rename-A≡A' ,, rename-e≡e' =
  cong₂
    lam
    (rename-injective prf A' A rename-A≡A')
    (rename-injective (under-injective prf) e' e rename-e≡e')
rename-injective prf a (app f x) rename-a≡rename-b with rename-app prf a rename-a≡rename-b
... | f' ,, x' ,, refl ,, rename-f≡f' ,, rename-x≡x' =
  cong₂
    app
    (rename-injective prf f' f rename-f≡f')
    (rename-injective prf x' x rename-x≡x')

sunderₙ-sunder-comm :
  {f : ℕ → Term} {ix : ℕ} →
  (ctx : Ctx) →
  sunderₙ ctx (sunder f) ix ≡ sunder (sunderₙ ctx f) ix
sunderₙ-sunder-comm {f} ◆ = refl
sunderₙ-sunder-comm {f} (ctx , x) = 
  sunderₙ-sunder-comm {f = sunder f} ctx

rename-suc-var :
  {ix : ℕ} →
  (a : Term) →
  rename suc a ≡ var (suc ix) →
  a ≡ var ix
rename-suc-var (var x) prf = cong var (suc-injective (var-injective prf))

under-f-≡-sucᵣ : {f : ℕ → ℕ} → (ix ix' : ℕ) → under f ix ≡ suc ix' → (f ∘ pred) ix ≡ ix'
under-f-≡-sucᵣ {f} (suc ix) ix' prf = suc-injective prf

under-f-≡-sucₗ : {f : ℕ → ℕ} → (ix ix' : ℕ) → under f ix ≡ suc ix' → (suc ∘ pred) ix ≡ ix
under-f-≡-sucₗ (suc ix) zero prf = refl
under-f-≡-sucₗ (suc ix) (suc ix') prf = refl

rename-under-f-≡-var-suc :
  {f : ℕ → ℕ} →
  (a : Term) →
  (ix : ℕ) →
  rename (under f) a ≡ var (suc ix) →
  rename (f ∘ pred) a ≡ var ix
rename-under-f-≡-var-suc {f} (var v) ix prf =
  cong var (under-f-≡-sucᵣ v ix (var-injective prf))

pred-under-zero : {f : ℕ → ℕ} → (pred ∘ under f) zero ≡ zero
pred-under-zero = refl

pred-under-suc : {f : ℕ → ℕ} → (ix : ℕ) → (pred ∘ under f) (suc ix) ≡ f ix
pred-under-suc ix = refl

pred-under-fzero :
  {f : ℕ → ℕ} →
  f zero ≡ zero →
  (ix : ℕ) → pred (under f ix) ≡ f (pred ix)
pred-under-fzero prf zero = sym prf
pred-under-fzero prf (suc ix) = refl

g-suc-f-≡-g-f-suc :
  {f : ℕ → ℕ} {g : (ℕ → ℕ) → ℕ → ℕ} →
  ((ix ix' : ℕ) → g (suc ∘ f) ix ≡ g (f ∘ suc) ix' → ix ≡ ix') →
  (ix ix' : ℕ) → (under ∘ g) (suc ∘ f) ix ≡ (under ∘ g) (f ∘ suc) ix' → ix ≡ ix'
g-suc-f-≡-g-f-suc g-suc-f zero zero prf = refl
g-suc-f-≡-g-f-suc {f} {g} g-suc-f (suc ix) (suc ix') prf = 
  let
    prf' : g (suc ∘ f) ix ≡ g (f ∘ suc) ix'
    prf' = suc-injective prf
  in
    cong suc (g-suc-f ix ix' prf')

rename-g-suc-f-≡-rename-g-f-suc :
  {f : ℕ → ℕ} {g : (ℕ → ℕ) → ℕ → ℕ}→
  (g-suc-f : (ix ix' : ℕ) → g (suc ∘ f) ix ≡ g (f ∘ suc) ix' → ix ≡ ix') →
  (a b : Term) → rename (g (suc ∘ f)) a ≡ rename (g (f ∘ suc)) b → a ≡ b
rename-g-suc-f-≡-rename-g-f-suc {f} {g} g-suc-f (var ix) (var ix') prf =
  let
    prf' : g (suc ∘ f) ix ≡ g (f ∘ suc) ix'
    prf' = var-injective prf
  in
    cong var (g-suc-f ix ix' prf')
rename-g-suc-f-≡-rename-g-f-suc g-suc-f Type Type prf = prf
rename-g-suc-f-≡-rename-g-f-suc g-suc-f Void Void prf = prf
rename-g-suc-f-≡-rename-g-f-suc g-suc-f Unit Unit prf = prf
rename-g-suc-f-≡-rename-g-f-suc {f} {g} g-suc-f (Pi A B) (Pi A' B') prf =
  cong₂
    Pi
    (rename-g-suc-f-≡-rename-g-f-suc {f} {g} g-suc-f A A' (Pi-injective₁ prf))
    (rename-g-suc-f-≡-rename-g-f-suc {f} {g = under ∘ g} (g-suc-f-≡-g-f-suc {f} {g} g-suc-f) B B' (Pi-injective₂ prf))
rename-g-suc-f-≡-rename-g-f-suc {f} {g} g-suc-f (Sigma A B) (Sigma A' B') prf =
  cong₂
    Sigma
    (rename-g-suc-f-≡-rename-g-f-suc {f} {g} g-suc-f A A' (Sigma-injective₁ prf))
    (rename-g-suc-f-≡-rename-g-f-suc {f} {g = under ∘ g} (g-suc-f-≡-g-f-suc {f} {g} g-suc-f) B B' (Sigma-injective₂ prf))
rename-g-suc-f-≡-rename-g-f-suc {f} {g} g-suc-f (lam A e) (lam A' e') prf =
  cong₂
    lam
    (rename-g-suc-f-≡-rename-g-f-suc {f} {g} g-suc-f A A' (lam-injective₁ prf))
    (rename-g-suc-f-≡-rename-g-f-suc {f} {g = under ∘ g} (g-suc-f-≡-g-f-suc {f} {g} g-suc-f) e e' (lam-injective₂ prf))
rename-g-suc-f-≡-rename-g-f-suc {f} {g} g-suc-f (app a b) (app a' b') prf =
  cong₂
    app
    (rename-g-suc-f-≡-rename-g-f-suc {f} {g} g-suc-f a a' (app-injective₁ prf))
    (rename-g-suc-f-≡-rename-g-f-suc {f} {g} g-suc-f b b' (app-injective₂ prf))

rename-suc-f-≡-rename-f-suc :
  {f : ℕ → ℕ} →
  (suc-f : (ix ix' : ℕ) → (suc ∘ f) ix ≡ (f ∘ suc) ix' → ix ≡ ix') →
  (a b : Term) → rename (suc ∘ f) a ≡ rename (f ∘ suc) b → a ≡ b
rename-suc-f-≡-rename-f-suc {f} = rename-g-suc-f-≡-rename-g-f-suc {f} {g = λ x → x}

under-g-under-f-≡-h-suc :
  {f : ℕ → ℕ} {g h : (ℕ → ℕ) → ℕ → ℕ} →
  ((ix ix' : ℕ) → g (under f) ix ≡ h suc ix' → g (suc ∘ pred) ix ≡ ix) →
  (ix ix' : ℕ) → under (g (under f)) ix ≡ under (h suc) ix' → under (g (suc ∘ pred)) ix ≡ ix
under-g-under-f-≡-h-suc fg zero zero prf = prf
under-g-under-f-≡-h-suc fg (suc ix) (suc ix') prf = cong suc (fg ix ix' (suc-injective prf))

rename-f-under-f-≡-h-suc :
  {f : ℕ → ℕ} {g h : (ℕ → ℕ) → ℕ → ℕ} →
  ((ix ix' : ℕ) → g (under f) ix ≡ h suc ix' → g (suc ∘ pred) ix ≡ ix) →
  (A B : Term) →
  rename (g (under f)) A ≡ rename (h suc) B →
  rename (g (suc ∘ pred)) A ≡ A
rename-f-under-f-≡-h-suc fg (var ix) (var ix') prf = cong var (fg ix ix' (var-injective prf))
rename-f-under-f-≡-h-suc fg Type Type prf = prf
rename-f-under-f-≡-h-suc fg Void Void prf = prf
rename-f-under-f-≡-h-suc fg Unit Unit prf = prf
rename-f-under-f-≡-h-suc {f} {g} {h} fg (Pi A B) (Pi A' B') prf =
  cong₂
    Pi
    (rename-f-under-f-≡-h-suc {f} {g} {h} fg A A' (Pi-injective₁ prf))
    (rename-f-under-f-≡-h-suc {f} {g = under ∘ g} {h = under ∘ h} (under-g-under-f-≡-h-suc {f} {g} {h} fg) B B' (Pi-injective₂ prf))
rename-f-under-f-≡-h-suc {f} {g} {h} fg (Sigma A B) (Sigma A' B') prf =
  cong₂
    Sigma
    (rename-f-under-f-≡-h-suc {f} {g} {h} fg A A' (Sigma-injective₁ prf))
    (rename-f-under-f-≡-h-suc {f} {g = under ∘ g} {h = under ∘ h} (under-g-under-f-≡-h-suc {f} {g} {h} fg) B B' (Sigma-injective₂ prf))
rename-f-under-f-≡-h-suc {f} {g} {h} fg (lam A e) (lam A' e') prf =
  cong₂
    lam
    (rename-f-under-f-≡-h-suc {f} {g} {h} fg A A' (lam-injective₁ prf))
    (rename-f-under-f-≡-h-suc {f} {g = under ∘ g} {h = under ∘ h} (under-g-under-f-≡-h-suc {f} {g} {h} fg) e e' (lam-injective₂ prf))
rename-f-under-f-≡-h-suc {f} {g} {h} fg (app a b) (app a' b') prf =
  cong₂
    app
    (rename-f-under-f-≡-h-suc {f} {g} {h} fg a a' (app-injective₁ prf))
    (rename-f-under-f-≡-h-suc {f} {g} {h} fg b b' (app-injective₂ prf))

rename-under-f-≡-rename-sucₗ :
  {f : ℕ → ℕ} →
  (A B : Term) →
  rename (under f) A ≡ rename suc B →
  rename (suc ∘ pred) A ≡ A
rename-under-f-≡-rename-sucₗ {f} = rename-f-under-f-≡-h-suc {f} {g = λ x → x} {h = λ x → x} under-f-≡-sucₗ

rename-under-f-≡-rename-sucᵣ :
  {f : ℕ → ℕ} →
  (A B : Term) →
  rename (under f) A ≡ rename suc B →
  rename (f ∘ pred) A ≡ B
rename-under-f-≡-rename-sucᵣ {f} A B prf =
  let
    prf'' : rename (pred ∘ under f) A ≡ B
    prf'' = 
      begin
        rename (pred ∘ under f) A
      ≡⟨ sym (rename-∘ A) ⟩
        rename pred (rename (under f) A)
      ≡⟨ cong (rename pred) prf ⟩
        rename pred (rename suc B)
      ≡⟨ rename-∘ B ⟩
        rename (pred ∘ suc) B
      ≡⟨ rename-cong (λ _ → refl) B ⟩
        rename (λ x → x) B
      ≡⟨ rename-id refl B ⟩
        B
      ∎

    prf''' : rename (pred ∘ under f) (rename (suc ∘ pred) A) ≡ B
    prf''' = trans (cong (rename (pred ∘ under f)) (rename-under-f-≡-rename-sucₗ A B prf)) prf''
  in
    begin
      rename (f ∘ pred) A
    ≡⟨ rename-cong (λ _ → refl) A ⟩
      rename (pred ∘ suc ∘ f ∘ pred) A
    ≡⟨ rename-cong (λ _ → refl) A ⟩
      rename (pred ∘ under f ∘ suc ∘ pred) A
    ≡⟨ sym (rename-∘ A) ⟩
      rename (pred ∘ under f) (rename (suc ∘ pred) A)
    ≡⟨ prf''' ⟩
      B
    ∎
