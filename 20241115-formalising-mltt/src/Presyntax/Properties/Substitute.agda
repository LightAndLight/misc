{-# OPTIONS --safe --without-K #-}

module Presyntax.Properties.Substitute where

open import Function using (_∘_; case_of_)
open import Data.Empty using (⊥-elim)
open import Data.Nat using (ℕ; zero; suc; pred; _∸_; _<_; _>_; z≤n; s≤s)
open import Data.Nat.Properties using (suc-injective)
open import Data.Product using (Σ-syntax; _×_) renaming (_,_ to _,,_)
open import Data.Sum using (_⊎_; inj₁; inj₂)
open import Relation.Binary.PropositionalEquality as Eq using (_≡_; refl; cong; cong₂; sym; trans)
open Eq.≡-Reasoning

open import Presyntax
open import Presyntax.Properties
open import Presyntax.Properties.Rename

sunder-var : {f : ℕ → Term} → ((ix : ℕ) → f ix ≡ var ix) → (ix : ℕ) → sunder f ix ≡ var ix
sunder-var g zero = refl
sunder-var {f} g (suc ix) = 
  begin
    rename suc (f ix)
  ≡⟨ cong (rename suc) (g ix) ⟩
    rename suc (var ix)
  ≡⟨⟩
    var (suc ix)
  ∎

sunder-cong :
  {f g : ℕ → Term} →
  ((ix : ℕ) → f ix ≡ g ix) →
  (ix : ℕ) →
  sunder f ix ≡ sunder g ix
sunder-cong f zero = refl
sunder-cong f (suc ix) = cong (rename suc) (f ix)

substitute-var : {f : ℕ → Term} → ((ix : ℕ) → f ix ≡ var ix) → (x : Term) → substitute f x ≡ x
substitute-var g (var ix) = g ix
substitute-var g Type = refl
substitute-var g Void = refl
substitute-var g Unit = refl
substitute-var g (Pi A B) = cong₂ Pi (substitute-var g A) (substitute-var (sunder-var g) B)
substitute-var g (Sigma A B) = cong₂ Sigma (substitute-var g A) (substitute-var (sunder-var g) B)
substitute-var g (lam A e) = cong₂ lam (substitute-var g A) (substitute-var (sunder-var g) e)
substitute-var g (app a b) = cong₂ app (substitute-var g a) (substitute-var g b)

substitute-cong :
  {f g : ℕ → Term} →
  ((ix : ℕ) → f ix ≡ g ix) →
  (x : Term) →
  substitute f x ≡ substitute g x
substitute-cong f (var ix) = f ix
substitute-cong f Type = refl
substitute-cong f Void = refl
substitute-cong f Unit = refl
substitute-cong f (Pi A B) = cong₂ Pi (substitute-cong f A) (substitute-cong (sunder-cong f) B)
substitute-cong f (Sigma A B) = cong₂ Sigma (substitute-cong f A) (substitute-cong (sunder-cong f) B)
substitute-cong f (lam A e) = cong₂ lam (substitute-cong f A) (substitute-cong (sunder-cong f) e)
substitute-cong f (app a b) = cong₂ app (substitute-cong f a) (substitute-cong f b)

sunder∘under :
  {f : ℕ → ℕ} {g : ℕ → Term} {h : ℕ → Term} →
  ((ix : ℕ) → h ix ≡ (g ∘ f) ix) →
  (ix : ℕ) → sunder h ix ≡ (sunder g ∘ under f) ix
sunder∘under compose zero = refl
sunder∘under {f} {g} {h} compose (suc ix) = 
  begin
    rename suc (h ix)
  ≡⟨ cong (rename suc) (compose ix) ⟩
    rename suc (g (f ix))
  ≡⟨⟩
    sunder g (suc (f ix))
  ≡⟨⟩
    (sunder g ∘ under f) (suc ix)
  ∎

substitute∘rename :
  {f : ℕ → ℕ} {g : ℕ → Term} {h : ℕ → Term} →
  (compose : (ix : ℕ) → h ix ≡ (g ∘ f) ix) →
  (x : Term) →
  substitute h x ≡ substitute g (rename f x)
substitute∘rename compose (var ix) = compose ix
substitute∘rename compose Type = refl
substitute∘rename compose Void = refl
substitute∘rename compose Unit = refl
substitute∘rename compose (Pi A B) =
  cong₂ Pi (substitute∘rename compose A) (substitute∘rename (sunder∘under compose) B)
substitute∘rename compose (Sigma A B) =
  cong₂ Sigma (substitute∘rename compose A) (substitute∘rename (sunder∘under compose) B)
substitute∘rename compose (lam A e) =
  cong₂ lam (substitute∘rename compose A) (substitute∘rename (sunder∘under compose) e)
substitute∘rename compose (app a b) =
  cong₂ app (substitute∘rename compose a) (substitute∘rename compose b)

rename-under-sunder :
  {f : ℕ → ℕ} {g : ℕ → Term} →
  (ix : ℕ) →
  rename (under f) (sunder g ix) ≡ sunder (rename f ∘ g) ix
rename-under-sunder zero = refl
rename-under-sunder {f} {g} (suc ix) = 
  begin
    rename (under f) (rename suc (g ix))
  ≡⟨ rename-∘ (g ix) ⟩
    rename (under f ∘ suc) (g ix)
  ≡⟨⟩
    rename (suc ∘ f) (g ix)
  ≡⟨ sym (rename-∘ (g ix)) ⟩
    rename suc (rename f (g ix))
  ∎

rename∘substitute :
  {f : ℕ → ℕ} {g : ℕ → Term} →
  (x : Term) →
  rename f (substitute g x) ≡ substitute (rename f ∘ g) x
rename∘substitute (var ix) = refl
rename∘substitute Type = refl
rename∘substitute Void = refl
rename∘substitute Unit = refl
rename∘substitute {f} {g} (Pi A B) =
  cong₂
    Pi
    (rename∘substitute A)
    (
      begin
        rename (under f) (substitute (sunder g) B)
      ≡⟨ rename∘substitute B ⟩
        substitute (rename (under f) ∘ sunder g) B
      ≡⟨ substitute-cong rename-under-sunder B ⟩
        substitute (sunder (rename f ∘ g)) B
      ∎
    )
rename∘substitute {f} {g} (Sigma A B) =
  cong₂
    Sigma
    (rename∘substitute A)
    (
      begin
        rename (under f) (substitute (sunder g) B)
      ≡⟨ rename∘substitute B ⟩
        substitute (rename (under f) ∘ sunder g) B
      ≡⟨ substitute-cong rename-under-sunder B ⟩
        substitute (sunder (rename f ∘ g)) B
      ∎
    )
rename∘substitute {f} {g} (lam A e) =
  cong₂
    lam
    (rename∘substitute A)
    (
      begin
        rename (under f) (substitute (sunder g) e)
      ≡⟨ rename∘substitute e ⟩
        substitute (rename (under f) ∘ sunder g) e
      ≡⟨ substitute-cong rename-under-sunder e ⟩
        substitute (sunder (rename f ∘ g)) e
      ∎
    )
rename∘substitute (app a b) = cong₂ app (rename∘substitute a) (rename∘substitute b)

substitute-sunder-sunder :
  {f g : ℕ → Term} →
  (ix : ℕ) →
  substitute (sunder f) (sunder g ix) ≡ sunder (substitute f ∘ g) ix
substitute-sunder-sunder zero = refl
substitute-sunder-sunder {f} {g} (suc ix) = 
  begin
    substitute (sunder f) (rename suc (g ix))
  ≡⟨ sym (substitute∘rename (λ _ → refl) (g ix)) ⟩
    substitute (sunder f ∘ suc) (g ix)
  ≡⟨⟩
    substitute (rename suc ∘ f) (g ix)
  ≡⟨ sym (rename∘substitute (g ix)) ⟩
    rename suc (substitute f (g ix))
  ∎

substitute-∘ :
  {f g : ℕ → Term} →
  (x : Term) →
  substitute f (substitute g x) ≡ substitute (substitute f ∘ g) x
substitute-∘ (var ix) = refl
substitute-∘ Type = refl
substitute-∘ Void = refl
substitute-∘ Unit = refl
substitute-∘ {f} {g} (Pi A B) =
  cong₂
    Pi
    (substitute-∘ A)
    (
      begin
        substitute (sunder f) (substitute (sunder g) B)
      ≡⟨ substitute-∘ B ⟩
        substitute (substitute (sunder f) ∘ sunder g) B
      ≡⟨ substitute-cong substitute-sunder-sunder B ⟩
      substitute (sunder (substitute f ∘ g)) B
      ∎
    )
substitute-∘ {f} {g} (Sigma A B) =
  cong₂
    Sigma
    (substitute-∘ A)
    (
      begin
        substitute (sunder f) (substitute (sunder g) B)
      ≡⟨ substitute-∘ B ⟩
        substitute (substitute (sunder f) ∘ sunder g) B
      ≡⟨ substitute-cong substitute-sunder-sunder B ⟩
      substitute (sunder (substitute f ∘ g)) B
      ∎
    )
substitute-∘ {f} {g} (lam A e) =
  cong₂
    lam
    (substitute-∘ A)
    (
      begin
        substitute (sunder f) (substitute (sunder g) e)
      ≡⟨ substitute-∘ e ⟩
        substitute (substitute (sunder f) ∘ sunder g) e
      ≡⟨ substitute-cong substitute-sunder-sunder e ⟩
      substitute (sunder (substitute f ∘ g)) e
      ∎
    )
substitute-∘ (app a b) = cong₂ app (substitute-∘ a) (substitute-∘ b)

inst1-Type :
  {x : Term} {ix : ℕ} →
  inst1 x ix ≡ Type →
  (ix ≡ zero) × (x ≡ Type)
inst1-Type {x} {zero} prf = refl ,, prf

inst1-Pi :
  {x A B : Term} {ix : ℕ} →
  inst1 x ix ≡ Pi A B →
  (ix ≡ zero) × (x ≡ Pi A B)
inst1-Pi {x} {ix = zero} prf = refl ,, prf

inst1-app :
  {x a b : Term} {ix : ℕ} →
  inst1 x ix ≡ app a b →
  (ix ≡ zero) × (x ≡ app a b)
inst1-app {x} {ix = zero} prf = refl ,, prf

inst1-under-var :
  {f : ℕ → ℕ} {x : Term} {ix ix' : ℕ} →
  inst1 x (under f ix) ≡ var ix' →
  ((ix ≡ zero) × (x ≡ var ix')) ⊎ Σ[ ix'' ∈ ℕ ] (ix ≡ suc ix'' × (f ix'' ≡ ix'))
inst1-under-var {ix = zero} prf = inj₁ (refl ,, prf)
inst1-under-var {ix = suc ix} prf = inj₂ (ix ,, refl ,, var-injective prf)

inst1-≡-var :
  {f : ℕ → ℕ} {x : Term} →
  {ix ix' : ℕ} →
  inst1 x ix ≡ var (f ix') →
  (x ≡ var (f ix')) ⊎ (pred ix ≡ f ix')
inst1-≡-var {ix = zero} prf = inj₁ prf
inst1-≡-var {ix = suc ix} prf = inj₂ (var-injective prf)

rename∘inst1 :
  {f : ℕ → ℕ} {x : Term} →
  (ix : ℕ) →
  rename f (inst1 x ix) ≡ inst1 (rename f x) (under f ix)
rename∘inst1 zero = refl
rename∘inst1 (suc ix) = refl

rename-sunder-inst :
  {f : ℕ → ℕ} {x : Term} →
  (ix : ℕ) →
  rename (under f) (sunder (inst1 x) ix) ≡ sunder (inst1 (rename f x) ∘ under f) ix
rename-sunder-inst zero = refl
rename-sunder-inst {f} {x} (suc ix) = 
  begin
    rename (under f) (rename suc (inst1 x ix))
  ≡⟨ rename-∘ (inst1 x ix) ⟩
    rename (under f ∘ suc) (inst1 x ix)
  ≡⟨⟩
    rename (suc ∘ f) (inst1 x ix)
  ≡⟨ sym (rename-∘ (inst1 x ix)) ⟩
    rename suc (rename f (inst1 x ix))
  ≡⟨ cong (rename suc) (rename∘inst1 ix) ⟩
    rename suc ((inst1 (rename f x) ∘ under f) ix)
  ∎

rename-a[] :
  {f : ℕ → ℕ} {x : Term} →
  (a : Term) →
  ((rename (under f) a) [ rename f x ]) ≡ rename f (a [ x ])
rename-a[] (var zero) = refl
rename-a[] (var (suc ix)) = cong var refl
rename-a[] Type = refl
rename-a[] Void = refl
rename-a[] Unit = refl
rename-a[] {f} {x} (Pi A B) =
  cong₂
    Pi
    (rename-a[] A)
    (
    begin
      substitute (sunder (inst1 (rename f x))) (rename (under (under f)) B)
    ≡⟨ sym (substitute∘rename (λ _ → refl) B) ⟩
      substitute (sunder (inst1 (rename f x)) ∘ under (under f)) B
    ≡⟨ substitute-cong (λ ix → sym (sunder∘under (λ _ → refl) ix)) B ⟩
      substitute (sunder (inst1 (rename f x) ∘ under f)) B
    ≡⟨ substitute-cong (λ ix → sym (rename-sunder-inst ix)) B ⟩
      substitute (rename (under f) ∘ sunder (inst1 x)) B
    ≡⟨ sym (rename∘substitute B) ⟩
      rename (under f) (substitute (sunder (inst1 x)) B)
    ∎
    )
rename-a[] {f} {x} (Sigma A B) =
  cong₂
    Sigma
    (rename-a[] A)
    (
    begin
      substitute (sunder (inst1 (rename f x))) (rename (under (under f)) B)
    ≡⟨ sym (substitute∘rename (λ _ → refl) B) ⟩
      substitute (sunder (inst1 (rename f x)) ∘ under (under f)) B
    ≡⟨ substitute-cong (λ ix → sym (sunder∘under (λ _ → refl) ix)) B ⟩
      substitute (sunder (inst1 (rename f x) ∘ under f)) B
    ≡⟨ substitute-cong (λ ix → sym (rename-sunder-inst ix)) B ⟩
      substitute (rename (under f) ∘ sunder (inst1 x)) B
    ≡⟨ sym (rename∘substitute B) ⟩
      rename (under f) (substitute (sunder (inst1 x)) B)
    ∎
    )
rename-a[] {f} {x} (lam A e) =
  cong₂
    lam
    (rename-a[] A)
    (
    begin
      substitute (sunder (inst1 (rename f x))) (rename (under (under f)) e)
    ≡⟨ sym (substitute∘rename (λ _ → refl) e) ⟩
      substitute (sunder (inst1 (rename f x)) ∘ under (under f)) e
    ≡⟨ substitute-cong (λ ix → sym (sunder∘under (λ _ → refl) ix)) e ⟩
      substitute (sunder (inst1 (rename f x) ∘ under f)) e
    ≡⟨ substitute-cong (λ ix → sym (rename-sunder-inst ix)) e ⟩
      substitute (rename (under f) ∘ sunder (inst1 x)) e
    ≡⟨ sym (rename∘substitute e) ⟩
      rename (under f) (substitute (sunder (inst1 x)) e)
    ∎
    )
rename-a[] (app a b) = cong₂ app (rename-a[] a) (rename-a[] b)

inst1-rename-suc-under-suc-≡-var-suc :
  {ix ix' : ℕ} {x : Term} →
  inst1 (rename suc x) (under suc ix) ≡ var (suc ix') →
  ((ix ≡ zero) × (x ≡ var ix')) ⊎ (pred ix ≡ ix')
inst1-rename-suc-under-suc-≡-var-suc {zero} {x = var x} prf = inj₁ (refl ,, cong var (suc-injective (var-injective prf)))
inst1-rename-suc-under-suc-≡-var-suc {suc ix} prf = inj₂ (suc-injective (var-injective prf))

substitute-inst :
  {f : ℕ → Term} {x : Term} →
  (ix : ℕ) →
  substitute (inst1 (substitute f x)) (sunder f ix) ≡ substitute f (inst1 x ix)
substitute-inst zero = refl
substitute-inst {f} {x} (suc ix) = 
  begin
    substitute (inst1 (substitute f x)) (rename suc (f ix))
  ≡⟨ sym (substitute∘rename (λ _ → refl) (f ix)) ⟩
    substitute (inst1 (substitute f x) ∘ suc) (f ix)
  ≡⟨⟩
    substitute var (f ix)
  ≡⟨ substitute-var (λ _ → refl) (f ix) ⟩
    f ix
  ∎

substitute-a[] :
  {f : ℕ → Term} {x : Term} →
  (a : Term) →
  (substitute (sunder f) a [ substitute f x ]) ≡ substitute f (a [ x ])
substitute-a[] (var zero) = refl
substitute-a[] {f} {x} (var (suc ix)) = 
  begin
    (rename suc (f ix)) [ substitute f x ]
  ≡⟨⟩
    substitute (inst1 (substitute f x)) (rename suc (f ix))
  ≡⟨ sym (substitute∘rename (λ _ → refl) (f ix)) ⟩
    substitute (inst1 (substitute f x) ∘ suc) (f ix)
  ≡⟨⟩
    substitute var (f ix)
  ≡⟨ substitute-var (λ _ → refl) _ ⟩
    f ix
  ∎
substitute-a[] Type = refl
substitute-a[] Void = refl
substitute-a[] Unit = refl
substitute-a[] {f} {x} (Pi A B) =
  cong₂
    Pi
    (substitute-a[] A)
    (
    begin
      substitute (sunder (inst1 (substitute f x))) (substitute (sunder (sunder f)) B)
    ≡⟨ substitute-∘ B ⟩
      substitute (substitute (sunder (inst1 (substitute f x))) ∘ sunder (sunder f)) B
    ≡⟨ substitute-cong substitute-sunder-sunder B ⟩
      substitute (sunder (substitute (inst1 (substitute f x)) ∘ sunder f)) B
    ≡⟨ substitute-cong (sunder-cong substitute-inst) B ⟩
      substitute (sunder (substitute f ∘ inst1 x)) B
    ≡⟨ substitute-cong (λ ix → sym (substitute-sunder-sunder ix)) B ⟩
      substitute (substitute (sunder f) ∘ sunder (inst1 x)) B
    ≡⟨ sym (substitute-∘ B) ⟩
      substitute (sunder f) (substitute (sunder (inst1 x)) B)
    ∎
    )
substitute-a[] {f} {x} (Sigma A B) =
  cong₂
    Sigma
    (substitute-a[] A)
    (
    begin
      substitute (sunder (inst1 (substitute f x))) (substitute (sunder (sunder f)) B)
    ≡⟨ substitute-∘ B ⟩
      substitute (substitute (sunder (inst1 (substitute f x))) ∘ sunder (sunder f)) B
    ≡⟨ substitute-cong substitute-sunder-sunder B ⟩
      substitute (sunder (substitute (inst1 (substitute f x)) ∘ sunder f)) B
    ≡⟨ substitute-cong (sunder-cong substitute-inst) B ⟩
      substitute (sunder (substitute f ∘ inst1 x)) B
    ≡⟨ substitute-cong (λ ix → sym (substitute-sunder-sunder ix)) B ⟩
      substitute (substitute (sunder f) ∘ sunder (inst1 x)) B
    ≡⟨ sym (substitute-∘ B) ⟩
      substitute (sunder f) (substitute (sunder (inst1 x)) B)
    ∎
    )
substitute-a[] {f} {x} (lam A e) =
  cong₂
    lam
    (substitute-a[] A)
    (
    begin
      substitute (sunder (inst1 (substitute f x))) (substitute (sunder (sunder f)) e)
    ≡⟨ substitute-∘ e ⟩
      substitute (substitute (sunder (inst1 (substitute f x))) ∘ sunder (sunder f)) e
    ≡⟨ substitute-cong substitute-sunder-sunder e ⟩
      substitute (sunder (substitute (inst1 (substitute f x)) ∘ sunder f)) e
    ≡⟨ substitute-cong (sunder-cong substitute-inst) e ⟩
      substitute (sunder (substitute f ∘ inst1 x)) e
    ≡⟨ substitute-cong (λ ix → sym (substitute-sunder-sunder ix)) e ⟩
      substitute (substitute (sunder f) ∘ sunder (inst1 x)) e
    ≡⟨ sym (substitute-∘ e) ⟩
      substitute (sunder f) (substitute (sunder (inst1 x)) e)
    ∎
    )
substitute-a[] (app a b) = 
  cong₂ app (substitute-a[] a) (substitute-a[] b)

sunder-Type : {x : Term} {ix : ℕ} → sunder (inst1 x) ix ≡ Type → (x ≡ Type) × (ix ≡ suc zero)
sunder-Type {x} {ix = suc ix} prf =
  let
    prf' : inst1 x ix ≡ Type
    prf' = rename-Type (inst1 x ix) prf

    ix≡zero ,, x≡Type = inst1-Type {x} prf'
  in
  x≡Type ,, cong suc ix≡zero

sunder-sunder-Type :
  {x : Term} {ix : ℕ} →
  sunder (sunder (inst1 x)) ix ≡ Type →
  (x ≡ Type) × (ix ≡ suc (suc zero))
sunder-sunder-Type {x} {ix = suc (suc ix)} prf =
  let
    prf' : rename suc (rename suc (inst1 x ix)) ≡ rename suc (rename suc Type)
    prf' = prf

    prf'' : inst1 x ix ≡ Type
    prf'' = rename-injective suc-injective _ _ (rename-injective suc-injective _ _ prf')

    ix≡zero ,, x≡Type = inst1-Type {x} prf''
  in
    x≡Type ,, cong suc (cong suc ix≡zero)

inst1-var :
  {x : Term} {ix ix' : ℕ} →
  inst1 x ix ≡ var ix' →
  ((ix ≡ zero) × (x ≡ var ix')) ⊎ (ix ≡ suc ix')
inst1-var {ix = zero} prf = inj₁ (refl ,, prf)
inst1-var {ix = suc ix} prf = inj₂ (cong suc (var-injective prf))

module _ where
  -- What are the implications of `sunder (inst1 x) ix ≡ var ix'`?

  module _ where
    _ : {x : Term} → sunder (inst1 x) zero ≡ var zero
    _ = refl

    _ : {x : Term} {ix ix' : ℕ} → (ix ≡ zero) × (ix' ≡ zero) → sunder (inst1 x) ix ≡ var ix'
    _ = λ{ (refl ,, refl) → refl }

  module _ where
    _ : {x : Term} → sunder (inst1 x) (suc zero) ≡ rename suc x
    _ = refl

    _ : {x : Term} {ix ix' : ℕ} → (ix ≡ suc zero) × (ix' > 0) × (x ≡ var (pred ix')) → sunder (inst1 x) ix ≡ var ix'
    _ = λ{ (refl ,, s≤s z≤n ,, refl) → refl }

  module _ where
    _ : {x : Term} → sunder (inst1 x) (suc (suc zero)) ≡ var (suc zero)
    _ = refl

    _ : {x : Term} {ix ix' : ℕ} → (ix ≡ suc (suc zero)) × (ix' ≡ suc zero) → sunder (inst1 x) ix ≡ var ix'
    _ = λ{ (refl ,, refl) → refl }

  module _ where
    _ : {x : Term} → sunder (inst1 x) (suc (suc (suc zero))) ≡ var (suc (suc zero))
    _ = refl

    _ : {x : Term} {ix ix' : ℕ} → (ix ≡ suc (suc (suc zero))) × (ix' ≡ suc (suc zero)) → sunder (inst1 x) ix ≡ var ix'
    _ = λ{ (refl ,, refl) → refl }

  module _ where
    _ : {x : Term} {ix ix' : ℕ} → (ix > 1) × (ix' ≡ pred ix) → sunder (inst1 x) ix ≡ var ix'
    _ =
      λ{
        (s≤s (s≤s ix) ,, refl) → refl
      }

sunder-inst1-var :
  {x : Term} {ix ix' : ℕ} →
  sunder (inst1 x) ix ≡ var ix' →
  ((ix ≡ 0) × (ix' ≡ 0)) ⊎
  ((ix > 0) × (x ≡ var (pred ix'))) ⊎
  ((ix > 1) × (ix' ≡ pred ix))
sunder-inst1-var {x} {zero} {.zero} refl = inj₁ (refl ,, refl)
sunder-inst1-var {x} {suc zero} {zero} prf = 
  ⊥-elim (rename-suc-≡-zero-⊥ _ prf)
sunder-inst1-var {x} {suc zero} {suc ix} prf =
  inj₂ (inj₁ (s≤s z≤n ,, rename-injective suc-injective _ _ prf))
sunder-inst1-var {x} {suc (suc ix)} {ix'} prf =
  inj₂ (inj₂ (s≤s (s≤s z≤n) ,, sym (var-injective prf)))

sunder-sunder-inst1-var :
  {x : Term} {ix ix' : ℕ} →
  sunder (sunder (inst1 x)) ix ≡ var ix' →
  ((ix ≡ 0) × (ix' ≡ 0)) ⊎
  ((ix ≡ 1) × (ix' ≡ 1)) ⊎
  ((ix > 1) × (x ≡ var (pred (pred ix')))) ⊎
  ((ix > 2) × (ix' ≡ pred ix))
sunder-sunder-inst1-var {x} {zero} {ix'} prf = inj₁ (refl ,, sym (var-injective prf))
sunder-sunder-inst1-var {x} {suc zero} {ix'} prf = inj₂ (inj₁ (refl ,, sym (var-injective prf)))
sunder-sunder-inst1-var {x} {suc (suc ix)} {zero} prf = ⊥-elim (rename-suc-≡-zero-⊥ _ prf)
sunder-sunder-inst1-var {x} {suc (suc ix)} {suc zero} prf = ⊥-elim (rename-suc-≡-zero-⊥ _ (rename-injective suc-injective _ _ prf))
sunder-sunder-inst1-var {x} {suc (suc ix)} {suc (suc ix')} prf =
  let prf' = rename-injective suc-injective _ _ (rename-injective suc-injective _ _ prf) in
  case inst1-var {x} {ix} prf' of λ{
    (inj₁ (refl ,, refl)) → inj₂ (inj₂ (inj₁ (s≤s (s≤s z≤n) ,, prf')))
    ;
    (inj₂ refl) → inj₂ (inj₂ (inj₂ (s≤s (s≤s (s≤s z≤n)) ,, cong suc (cong suc (var-injective prf')))))
  }

sunderₙ-var :
  {x : Term} {ix ix' : ℕ} →
  (ctx : Ctx) →
  sunderₙ ctx (inst1 x) ix ≡ var ix' →
  -- Ignored
  ((ix < length ctx) × (ix ≡ ix')) ⊎
  -- Instantiated
  ((suc ix > length ctx) × (x ≡ var (ix' ∸ length ctx))) ⊎
  -- Shifted
  ((ix > length ctx) × (ix' ≡ pred ix))
sunderₙ-var {x} {ix} {ix'} ◆ prf with inst1-var {x} {ix} prf
... | inj₁ (refl ,, refl) = inj₂ (inj₁ (s≤s z≤n ,, prf))
... | inj₂ refl = inj₂ (inj₂ (s≤s z≤n ,, var-injective prf))
sunderₙ-var {x} {zero} {ix'} (ctx , X) prf =
  let
    prf' =
      begin
        var zero
      ≡⟨⟩
        sunder (sunderₙ ctx (inst1 x)) zero
      ≡⟨ sym (sunderₙ-sunder-comm ctx) ⟩
        sunderₙ ctx (sunder (inst1 x)) zero
      ≡⟨ prf ⟩
        var ix'
      ∎
  in
    case prf' of λ{ refl → inj₁ (s≤s z≤n ,, refl) }
sunderₙ-var {x} {suc ix} {zero} (ctx , X) prf =
  let
    prf' =
      begin
        rename suc (sunderₙ ctx (inst1 x) ix)
      ≡⟨⟩
        sunder (sunderₙ ctx (inst1 x)) (suc ix)
      ≡⟨ sym (sunderₙ-sunder-comm ctx) ⟩
        sunderₙ ctx (sunder (inst1 x)) (suc ix)
      ≡⟨ prf ⟩
        var zero
      ∎
  in
    ⊥-elim (rename-suc-≡-zero-⊥ _ prf')
sunderₙ-var {x} {suc ix} {suc ix'} (ctx , X) prf =
  let
    prf' =
      begin
        rename suc (sunderₙ ctx (inst1 x) ix)
      ≡⟨⟩
        sunder (sunderₙ ctx (inst1 x)) (suc ix)
      ≡⟨ sym (sunderₙ-sunder-comm ctx) ⟩
        sunderₙ ctx (sunder (inst1 x)) (suc ix)
      ≡⟨ prf ⟩
        var (suc ix')
      ∎

    prf'' : sunderₙ ctx (inst1 x) ix ≡ var ix'
    prf'' = rename-injective suc-injective _ _ prf'

    prf''' :
      ix < length ctx × ix ≡ ix' ⊎
      suc ix > length ctx × x ≡ var (ix' ∸ length ctx) ⊎
      ix > length ctx × ix' ≡ pred ix
    prf''' = sunderₙ-var {x} {ix} {ix'} ctx prf''
  in
    case prf''' of λ{
      (inj₁ (ix<length-ctx ,, ix≡ix')) → inj₁ (s≤s ix<length-ctx ,, cong suc ix≡ix')
      ;
      (inj₂ (inj₁ (suc-ix<length-ctx ,, x≡var-ix'∸length-ctx))) → inj₂ (inj₁ (s≤s suc-ix<length-ctx ,, x≡var-ix'∸length-ctx))
      ;
      (inj₂ (inj₂ (ix>length-ctx ,, refl))) → inj₂ (inj₂ (s≤s ix>length-ctx ,, nonzero-pred-suc ix ix>length-ctx))
    }
  where
    nonzero-pred-suc : {n : ℕ} → (ix : ℕ) → ix > n → suc (pred ix) ≡ pred (suc ix)
    nonzero-pred-suc (suc ix) ix>n = refl

sunderₙ-Type :
  {x : Term} {ix : ℕ} {ctx : Ctx} →
  sunderₙ ctx (inst1 x) ix ≡ Type →
  (x ≡ Type) × (ix ≡ length ctx)
sunderₙ-Type {x = x} {ctx = ◆} prf =
  let ix≡zero ,, x≡Type = inst1-Type {x} prf in
  x≡Type ,, ix≡zero
sunderₙ-Type {x} {ix = zero} {ctx = ctx , X} prf =
  let
    prf' : sunder (sunderₙ ctx (inst1 x)) zero ≡ Type
    prf' = trans (sym (sunderₙ-sunder-comm ctx)) prf
  in
    case prf' of λ()
sunderₙ-Type {x} {ix = suc ix} {ctx = ctx , X} prf =
  let
    prf' : sunder (sunderₙ ctx (inst1 x)) (suc ix) ≡ Type
    prf' = trans (sym (sunderₙ-sunder-comm ctx)) prf

    prf'' : rename suc (sunderₙ ctx (inst1 x) ix) ≡ rename suc Type
    prf'' = prf'

    prf''' : sunderₙ ctx (inst1 x) ix ≡ Type
    prf''' = rename-injective suc-injective _ _ prf''

    x≡Type ,, ix≡length-ctx = sunderₙ-Type {x = x} {ix = ix} {ctx = ctx} prf'''
  in
    x≡Type ,, cong suc ix≡length-ctx

sunder-Pi :
  {x A B : Term} {ix : ℕ} →
  sunder (inst1 x) ix ≡ Pi A B →
  Σ[ A' ∈ Term ] Σ[ B' ∈ Term ] ((x ≡ Pi A' B') × (ix ≡ suc zero))
sunder-Pi {x = x} {ix = suc ix} prf with rename-Pi suc-injective (inst1 x ix) prf
... | A' ,, B' ,, ≡Pi ,, rename-A' ,, rename-B' =
  let ix≡zero ,, x≡PiA'B' = inst1-Pi {x = x} ≡Pi in
  A' ,, B' ,, x≡PiA'B' ,, cong suc ix≡zero

sunder-app :
  {x a b : Term} {ix : ℕ} →
  sunder (inst1 x) ix ≡ app a b →
  Σ[ a' ∈ Term ] Σ[ b' ∈ Term ] ((x ≡ app a' b') × (ix ≡ suc zero))
sunder-app {x = x} {ix = suc ix} prf with rename-app suc-injective (inst1 x ix) prf
... | a' ,, b' ,, ≡app ,, rename-a' ,, rename-b' =
  let ix≡zero ,, x≡appa'b' = inst1-app {x = x} ≡app in
  a' ,, b' ,, x≡appa'b' ,, cong suc ix≡zero
