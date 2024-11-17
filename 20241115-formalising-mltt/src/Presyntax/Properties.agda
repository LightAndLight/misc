{-# OPTIONS --safe --without-K #-}

module Presyntax.Properties where

open import Function using (_∘_; case_of_)
open import Data.Empty using (⊥-elim)
open import Data.Nat using (ℕ; suc; zero; pred; _+_; _>_; s≤s; z≤n)
open import Data.Nat.Properties using (suc-injective)
open import Data.Product using (Σ-syntax; _×_) renaming (_,_ to _,,_)
open import Data.Sum using (_⊎_; inj₁; inj₂)
open import Relation.Binary.PropositionalEquality as Eq using (_≡_; refl; cong; cong₂; trans; sym)
open Eq.≡-Reasoning
open import Relation.Nullary using (¬_)

open import Presyntax

var-injective : {ix ix' : ℕ} → var ix ≡ var ix' → ix ≡ ix'
var-injective refl = refl

Pi-injective₁ : {A A' B B' : Term} → Pi A B ≡ Pi A' B' → A ≡ A'
Pi-injective₁ refl = refl

Pi-injective₂ : {A A' B B' : Term} → Pi A B ≡ Pi A' B' → B ≡ B'
Pi-injective₂ refl = refl

Sigma-injective₁ : {A A' B B' : Term} → Sigma A B ≡ Sigma A' B' → A ≡ A'
Sigma-injective₁ refl = refl

Sigma-injective₂ : {A A' B B' : Term} → Sigma A B ≡ Sigma A' B' → B ≡ B'
Sigma-injective₂ refl = refl

lam-injective₁ : {A A' e e' : Term} → lam A e ≡ lam A' e' → A ≡ A'
lam-injective₁ refl = refl

lam-injective₂ : {A A' e e' : Term} → lam A e ≡ lam A' e' → e ≡ e'
lam-injective₂ refl = refl

app-injective₁ : {a a' b b' : Term} → app a b ≡ app a' b' → a ≡ a'
app-injective₁ refl = refl

app-injective₂ : {a a' b b' : Term} → app a b ≡ app a' b' → b ≡ b'
app-injective₂ refl = refl

++-,-assoc : (a b : Ctx) (c : Term) → (a ++ b) , c ≡ a ++ (b , c)
++-,-assoc a ◆ c = refl
++-,-assoc a (b , x) c = refl

◆-++-id : {a : Ctx} → ◆ ++ a ≡ a
◆-++-id {◆} = refl
◆-++-id {a , x} = cong (_, x) ◆-++-id

