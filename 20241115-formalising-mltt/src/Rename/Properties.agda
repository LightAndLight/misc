{-# OPTIONS --safe --without-K #-}

module Rename.Properties where

open import Function using (_∘_)
open import Data.Nat using (ℕ; zero; suc)
open import Data.Nat.Properties using (suc-injective)
open import Data.Product using (Σ-syntax; _×_) renaming (_,_ to _,,_)
open import Relation.Binary.PropositionalEquality using (_≡_; refl; cong; cong₂; trans)
open import Relation.Nullary using (¬_)

open import Presyntax
open import Presyntax.Properties
