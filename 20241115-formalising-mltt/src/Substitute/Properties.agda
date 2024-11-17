{-# OPTIONS --safe --without-K #-}

module Substitute.Properties where

open import Function using (_∘_)
open import Data.Nat using (ℕ; zero; suc; pred)
open import Data.Nat.Properties using (suc-injective)
open import Data.Product using (_×_) renaming (_,_ to _,,_)
open import Data.Sum using (_⊎_; inj₁; inj₂)
open import Relation.Binary.PropositionalEquality as Eq using (_≡_; refl; cong; cong₂; sym)
open Eq.≡-Reasoning

open import Presyntax
open import Presyntax.Properties
open import Rename
open import Rename.Properties
open import Substitute

