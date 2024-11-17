{-# OPTIONS --safe --without-K #-}

module Presyntax where

open import Data.Nat using (ℕ; zero; suc)
open import Relation.Binary.PropositionalEquality as Eq using (_≡_; refl)

data Term : Set where
  var : ℕ → Term
  Type : Term

  -- types
  Void : Term
  Unit : Term
  Pi : (A B : Term) → Term
  Sigma : (A B : Term) → Term

  -- terms
  lam : (A e : Term) → Term
  app : (f x : Term) → Term

data Ctx : Set where
  ◆ : Ctx
  _,_ : Ctx → Term → Ctx

infixl 60 _,_

length : Ctx → ℕ
length ◆ = zero
length (ctx , _) = suc (length ctx)

_++_ : Ctx → Ctx → Ctx
a ++ ◆ = a
a ++ (b , x) = (a ++ b) , x

infixl 60 _++_

under : (ℕ → ℕ) → ℕ → ℕ
under f zero = zero
under f (suc ix) = suc (f ix)

underₙ : Ctx → (ℕ → ℕ) → ℕ → ℕ
underₙ ◆ f ix = f ix
underₙ (ctx , x) f ix = underₙ ctx (under f) ix

rename : (ℕ → ℕ) → Term → Term
rename f (var ix) = var (f ix)
rename f Type = Type
rename f Void = Void
rename f Unit = Unit
rename f (Pi A B) = Pi (rename f A) (rename (under f) B)
rename f (Sigma A B) = Sigma (rename f A) (rename (under f) B)
rename f (lam A e) = lam (rename f A) (rename (under f) e)
rename f (app a b) = app (rename f a) (rename f b)

rename-ctx : (ℕ → ℕ) → Ctx → Ctx
rename-ctx f ◆ = ◆
rename-ctx f (ctx , x) = rename-ctx f ctx , rename (underₙ ctx f) x

module _ where
  open import Relation.Binary.PropositionalEquality as Eq using (_≡_; refl)
  open Eq.≡-Reasoning

  _ :
    {A B C : Term} →
    rename-ctx suc (◆ , A , B , C) ≡
    (◆ , rename suc A , rename (under suc) B , rename (under (under suc)) C)
  _ = refl

sunder : (ℕ → Term) → ℕ → Term
sunder f zero = var zero
sunder f (suc ix) = rename suc (f ix)

sunderₙ : Ctx → (ℕ → Term) → ℕ → Term
sunderₙ ◆ f ix = f ix
sunderₙ (ctx , x) f ix = sunderₙ ctx (sunder f) ix

substitute : (ℕ → Term) → Term → Term
substitute f (var ix) = f ix
substitute f Type = Type
substitute f Void = Void
substitute f Unit = Unit
substitute f (Pi A B) = Pi (substitute f A) (substitute (sunder f) B)
substitute f (Sigma A B) = Sigma (substitute f A) (substitute (sunder f) B)
substitute f (lam A e) = lam (substitute f A) (substitute (sunder f) e)
substitute f (app a b) = app (substitute f a) (substitute f b)

inst1 : Term → ℕ → Term
inst1 x zero = x
inst1 x (suc ix) = var ix

_[_] : Term → Term → Term
a [ x ] = substitute (inst1 x) a
