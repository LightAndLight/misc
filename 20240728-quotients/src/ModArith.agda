module ModArith where

open import Data.Nat using (ℕ; zero; suc; _+_)
open import Data.Product using (_,_)
open import Function using (_↔_; mk↔)
open import Relation.Binary.PropositionalEquality as Eq using (_≡_; refl)
open import Data.Unit using (⊤)

data Equiv (A : Set) (x y : A) : Set where
  equiv : ({P : A → ⊤} → P x ↔ P y) → Equiv A x y

subst : {A : Set} {x y : A} {P : A → ⊤} → Equiv A x y → P x → P y
subst = ?

{-
data ℕmod[_]∋_ (n : ℕ) : ℕ → Set where
  z : ℕmod[ n ]∋ 0
  s : {m : ℕ} → ℕmod[ n ]∋ m → ℕmod[ n ]∋ (m + n)

record ℕmod (n : ℕ) : Set1 where
  field
    value : ℕ
    loop : ℕmod[ n ]∋ value
{-

z : {n : ℕ} → ℕmod n
z = record { value = 0 ; loop = mk↔ {to = {!!}} {from = {!!}} ({!!} , {!!}) }

elim :
  {n : ℕ} →
  {P : ℕ → Set} →
  (P 0 ↔ P n) →
  ({m : ℕ} → P m ↔ P (m + n) → P (suc m) ↔ P (suc m + n)) →
  (m : ℕ) →
  P m
elim = {!!}

-}

-}
