module Partition where

open import Data.Bool using (Bool; true; false; if_then_else_; _∧_)
open import Data.Empty using (⊥; ⊥-elim)
open import Data.Product using (Σ-syntax)
open import Data.Unit using (⊤; tt)

IsTrue : Bool → Set
IsTrue true = ⊤
IsTrue false = ⊥

SetOf : Set → Set
SetOf A = A → Bool

_∈_ : {A : Set} → A → SetOf A → Bool
x ∈ xs = xs x

record Equivalence (A : Set) : Set where
  field
    _R_ : A → A → Bool
    reflexive : (x : A) → IsTrue (x R x)
    symmetric : (x y : A) → IsTrue (x R y) → IsTrue (y R x)
    transitive : (x y z : A) → IsTrue (x R y) → IsTrue (y R z) → IsTrue (x R z)

  [_] : A → SetOf A
  [_] = _R_

record PartialEquivalence (A : Set) : Set where
  field
    _R_ : A → A → Bool
    symmetric : (x y : A) → IsTrue (x R y) → IsTrue (y R x)
    transitive : (x y z : A) → IsTrue (x R y) → IsTrue (y R z) → IsTrue (x R z)

  [_] : A → SetOf A
  [_] = _R_

record Eq (A : Set) : Set where
  field
    _==_ : A → A → Bool
    ==-reflexive : (x : A) → IsTrue (x == x)
    ==-symmetric : (x y : A) → IsTrue (x == y) → IsTrue (y == x)
    ==-transitive : (x y z : A) → IsTrue (x == y) → IsTrue (y == z) → IsTrue (x == z)

record Empty (A : Set) : Set where
  field
    f : PartialEquivalence A
    correct :
      (x : A) →
      (let open PartialEquivalence f) →
      IsTrue (x R x) → ⊥

record New (A : Set) : Set where
  field
    f : A → PartialEquivalence A
    reflexive :
      (x : A) →
      (let pe = f x) →
      (let open PartialEquivalence pe) →
      IsTrue (x R x)

record Equate (A : Set) : Set where
  field
    f : A → A → PartialEquivalence A
    correct :
      (x y : A) →
      (let pe = f x y) →
      (let open PartialEquivalence pe) →
      IsTrue (x R y)

record Find (A : Set) : Set where
  field
    f : PartialEquivalence A → A → A
    Eq-A : Eq A
    canonical :
      (pe : PartialEquivalence A) →
      (let open PartialEquivalence pe) →
      (let open Eq Eq-A) →
      (x y : A) →
      IsTrue (x R y) →
      IsTrue (f pe x == f pe y)

record Union (A : Set) : Set where
  field
    f : PartialEquivalence A → PartialEquivalence A → PartialEquivalence A
    correct :
      (x y z : A) →
      (pe1 pe2 : PartialEquivalence A) →
      IsTrue (PartialEquivalence._R_ pe1 x y) →
      IsTrue (PartialEquivalence._R_ pe2 y z) →
      IsTrue (PartialEquivalence._R_ (f pe1 pe2) x z)

{-
open import Data.Product using (_×_; _,_; Σ-syntax)
open import Relation.Binary.PropositionalEquality using (_≡_; _≢_)

record Iso (A B : Set) : Set where
  field
    to : A → B
    from : B → A
    from-to : (x : A) → from (to x) ≡ x
    to-from : (x : B) → to (from x) ≡ x

module _ (A : Set) where

  open import Data.Empty using (⊥)
  open import Data.Sum using (_⊎_)
  open import Relation.Nullary using (¬_)

  Part : Set1
  Part = A → Set

  ∅ : Part
  ∅ = λ _ → ⊥

  _∪_ : Part → Part → Part
  a ∪ b = λ x → a x ⊎ b x

  _∩_ : Part → Part → Part
  a ∩ b = λ x → a x × b x

  record Partition (R : Set) : Set1 where
    field
      f : (r : R) → Part
      cover : Iso (Σ[ r ∈ R ] Σ[ x ∈ A ] (f r x)) A
      exclusive : (r1 r2 : R) → r1 ≢ r2 → (x : A) → ¬ (f r1 x × f r2 x)

data S1 : Set where
  A1 B1 C1 D1 : S1

data S2 : Set where
  A2 B2 : S2

test : Partition S1 S2
test =
  record
  { f = λ{ A2 → λ x → x ≡ A1 ; B2 → λ x → x ≡ B1 }
  ; cover = record
    { to = λ{
        (A2 , (b , c)) → {!!} ;
        (B2 , (b , c)) → {!!}
      }
    ; from = {!!}
    ; from-to = {!!}
    ; to-from = {!!}
    }
  ; exclusive = {!!}
  }

-}
