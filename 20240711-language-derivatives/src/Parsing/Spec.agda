module Parsing.Spec (A : Set) where

open import Data.Empty using (⊥)
open import Data.List using (List; []; _∷_; [_]; _++_; concat)
open import Data.List.Relation.Unary.All using (All)
open import Data.Product using (Σ-syntax; _×_)
open import Data.Sum using (_⊎_)
open import Data.Unit using (⊤)
open import Relation.Binary.PropositionalEquality using (_≡_)

Lang : Set1
Lang = List A → Set

ø : Lang
ø string = ⊥

𝒰 : Lang
𝒰 string = ⊤

ε : Lang
ε string = string ≡ []

‘_ : A → Lang
‘_ c string = string ≡ [ c ]

_∪_ : Lang → Lang → Lang
(P ∪ Q) string = P string ⊎ Q string

_·_ : Lang → Lang → Lang
(P · Q) string = Σ[ u ∈ List A ] Σ[ v ∈ List A ] (string ≡ u ++ v × P u × Q v)

_⋆ : Lang → Lang
_⋆ P string = Σ[ parts ∈ List (List A) ] (string ≡ concat parts × All P parts)

ν : ∀{l} {B : Set l} → (List A → B) → B
ν f = f []

Δ : ∀{l} {B : Set l} → (List A → B) → List A → (List A → B)
Δ f u = λ v → f (u ++ v)

δ : ∀{l} {B : Set l} → (List A → B) → A → (List A → B)
δ f x = Δ f [ x ]
