module Partition2 where

module PartialEquivalence where
  open import Data.Bool using (Bool; true; false; if_then_else_; _∧_)
  open import Data.Empty using (⊥; ⊥-elim)
  open import Data.Product using (_×_; _,_; Σ-syntax; ∃-syntax)
  open import Data.Maybe using (Maybe; just; nothing)
  open import Data.Sum using (_⊎_; inj₁; inj₂)
  open import Data.Unit using (⊤; tt)
  open import Relation.Nullary using (¬_)
  open import Relation.Binary.PropositionalEquality using (_≡_; refl)

  SetOf : Set → Set1
  SetOf A = A → Set

  {-
  _∈_ : {A : Set} → A → SetOf A → Set
  x ∈ xs = xs x
  -}

  record Equivalence (A : Set) : Set1 where
    field
      _R_ : A → A → Set
      reflexive : (x : A) → x R x
      symmetric : (x y : A) → x R y → y R x
      transitive : (x y z : A) → x R y → y R z → x R z

    [_] : A → SetOf A
    [_] = _R_

  record PartialEquivalence (A : Set) : Set1 where
    field
      _R_ : A → A → Set
      symmetric : (x y : A) → x R y → y R x
      transitive : (x y z : A) → x R y → y R z → x R z

    [_] : A → SetOf A
    [_] = _R_

  empty : {A : Set} → PartialEquivalence A
  empty =
    record
    { _R_ = λ _ _ → ⊥
    ; symmetric = λ _ _ x → ⊥-elim x
    ; transitive = λ _ _ _ x → ⊥-elim x
    }

  empty-correct :
    {A : Set} →
    (let open PartialEquivalence empty) →
    (x y : A) →
    ¬(x R y)
  empty-correct x y xRy = xRy

  single : {A : Set} → A → PartialEquivalence A
  single a =
    record
    { _R_ = λ x y → a ≡ x × a ≡ y
    ; symmetric = λ x y (a≡x , a≡y) → (a≡y , a≡x)
    ; transitive = λ x y z (a≡x , a≡y) (a≡y , a≡z) → (a≡x , a≡z)
    }

  record _⇔_ (A B : Set) : Set where
    field
      to : A → B
      from : B → A

  single-correct :
    {A : Set} →
    (x : A) →
    (let open PartialEquivalence (single x)) →
    (x R x)
  single-correct x =
    refl , refl

  equate : {A : Set} → A → A → PartialEquivalence A
  equate a b =
    record
    { _R_ = λ x y → (a ≡ x ⊎ b ≡ x) × (a ≡ y ⊎ b ≡ y)
    ; symmetric = λ x y (pa , pb) → pb , pa
    ; transitive = λ x y z (pa , pb) (pb , pc) → pa , pc
    }

  equate-correct :
    {A : Set} →
    (x y : A) →
    (let open PartialEquivalence (equate x y)) →
    (x R y) 
  equate-correct x y =
    (inj₁ refl) , inj₂ refl

  xRy-reflexive-l :
    {A : Set} →
    (x y : A) →
    (pe : PartialEquivalence A) →
    (let open PartialEquivalence pe) →
    x R y →
    x R x
  xRy-reflexive-l x y pe xRy = 
    let open PartialEquivalence pe in
    transitive x y x xRy (symmetric x y xRy)

  xRy-reflexive-r :
    {A : Set} →
    (x y : A) →
    (pe : PartialEquivalence A) →
    (let open PartialEquivalence pe) →
    x R y →
    y R y
  xRy-reflexive-r x y pe xRy = 
    let open PartialEquivalence pe in
    transitive y x y (symmetric x y xRy) xRy

  data Path {A : Set} (R : A → A → Set) : A → A → Set where
    done : {x y : A} → R x y → Path R x y
    cons : {x y z : A} → R x y → Path R y z → Path R x z

  Path-snoc :
    {A : Set} {R : A → A → Set} {x y z : A} →
    Path R x y →
    R y z →
    Path R x z
  Path-snoc (done Rxy) Ryz = cons Rxy (done Ryz)
  Path-snoc (cons Rxy PathR-y-z) Ryz = cons Rxy (Path-snoc PathR-y-z Ryz)

  {- Note: this is just an O(n^2) list reversal.

  A linear-time version would be better.
  -}
  Path-symmetric :
    {A : Set} {R : A → A → Set} →
    ((x y : A) → R x y → R y x) →
    (x y : A) →
    Path R x y →
    Path R y x
  Path-symmetric sym x y (done Rxy) = done (sym x y Rxy)
  Path-symmetric sym x z (cons {x} {y} {z} Rxy PathRyz) with Path-symmetric sym y z PathRyz
  ... | PathRzy = Path-snoc PathRzy (sym x y Rxy)

  Path-transitive :
    {A : Set} {R : A → A → Set} →
    (x y z : A) →
    Path R x y →
    Path R y z →
    Path R x z
  Path-transitive x y z (done Rxy) PathRyz = cons Rxy PathRyz
  Path-transitive x y z (cons {x} {x'} {y} Rxx' PathRx'y) PathRyz = cons Rxx' (Path-transitive x' y z PathRx'y PathRyz)

  union : {A : Set} → (pe₁ pe₂ : PartialEquivalence A) → PartialEquivalence A
  union pe₁ pe₂ =
    let open PartialEquivalence pe₁ renaming (_R_ to _R₁_; symmetric to sym₁; transitive to trans₁) in
    let open PartialEquivalence pe₂ renaming (_R_ to _R₂_; symmetric to sym₂; transitive to trans₂) in
    record
    { _R_ = Path (λ x y → (x R₁ y) ⊎ (x R₂ y))
    ; symmetric = Path-symmetric λ x y → λ{ (inj₁ xR₁y) → inj₁ (sym₁ x y xR₁y) ; (inj₂ xR₂y) → inj₂ (sym₂ x y xR₂y) } 
    ; transitive = Path-transitive
    }

  union-correct :
    {A : Set} →
    (pe₁ pe₂ : PartialEquivalence A) →
    (let open PartialEquivalence pe₁ renaming (_R_ to _R₁_)) →
    (let open PartialEquivalence pe₂ renaming (_R_ to _R₂_)) →
    (let open PartialEquivalence (union pe₁ pe₂) renaming (_R_ to _R₃_)) →
    ((x y : A) → x R₁ y → x R₃ y) ×
    ((x y : A) → x R₂ y → x R₃ y) ×
    ((x y z : A) → x R₁ y → y R₂ z → x R₃ z)
  union-correct pe₁ pe₂ =
    (λ x y xR₁y → done (inj₁ xR₁y))
    ,
    (λ x y xR₂y → done (inj₂ xR₂y))
    ,
    (λ x y z xR₁y yR₂z → cons (inj₁ xR₁y) (done (inj₂ yR₂z)))

  union-empty-r : {A : Set} {a : PartialEquivalence A} → union a empty ≡ a
  union-empty-r = {!refl!}

module NaiveDisjointSet where
  open import Function using (case_of_)
  open import Data.Product using (_×_; _,_; ∃-syntax; Σ-syntax)
  open import Data.List using (List; []; _∷_; _++_; foldr)
  open import Data.List.Membership.Propositional using (_∈_)
  open import Data.List.Membership.Propositional.Properties as Eq using (∈-++⁺ʳ)
  open import Relation.Binary.PropositionalEquality as Eq using (_≡_; refl)
  open import Relation.Nullary using (Dec; yes; no)

  open PartialEquivalence using (PartialEquivalence)

  NaiveDisjointSet : Set → Set
  NaiveDisjointSet A = List (List A)

  data Part {A : Set} : NaiveDisjointSet A → A → List A → Set where
    here : {x : A} {xs : List A} {xss : NaiveDisjointSet A} → x ∈ xs → Part (xs ∷ xss) x xs
    there : {x : A} {xs xs' : List A} {xss : NaiveDisjointSet A} → Part xss x xs → Part (xs' ∷ xss) x xs

  Part-swap :
    {A : Set} {xss : NaiveDisjointSet A} {xs : List A} {x y : A} →
    y ∈ xs →
    Part xss x xs →
    (x ∈ xs) × Part xss y xs
  Part-swap y∈xs (here x∈xs) = x∈xs , here y∈xs
  Part-swap y∈xs (there xs-partof-xss) with Part-swap y∈xs xs-partof-xss
  ... | x∈xs , result = x∈xs , there result

  ⟦_⟧ : {A : Set} → NaiveDisjointSet A → PartialEquivalence A
  ⟦_⟧ [] = PartialEquivalence.empty
  ⟦_⟧ (xs ∷ xss) = foldr (λ x → PartialEquivalence.union (PartialEquivalence.single x)) ⟦ xss ⟧ xs

  CorrectNaiveDisjointSet : (A : Set) → PartialEquivalence A → Set1
  CorrectNaiveDisjointSet A μ = Σ[ xss ∈ NaiveDisjointSet A ] (⟦ xss ⟧ ≡ μ)

  empty : {A : Set} → CorrectNaiveDisjointSet A PartialEquivalence.empty
  empty = [] , refl

  single : {A : Set} → (x : A) → CorrectNaiveDisjointSet A (PartialEquivalence.single x)
  single x = (x ∷ []) ∷ [] , {!!}

  equate : {A : Set} → (x y : A) → CorrectNaiveDisjointSet A (PartialEquivalence.equate x y)
  equate x y = {!!} , {!!}

  union :
    {A : Set} {μ₁ μ₂ : PartialEquivalence A} →
    CorrectNaiveDisjointSet A μ₁ →
    CorrectNaiveDisjointSet A μ₂ →
    CorrectNaiveDisjointSet A (PartialEquivalence.union μ₁ μ₂)
  union (x , x-correct) (y , y-correct) = {!!} , {!!}

  equal :
    {A : Set} {μ : PartialEquivalence A} →
    (x y : A) →
    (xss : CorrectNaiveDisjointSet A μ) →
    (let open PartialEquivalence.PartialEquivalence μ) →
    Dec (x R y)
  equal x y ([] , refl) = no λ x → x
  equal x y (xs ∷ xss , refl) = {!!}
