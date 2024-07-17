module Lang where

open import Data.Bool using (Bool; if_then_else_)
open import Data.Empty using (⊥; ⊥-elim)
open import Data.List as List using (List; _∷_; []; [_]; _++_)
import Data.List.Properties
open import Data.List.Relation.Unary.All using (All)
open import Data.Product using (Σ-syntax; _×_; _,_; proj₁; proj₂)
open import Data.Sum using (_⊎_; inj₁; inj₂)
open import Data.Unit using (⊤; tt)
open import Function using (_∘_)
open import Level using (Level)
open import Relation.Binary.PropositionalEquality as Eq using (_≡_; refl; cong; sym; subst)
open import Relation.Nullary.Decidable using (Dec; yes; no)
open import Relation.Nullary.Negation using (¬_)

module Part1 (A : Set) where
  Lang : Set1
  Lang = List A → Set

  {- A language is usually defined as a set of strings. "Set of strings" also means a function from string to bool;
  a predicate on strings.

  We're going to define languages in type theory using "type predicates". A function from string to type, where the
  "unit" type is analgous to "true" and the "empty" type is analogous to "false".
  -}

  Null : Lang
  Null string = ⊥

  Universal : Lang
  Universal string = ⊤

  Empty : Lang
  Empty string = string ≡ []

  Single : A → Lang
  Single c string = string ≡ [ c ]

  Union : Lang → Lang → Lang
  Union P Q string = P string ⊎ Q string

  Intersect : Lang → Lang → Lang
  Intersect P Q string = P string × Q string

  And : Set → Lang → Lang
  And s P string = s × P string

  Concat : Lang → Lang → Lang
  Concat P Q string = Σ[ prefix ∈ List A ] Σ[ suffix ∈ List A ] (string ≡ prefix ++ suffix × P prefix × Q suffix)

  Closure : Lang → Lang
  Closure P string = Σ[ parts ∈ List (List A) ] (string ≡ List.concat parts × All P parts)

{- When `P : Lang`, `P string` is a type. Therefore, `P string` is inhabited by a proof that `string` is in the
language `P`.
-}

module _ where
  open import Data.Char using (Char)
  open Part1 (Char)

  -- Every string is not in the `Null` language.
  _ : (string : List Char) → ¬(Null string)
  _ = λ string → λ Null-string → Null-string

  -- "c" is in the language {'c'}
  _ : Single 'c' [ 'c' ]
  _ = refl

  -- "cc" is not in the language {'c'}
  _ : ¬(Single 'c' ('c' ∷ 'c' ∷ []))
  _ = λ ()

  -- "ab" is in the language {"ab"}
  _ : Concat (Single 'a') (Single 'b') ('a' ∷ 'b' ∷ [])
  _ = 'a' ∷ [] , 'b' ∷ [] , refl , refl , refl

  -- "ab" is in the language a·b* == {"a", "ab", "abb", "abbb", ...}
  _ : Concat (Single 'a') (Closure (Single 'b')) ('a' ∷ 'b' ∷ [])
  _ = 'a' ∷ [] , 'b' ∷ [] , refl , refl , [ [ 'b' ] ] , refl , refl All.∷ All.[]

  -- "abab" is in the language (a·b)* == {"", "ab", "abab", ...}
  _ : Closure (Concat (Single 'a') (Single 'b')) ('a' ∷ 'b' ∷ 'a' ∷ 'b' ∷ [])
  _ =
    let ab∈ab = [ 'a' ] , [ 'b' ] , refl , refl , refl in
    ('a' ∷ 'b' ∷ []) ∷ ('a' ∷ 'b' ∷ []) ∷ [] , refl , ab∈ab All.∷ ab∈ab All.∷ All.[]

module Part2 (A : Set) where
  open Part1 (A) using (Lang; Concat; Union; And; Closure)
  open Eq.≡-Reasoning

  nullable : ∀{l : Level} → {B : Set l} → (List A → B) → B
  nullable f = f []

  derivative : ∀{l : Level} {B : Set l} → (List A → B) → List A → (List A → B)
  derivative f u = λ v → f (u ++ v)

  step : ∀{l : Level} {B : Set l} → (List A → B) → A → (List A → B)
  step f x = derivative f [ x ]

  step-Concat-→ :
    ∀{P Q : Lang} →
    (x : A) →
    (string : List A) →
    step (Concat P Q) x string →
    Union
      (And (nullable P) (step Q x))
      (Concat (step P x) Q) string
  step-Concat-→ {P} {Q} x string (u , v , x∷string≡u++v , Pu , Qv) with u
  ... | [] = inj₁ (Pu , subst (λ hole → Q hole) (sym x∷string≡u++v) Qv)
  ... | u ∷ us with x∷string≡u++v
  ... | refl = inj₂ (us , v , refl , Pu , Qv)

  step-Concat-← :
    ∀{P Q : Lang} →
    (x : A) →
    (string : List A) →
    Union
      (And (nullable P) (step Q x))
      (Concat (step P x) Q) string →
    step (Concat P Q) x string
  step-Concat-← {P} {Q} x string (inj₁ (nullable-P , step-Q)) = [] , x ∷ string , refl , nullable-P , step-Q
  step-Concat-← x string (inj₂ (u , v , refl , step-P , Qv)) = x ∷ u , v , refl , step-P , Qv

  step-Closure-→ :
    ∀{P : Lang} →
    (x : A) →
    (string : List A) →
    step (Closure P) x string →
    And (List (nullable P)) (Concat (step P x) (Closure P)) string
  step-Closure-→ x string ([] , () , All.[])
  step-Closure-→ {P} x string (part ∷ parts , x∷string≡concat-parts , P-part All.∷ All-P-parts) = [] , {!!} , {!!} , {!!} , {!!} , parts , refl , All-P-parts

  step-Closure-← :
    ∀{P : Lang} →
    (x : A) →
    (string : List A) →
    And (List (nullable P)) (Concat (step P x) (Closure P)) string →
    step (Closure P) x string
  step-Closure-← x string (s , u , v , string≡u++v , Pu , Qv) with string
  ... | [] = {![]!} , {!!} , Pu All.∷ {!!}
  ... | c ∷ cs  = (x ∷ u) ∷ {!!} , {!!} , Pu All.∷ {!!}

  nullable-derivative : ∀{B : Set} (f : List A → B) → (string : List A) → nullable (derivative f string) ≡ f string
  nullable-derivative f [] = refl
  nullable-derivative f (c ∷ cs) = 
    begin
      nullable (derivative f (c ∷ cs))
    ≡⟨⟩
      derivative f (c ∷ cs) []
    ≡⟨⟩
      f ((c ∷ cs) ++ [])
    ≡⟨⟩
      f (c ∷ cs ++ [])
    ≡⟨ cong (λ x → f (c ∷ x)) (Data.List.Properties.++-identityʳ cs) ⟩
      f (c ∷ cs)
    ∎

  derivative-[] : ∀{B : Set} → (f : List A → B) → (string : List A) → derivative f [] string ≡ f string
  derivative-[] f string = 
    begin
      derivative f [] string
    ≡⟨⟩
      f ([] ++ string)
    ≡⟨⟩
      f string
    ∎

  derivative-++ :
    ∀{B : Set} →
    (f : List A → B) →
    (u v string : List A) →
    derivative f (u ++ v) string ≡ derivative (derivative f u) v string
  derivative-++ f u v string = 
    begin
      derivative f (u ++ v) string
    ≡⟨⟩
      f ((u ++ v) ++ string)
    ≡⟨ cong f (Data.List.Properties.++-assoc u v string) ⟩
      f (u ++ (v ++ string))
    ≡⟨⟩
      derivative f u (v ++ string)
    ≡⟨⟩
      derivative (derivative f u) v string
    ∎

module Part3 (A : Set) (_≟_ : (x y : A) → Dec (x ≡ y)) where
  open Part1 (A) renaming
    ( Lang to LangP
    ; Null to NullP
    ; Universal to UniversalP
    ; Empty to EmptyP
    ; Single to SingleP
    ; Union to UnionP
    ; Intersect to IntersectP
    ; Concat to ConcatP
    ; Closure to ClosureP
    ; And to AndP
    )
  open Part2 (A) renaming
    ( nullable to nullableP
    ; step to stepP
    )

  data Lang : LangP → Set1 where
    null : Lang NullP
    universal : Lang UniversalP
    empty : Lang EmptyP
    single : (c : A) → Lang (SingleP c)
    and : ∀{s : Set} {P : LangP} → Dec s → Lang P → Lang (AndP s P)
    union : ∀{P Q : LangP} → Lang P → Lang Q → Lang (UnionP P Q)
    intersect : ∀{P Q : LangP} → Lang P → Lang Q → Lang (IntersectP P Q)
    concat : ∀{P Q : LangP} → Lang P → Lang Q → Lang (ConcatP P Q)
    closure : ∀{P : LangP} → Lang P → Lang (ClosureP P)
    iso :
      ∀{P Q : LangP} →
      ({string : List A} → (P string → Q string) × (Q string → P string)) →
      Lang P →
      Lang Q


  nullable : ∀{P : LangP} → Lang P → Dec (nullableP P)
  nullable null = no λ nullable-Null → nullable-Null
  nullable universal = yes tt
  nullable empty = yes refl
  nullable (single c) = no (λ ())

  nullable (and s lang) with s
  ... | no ¬prf = no (¬prf ∘ proj₁)
  ... | yes prf1 with nullable lang
  ... | no ¬prf = no (¬prf ∘ proj₂)
  ... | yes prf2 = yes (prf1 , prf2)

  nullable (union lang lang₁) with nullable lang
  ... | yes prf = yes (inj₁ prf)
  ... | no ¬prf1 with nullable lang₁
  ... | yes prf = yes (inj₂ prf)
  ... | no ¬prf2 = no λ{ (inj₁ x) → ¬prf1 x ; (inj₂ x) → ¬prf2 x }

  nullable (intersect lang lang₁) with nullable lang
  ... | no ¬prf = no λ (x , y) → ¬prf x
  nullable (intersect lang lang₁) | yes prf with nullable lang₁
  ... | yes prf2 = yes (prf , prf2)
  ... | no ¬prf = no λ (x , y) → ¬prf y

  nullable (concat {P} {Q} lang lang₁) with nullable lang
  ... | no ¬prf =
    no λ (u , v , []≡u++v , Pu , Qv) →
      ¬prf (subst (λ x → P x) (Data.List.Properties.++-conicalˡ u v (sym []≡u++v)) Pu)
  nullable (concat {P} {Q} lang lang₁) | yes prf1 with nullable lang₁
  ... | no ¬prf =
    no λ (u , v , []≡u++v , Pu , Qv) →
      ¬prf (subst (λ x → Q x) (Data.List.Properties.++-conicalʳ u v (sym []≡u++v)) Qv)
  ... | yes prf2 = yes ([] , [] , refl , prf1 , prf2)

  nullable (closure lang) =
    yes ([] , refl , All.[])

  nullable (iso P↔Q lang) with nullable lang
  ... | yes prf = yes (proj₁ P↔Q prf)
  ... | no ¬prf = no λ P[] → ¬prf (proj₂ P↔Q P[])


  step : ∀{P : LangP} → Lang P → (x : A) → Lang (stepP P x)
  step null x = null
  step universal x = universal
  step empty x = iso (λ {string} → ⊥-elim , λ ()) null
  step (and s lang) x = and s (step lang x)

  step (single c) x with c ≟ x
  ... | yes refl = iso (λ {string} → P→Q string , Q→P string) empty
    where
      P→Q : (string : List A) → EmptyP string → c ∷ string ≡ c ∷ []
      P→Q .[] refl = refl

      Q→P : (string : List A) → c ∷ string ≡ c ∷ [] → EmptyP string
      Q→P .[] refl = refl

  ... | no ¬prf = iso (λ {string} → P→Q string , Q→P string) null
    where
      P→Q : (string : List A) → NullP string → x ∷ string ≡ c ∷ []
      P→Q string prf = ⊥-elim prf

      Q→P : (string : List A) → x ∷ string ≡ c ∷ [] → NullP string
      Q→P .[] refl = ¬prf refl

  step {P} (concat lang lang₁) x =
    iso
      (λ {string} → step-Concat-← x string , step-Concat-→ x string)
      (union (and (nullable lang) (step lang₁ x)) (concat (step lang x) lang₁))

  step (closure lang) x = 
    iso
      {!!}
      {!!}

  step (union lang lang₁) x = {!!}
  step (intersect lang lang₁) x = {!!}
  step (iso P↔Q lang) x = {!!}
