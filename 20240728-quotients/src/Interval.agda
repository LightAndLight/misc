{-# OPTIONS --safe --with-K #-}
module Interval where

open import Data.Product using (Σ-syntax; _×_; _,_; proj₁; proj₂)
open import Level using (Level; _⊔_) renaming (zero to lzero; suc to lsuc)
import Relation.Binary.PropositionalEquality as Eq

module HasPath where
  record HasPath {l1 l2 : Level} (A : Set l1) : Set (lsuc (l1 ⊔ l2)) where
    field
      Path : A → A → Set l2

      refl : {x : A} → Path x x

  module Def-Set where
    open import Function using (_↔_)
    data Path-Set : Set → Set → Set1 where
      refl : {x : Set} → Path-Set x x
      iso : {x y : Set} → x ↔ y → Path-Set x y

  open Def-Set using (Path-Set) public

  Path-Set-run : {X Y : Set} → Path-Set X Y → X → Y
  Path-Set-run Path-Set.refl x = x
  Path-Set-run (Path-Set.iso i) x = Function.Inverse.to i x
    where
      import Function

  instance
    HasPath-Path-Set : HasPath Set
    HasPath-Path-Set =
      record {
        Path = Path-Set
        ;
        refl = Path-Set.refl
      }
      where
        open import Function using (mk↔)

  module Def-Σ where
    data Path-Σ : {A : Set} {B : A → Set} → Σ[ x ∈ A ] (B x) → Σ[ x ∈ A ] (B x) → Set1 where
      refl : {A : Set} {B : A → Set} {x : Σ[ y ∈ A ] (B y)} → Path-Σ x x

    instance
      HasPath-Path-Σ : {A : Set} {B : A → Set} → HasPath {l2 = lsuc lzero} (Σ[ x ∈ A ] (B x))
      HasPath-Path-Σ {A} {B} =
        record {
          Path = Path-Σ
          ;
          refl = Path-Σ.refl
        }

  open HasPath {{...}}

  record HasPathCong
    {l1 l2 l3 l4 : Level}
    {A : Set l1}
    {{HasPath-A : HasPath {l2 = l3} A}}
    {B : Set l2}
    {{HasPath-B : HasPath {l2 = l4} B}}
    (f : A → B)
    : Set (l1 ⊔ l2 ⊔ l3 ⊔ l4) where
    field
      cong : {x y : A} → Path x y → Path (f x) (f y)
      cong-refl : {x : A} → cong (refl {x = x}) Eq.≡ refl {x = f x}

  open HasPathCong {{...}}

  Path-elim : {l : Level} {A : Set} {{_ : HasPath {l2 = l} A}} {P : A → Set} {{_ : HasPathCong P}} {x y : A} → Path x y → P x → P y
  Path-elim path = Path-Set-run (cong path)

  Path-elim-refl :
    {l : Level} {A : Set} {{_ : HasPath {l2 = l} A}} {P : A → Set} {{_ : HasPathCong P}} {x : A} {Px : P x} →
    Path-elim (refl {x = x}) Px Eq.≡ Px
  Path-elim-refl {x = x} {Px = Px} rewrite cong-refl {x = x} = Eq.refl

  Path-sym : {A : Set} {x y : A} {{_ : HasPath A}} {{_ : HasPathCong (λ a → Path a x)}} → Path x y → Path y x
  Path-sym {A} {x} path = Path-elim {P = λ a → Path a x} path (refl {x = x})

  open Def-Σ using (Path-Σ)

  record HasPathInd
    {A : Set}
    {{HasPath-A : HasPath A}}
    (P : {x y : A} → Path x y → Set)
    : Set1 where
    field
      ind :
        {x b : A} →
        (path : Path x b) →
        Path-Σ {A = A} {B = Path x} (x , refl {x = x}) (b , path)

  open HasPathInd {{...}}

  instance
    _ :
      {A : Set} {{_ : HasPath A}} →
      {x : A} →
      {P : {y : A} → Path x y → Set} →
      HasPathCong (λ p → P {proj₁ p} (proj₂ p))
    _ =
      record {
        cong = λ{ Path-Σ.refl → Path-Set.refl }
        ;
        cong-refl = Eq.refl
      }

  Path-ind :
    {A : Set} →
    {{_ : HasPath A}} →
    {P : {x y : A} → Path x y → Set} →
    {{_ : HasPathInd P}} →
    (base : {x : A} → P (refl {x = x})) →
    {x y : A} →
    (path : Path x y) →
    P path
  Path-ind {A} {P} base {x = x} path =
    Path-elim {P = λ p → P {x} {proj₁ p} (proj₂ p) } (ind path) base

module Def-Bool where
  open import Data.Bool using (Bool)

  open HasPath using (HasPath)

  data Path-Bool : Bool → Bool → Set where
    refl : {x : Bool} → Path-Bool x x

  instance
    HasPath-Path-Bool : HasPath Bool
    HasPath-Path-Bool =
      record {
        Path = Path-Bool
        ;
        refl = Path-Bool.refl
      }

module Def-𝕀 where
  data 𝕀 : Set where
    zero : 𝕀
    one : 𝕀
    -- segment : Path 𝕀 zero one

  data Path-𝕀 : 𝕀 → 𝕀 → Set where
    refl : {x : 𝕀} → Path-𝕀 x x

    𝕀-segment : Path-𝕀 zero one
    𝕀-segment-sym : Path-𝕀 one zero

  open HasPath

  instance
    HasPath-Path-𝕀 : HasPath 𝕀
    HasPath-Path-𝕀 =
      record {
        Path = Path-𝕀
        ;
        refl = Path-𝕀.refl
      }

    _ : {x : 𝕀} → HasPathCong (λ a → Path-𝕀 a x)
    _ =
      record {
        cong =
          λ{
            Path-𝕀.refl → Path-Set.refl
            ;
            Path-𝕀.𝕀-segment →
              Path-Set.iso zero↔one
            ;
            Path-𝕀.𝕀-segment-sym →
              Path-Set.iso (mk↔ (Inverse.inverseʳ zero↔one , Inverse.inverseˡ zero↔one))
          }
        ;
        cong-refl = Eq.refl
      }
      where
        open import Function using (Inverse; _↔_; mk↔)

        zero↔one : {x : 𝕀} → Path-𝕀 zero x ↔ Path-𝕀 one x
        zero↔one = 
          mk↔
            {to = λ{ Path-𝕀.refl → Path-𝕀.𝕀-segment-sym ; Path-𝕀.𝕀-segment → Path-𝕀.refl }}
            {from = λ{ Path-𝕀.refl → Path-𝕀.𝕀-segment ; Path-𝕀.𝕀-segment-sym → Path-𝕀.refl }}
            (
              (λ{ {Path-𝕀.refl} → λ{ Eq.refl → Eq.refl } ; {Path-𝕀.𝕀-segment-sym} → λ{ Eq.refl → Eq.refl } })
              ,
              (λ{ {Path-𝕀.refl} → λ{ Eq.refl → Eq.refl } ; {Path-𝕀.𝕀-segment} → λ{ Eq.refl → Eq.refl } })
            )

module Def-𝕊₁ where
  data 𝕊₁ : Set where
    base : 𝕊₁
    -- loop : Path 𝕊₁ base base

  data Path-𝕊₁ : 𝕊₁ → 𝕊₁ → Set where
    refl : {x : 𝕊₁} → Path-𝕊₁ x x

    𝕊₁-loop : Path-𝕊₁ base base

  open HasPath

  instance
    HasPath-Path-𝕊₁ : HasPath 𝕊₁
    HasPath-Path-𝕊₁ =
      record {
        Path = Path-𝕊₁
        ;
        refl = Path-𝕊₁.refl
      }

module Def-Path-𝕊₁ where
  open Def-𝕊₁ using (𝕊₁; Path-𝕊₁)

  data Path-Path-𝕊₁ : {x y x' y' : 𝕊₁} → Path-𝕊₁ x y → Path-𝕊₁ x' y' → Set where
    refl : {x y : 𝕊₁} {p : Path-𝕊₁ x y} → Path-Path-𝕊₁ p p

    loop : {x : 𝕊₁} → Path-Path-𝕊₁ {x} {x} {𝕊₁.base} {𝕊₁.base} Path-𝕊₁.refl Path-𝕊₁.𝕊₁-loop

  open HasPath

  instance
    HasPath-Path-Path-𝕊₁ : {x y : 𝕊₁} → HasPath (Path-𝕊₁ x y)
    HasPath-Path-Path-𝕊₁ =
      record {
        Path = Path-Path-𝕊₁
        ;
        refl = Path-Path-𝕊₁.refl
      }

open HasPath.HasPath {{...}} public
open HasPath.HasPathCong {{...}} public
open HasPath using (HasPathCong)

module Use-𝕀 where
  open import Data.Bool using (Bool; true; false)

  open Def-𝕀

  isZero : 𝕀 → Bool
  isZero zero = true
  isZero one = false
  {- isZero segment = ... impossible

  Need to prove:

  * Path Bool (isZero zero) (isZero one)
  * Path Bool true false
  * not inhabited

  instance
    HasPathCong-isZero : HasPathCong isZero
    HasPathCong-isZero =
      record {
        cong =
          λ{
            Path-𝕀.refl → Def-Bool.refl
            ;
            Path-𝕀.𝕀-segment →
              -- Impossible (true ≡ false)
              {!!}
          }
        ;
        cong-refl = Eq.refl
      }
  -}

  truthy : 𝕀 → Bool
  truthy zero = true
  truthy one = true
  -- isZero segment = refl

  instance
    HasPathCong-truthy : HasPathCong truthy
    HasPathCong-truthy =
      record {
        cong = λ {x} {y} →
          λ{
            refl → 
              case x returning (λ x → Def-Bool.Path-Bool (truthy x) (truthy x)) of λ{
                zero → Def-Bool.Path-Bool.refl
                ;
                one → Def-Bool.Path-Bool.refl
              }
            ;
            𝕀-segment → Def-Bool.Path-Bool.refl
            ;
            𝕀-segment-sym → Def-Bool.Path-Bool.refl
          }
        ;
        cong-refl = λ{ {zero} → Eq.refl ; {one} → Eq.refl }
      }
      where
        open import Function using (case_returning_of_)

module Use-𝕊₁ where
  open HasPath using (HasPath; HasPathInd; Path-Set)

  {-
  open Def-Σ
  open Def-𝕊₁
  open Def-Path-𝕊₁ using (Path-Path-𝕊₁)

  instance
    _ : {A : Set} {{_ : HasPath A}} → HasPathCong {A = A} (λ _ → Path {A = 𝕊₁} base base)
    _ = record { cong = λ p → Path-Set.refl }

  test1 : Path {A = Σ[ x ∈ 𝕊₁ ] (Path {A = 𝕊₁} base base)} (base , 𝕊₁-loop) (base , Path-𝕊₁.refl)
  test1 = Σ-pair Path-𝕊₁.refl {!!}

  instance
    _ : HasPathCong (λ x → Path {A = 𝕊₁} base x)
    _ = record { cong = λ{ Path-𝕊₁.refl → Path-Set.refl ; Path-𝕊₁.𝕊₁-loop → Path-Set.refl } }

    _ : HasPathInd (λ {x} {y} p → Path-Path-𝕊₁ {x} {y} {x} {x} p Path-𝕊₁.refl)
    _ =
      record {
        ind =
          λ {x} {y} →
          λ{
            Path-𝕊₁.refl → Path-Set.refl
            ;
            Path-𝕊₁.𝕊₁-loop →
              {!!}
              {-
              Path-Set.iso
                (mk↔
                  {to = λ{ Path-Path-𝕊₁.refl → Path-Path-𝕊₁.loop-sym }}
                  {from = λ{ Path-Path-𝕊₁.loop-sym → Path-Path-𝕊₁.refl }}
                  (
                    (λ{ Eq.refl → {!!} })
                    ,
                    (λ{ Eq.refl → {!!} })
                  )
                )
              -}
          }
      }
      where
        open import Function using (mk↔)

  test2 : Path {A = Σ[ x ∈ 𝕊₁ ] (Path {A = 𝕊₁} base x)} (base , 𝕊₁-loop) (base , Path-𝕊₁.refl)
  test2 = Σ-pair Path-𝕊₁.refl (HasPath.Path-ind {P = λ x → Path-Path-𝕊₁ x Path-𝕊₁.refl} Path-Path-𝕊₁.refl 𝕊₁-loop)
  -}
