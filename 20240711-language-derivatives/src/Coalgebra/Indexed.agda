{-# OPTIONS --safe --erasure #-}

open import Data.Nat using (ℕ)

open import Functor.Indexed using (IFunctor; IFunctorEquiv)

module Coalgebra.Indexed
  (F : (@0 ℕ → Set) → (@0 ℕ → Set))
  {F-IFunctor : IFunctor F}
  {F-IFunctorEquiv : IFunctorEquiv F {F-IFunctor = F-IFunctor}}
  where

open import Data.Product using (Σ-syntax)

open import Category.Indexed using (∀[_]; _⇒_)
open IFunctor F-IFunctor
open IFunctorEquiv F-IFunctorEquiv

record ICoalgebra : Set1 where
  field
    {Carrier} : @0 ℕ → Set
    coalgebra : ∀[ Carrier ⇒ F Carrier ]
    Equiv : {@0 i : ℕ} → Carrier i → Carrier i → Set

  apply : ∀[ Carrier ⇒ F Carrier ]
  apply = coalgebra

record ICoalgebraArrow (x y : ICoalgebra) : Set1 where
  X : @0 ℕ → Set
  X = ICoalgebra.Carrier x

  Y : @0 ℕ → Set
  Y = ICoalgebra.Carrier y

  field
    f : ∀[ X ⇒ Y ]
    correct :
      {@0 i : ℕ} →
      (arg : X i) →
      Equiv (ICoalgebra.Equiv y) (ifmap f (ICoalgebra.apply x arg)) (ICoalgebra.apply y (f arg))

  apply : ∀[ X ⇒ Y ]
  apply = f

_ICoalgebraArrow-≡_ : {x y : ICoalgebra} → (f g : ICoalgebraArrow x y) → Set
_ICoalgebraArrow-≡_ {x} {y} f g =
  {@0 i : ℕ} →
  (arg : ICoalgebra.Carrier x i) →
  ICoalgebra.Equiv y (ICoalgebraArrow.apply f arg) (ICoalgebraArrow.apply g arg)

IUnique : {x y : ICoalgebra} → ICoalgebraArrow x y → Set1
IUnique {x} {y} f = (g : ICoalgebraArrow x y) → f ICoalgebraArrow-≡ g

ITerminal : (𝟙 : ICoalgebra) → Set1
ITerminal 𝟙 = (x : ICoalgebra) → Σ[ f ∈ ICoalgebraArrow x 𝟙 ] (IUnique f)
