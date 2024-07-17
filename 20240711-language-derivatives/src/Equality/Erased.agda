{-# OPTIONS --safe --erasure #-}
module Equality.Erased where

data _≡_ {A : Set} (@0 x : A) : @0 A → Set where
  refl : x ≡ x

trans : {A : Set} {a b c : A} → a ≡ b → b ≡ c → a ≡ c
trans refl refl = refl

sym : {A : Set} {a b : A} → a ≡ b → b ≡ a
sym refl = refl
