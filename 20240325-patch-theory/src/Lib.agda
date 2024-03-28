module Lib where

open import Data.Bool
open import Relation.Nullary using (Dec)
open import Relation.Binary.PropositionalEquality

module Patch (Line : Set) (eqLine : (a b : Line) → Dec (a ≡ b)) where
  open import Data.List
  open import Data.List.Properties
  open import Data.Nat
  open import Data.Product

  data Patch : List Line → List Line → Set where
    empty :
      ∀{a} → Patch a a
    modify :
      ∀{lines} →
      (oldLine newLine : Line) →
      Patch (oldLine ∷ lines) (newLine ∷ lines)
    delete :
      ∀{oldLines newLines} →
      (deletedLines : List Line) →
      {correct : oldLines ≡ deletedLines ++ newLines} →
      Patch oldLines newLines
    insert :
      ∀{oldLines newLines} →
      (insertedLines : List Line) →
      {correct : newLines ≡ insertedLines ++ oldLines} →
      Patch oldLines newLines


  compose : ∀{a b c} → Patch a b → Patch b c → Patch a c

  -- empty then
  compose empty a = a

  -- modify then
  compose (modify oldLine newLine) empty =
    modify oldLine newLine

  compose (modify oldLine newLine) (modify .newLine newLine') =
    modify oldLine newLine'

  compose (modify oldLine newLine) (delete deletedLines {correct}) with deletedLines
  ... | [] rewrite sym correct = modify oldLine newLine
  compose (modify oldLine newLine) (delete deletedLines {correct}) | d ∷ ds with ∷-injective correct
  ... | refl , refl = delete (oldLine ∷ ds) {correct = refl}

  compose (modify oldLine newLine) (insert insertedLines) = {!!}

  -- delete then
  compose (delete deletedLines {correct = refl}) empty =
    delete deletedLines {correct = refl}

  compose (delete deletedLines {correct = refl}) (modify oldLine newLine) = {!!}

  compose (delete deletedLines {correct = refl}) (delete deletedLines' {correct = refl}) =
    delete (deletedLines ++ deletedLines') {correct = sym (++-assoc deletedLines deletedLines' _)}

  compose (delete deletedLines {correct = refl}) (insert insertedLines {correct = refl}) = {!!}

  -- insert then
  compose (insert insertedLines {correct}) empty =
    insert insertedLines {correct}

  compose (insert insertedLines {correct}) (modify oldLine newLine) with insertedLines
  ... | [] rewrite sym correct = modify oldLine newLine
  compose (insert insertedLines {correct}) (modify oldLine newLine) | i ∷ is with ∷-injective correct
  ... | refl , refl = insert (newLine ∷ is) {correct = refl}

  compose (insert insertedLines {insertedLinesCorrect}) (delete deletedLines {deletedLinesCorrect}) =
    go insertedLines insertedLinesCorrect deletedLines deletedLinesCorrect
    where
      go :
        ∀{a b c}
        (insertedLines : List Line) →
        (insertedLinesCorrect : b ≡ insertedLines ++ a)
        (deletedLines : List Line) →
        (deletedLinesCorrect : b ≡ deletedLines ++ c) →
        Patch a c
      go [] refl deletedLines refl = delete deletedLines {correct = refl}
      go (i ∷ is) refl [] refl = insert (i ∷ is) {correct = refl}
      go (i ∷ is) refl (d ∷ ds) deletedLinesCorrect with ∷-injective deletedLinesCorrect
      ... | refl , dsCorrect = go is refl ds dsCorrect

  compose (insert insertedLines {correct = refl}) (insert insertedLines' {correct = refl}) =
    insert (insertedLines' ++ insertedLines) {correct = sym (++-assoc insertedLines' insertedLines _)}


  invert : ∀{a b} → Patch a b → Patch b a
  invert (modify oldLine newLine) = modify newLine oldLine
  invert (delete deletedLines {correct}) = insert deletedLines {correct}
  invert (insert insertedLines {correct}) = delete insertedLines {correct}
  invert empty = empty

  invert-involutive : ∀{a b} → (p : Patch a b) → invert (invert p) ≡ p
  invert-involutive (modify oldLine newLine) = refl
  invert-involutive (delete deletedLines {correct = refl}) = refl
  invert-involutive (insert insertedLines {correct = refl}) = refl
  invert-involutive empty = refl

  apply :
    ∀{a b} →
    Patch a b →
    Σ[ a' ∈ List Line ](a ≡ a') →
    Σ[ b' ∈ List Line ](b ≡ b')
  apply empty (lines , refl) = lines , refl
  apply (modify oldLine newLine) (.oldLine ∷ lines , refl) = (newLine ∷ lines) , refl
  apply (delete deletedLines) (lines , prf) = {!!}
  apply (insert insertedLines) (lines , refl) = {!!}
