{-# OPTIONS --erasure #-}
module Codata.Indexed.Stream.IO where

open import Agda.Builtin.IO using (IO)
open import Data.Nat using (ℕ; suc)
open import Data.Unit using (⊤)
open import Relation.Binary.PropositionalEquality as Eq using (_≡_)

open import Codata.Indexed.Stream using (Stream-∞; head-∞; tail-∞)

postulate _*>_ : {A B : Set} → IO A → IO B → IO B
{-# COMPILE GHC _*>_ = \_ _ -> (*>) #-}

{-# NON_TERMINATING #-}
consume-∞ : {A : Set} → (A → IO ⊤) → Stream-∞ A → IO ⊤
consume-∞ f s = 
  f (head-∞ s) *> consume-∞ f (tail-∞ s)

module Example where
  open import Agda.Builtin.IO using (IO)
  open import Data.Nat.Show renaming (show to show-ℕ)
  open import Data.String using (String)

  open import Codata.Indexed.Stream using (to-∞; merge-∞; repeat; map)

  postulate putStrLn : String → IO ⊤
  {-# FOREIGN GHC import qualified Data.Text.IO #-}
  {-# COMPILE GHC putStrLn = Data.Text.IO.putStrLn #-}

  main : IO ⊤
  main =
    consume-∞
      putStrLn
      (merge-∞
        (to-∞ (repeat "yes"))
        (to-∞ (map show-ℕ fib))
      )
    where
      open Codata.Indexed.Stream.Example using (fib)
