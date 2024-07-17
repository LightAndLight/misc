{-# OPTIONS --erasure --guardedness #-}
module Main where

open import Data.Bool.Show renaming (show to showᵇ)
import Data.String as String
open import IO

open import Parser using (parse; expr)

main : Main
main = run (do
  putStrLn (showᵇ (parse (String.toList "(\\x -> x x) (\\x -> x x)") expr))
  putStrLn (showᵇ (parse (String.toList "(\\x -> x x) \\x -> x x)") expr))
  )
