{-# OPTIONS --safe --erasure #-}
module Parsing.Expr where

open import Data.Char using (isLower)
open import Data.List as List using ([]; _∷_; [_])
open import Data.Nat using (ℕ)
open import Data.Product using (_,_)
open import Data.String as String using (String)
open import Data.Unit using (⊤; tt)

open import Parsing.Construction 

module Expr where
  data Expr : Set where
    var : String → Expr
    lam : String → Expr → Expr
    app : Expr → Expr → Expr

open Expr using (Expr)

ident : {@0 i : ℕ} → Parser String i
ident = map (λ{ (x , xs) → String.fromList (x ∷ xs) }) (satisfy isLower · (satisfy isLower ⋆))

arrow : {@0 i : ℕ} → Parser ⊤ i
arrow = char '-' *> char '>'

spaces : {@0 i : ℕ} → Parser ⊤ i
spaces = map (λ _ → tt) (char ' ' ⋆)

token : {A : Set} → {@0 i : ℕ} → Parser A i → Parser A i
token p = p <* spaces

expr : {@0 i : ℕ} → Parser Expr i
expr = fix go
  where
    go : {@0 i : ℕ} → Guard (Parser Expr) i → Parser Expr i
    go {i} self = lam ∪ app
      where
        lam : Parser Expr i
        lam =
          map
            (λ{ (name , body) → Expr.lam name body })
            -- TODO: spaces after arrow
            (token (char '\\') *> token ident <* arrow · ' ' ‘·-g self)

        atom : Parser Expr i
        atom =
          map Expr.var (token ident) ∪
          -- TODO: spaces after '('
          ('(' ‘·-g self <* token (char ')'))

        app : Parser Expr i
        app = map (λ{ (x , xs) → List.foldl Expr.app x xs }) (atom · ((char ' ' *> atom) ⋆))

module Tests where
  open import Relation.Binary.PropositionalEquality using (_≡_; refl)

  _ : parse (String.toList "") expr ≡ []
  _ = refl

  _ : parse (String.toList "hello") expr ≡ [ Expr.var "hello" ]
  _ = refl

  _ : parse (String.toList "f x") expr ≡ [ Expr.app (Expr.var "f") (Expr.var "x") ]
  _ = refl

  _ : parse (String.toList "f x y") expr ≡ [ Expr.app (Expr.app (Expr.var "f") (Expr.var "x")) (Expr.var "y") ]
  _ = refl

  _ :
    (let tm = Expr.lam "x" (Expr.app (Expr.var "x") (Expr.var "x"))) →

    parse (String.toList "(\\x -> x x) (\\x -> x x)") expr ≡ [ Expr.app tm tm ]
  _ = refl
