module type where

data Type : Set where
  product : Type → Type → Type
  sum : Type → Type → Type
  exponent : Type → Type → Type

  int : Type
  char : Type
  string : Type
  bool : Type
  list : Type → Type
  maybe : Type → Type
