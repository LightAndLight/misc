module cat where

open import type using (Type; exponent; product; bool; maybe; list)

data Context : Set where
  ◆ : Context
  _,_ : Context → Type → Context

infixl 50 _,_

open import Data.Nat using (ℕ; zero; suc)
open import Data.Maybe using (Maybe)

_[_] : Context → ℕ → Maybe Type
◆ [ ix ] = Maybe.nothing
(xs , x) [ zero ] = Maybe.just x
(xs , x) [ suc ix ] = xs [ ix ]

open import Relation.Binary.PropositionalEquality using (_≡_)
open import Data.Char using (Char)
open import Data.String using (String)
open import Data.Integer using (ℤ)
open import Data.List using (List)
open import Data.Product using (_×_)

record Cat (Term : Context → Context → Set) : Set where
  field
    id : ∀{x} → Term x x
    _∘_ : ∀{a b c} → Term a b → Term b c → Term a c

    #_ : ∀{x a} → (ix : ℕ) → ∀{prf : x [ ix ] ≡ Maybe.just a} → Term x (◆ , a)
    par : ∀{ctx a b} → Term ctx a → Term ctx (◆ , b) → Term ctx (a , b)

    fn : ∀{x a b} → Term (x , a) (◆ , b) → Term x (x , exponent b a)
    app : ∀{ctx a b} → Term (ctx , exponent b a , a) (ctx , b)

    pair : ∀{ctx a b} → Term (ctx , a , b) (ctx , product a b)
    unpair : ∀{ctx a b} → Term (ctx , product a b) (ctx , a , b)

    inl : ∀{ctx a b} → Term (ctx , a) (ctx , Type.sum a b)
    inr : ∀{ctx a b} → Term (ctx , b) (ctx , Type.sum a b)
    match-sum :
      ∀{ctx ctx' a b} →
      Term (ctx , a) ctx' →
      Term (ctx , b) ctx' →
      Term (ctx , Type.sum a b) ctx'

    char : ∀{ctx} → Char → Term ctx (ctx , type.char)
    eq-char : ∀{ctx} → Term (ctx , type.char , type.char) (ctx , bool)

    if :
      ∀{ctx ctx'} →
      Term ctx ctx' →
      Term ctx ctx' →
      Term (ctx , bool) ctx'

    string : ∀{ctx} → String → Term ctx (ctx , type.string)
    uncons : ∀{ctx} → Term (ctx , type.string) (ctx , maybe (product type.string type.char))
    cons-string : ∀{ctx} → Term (ctx , type.string , type.char) (ctx , type.string)

    int : ∀{ctx} → ℤ → Term ctx (ctx , type.int)
    add : ∀{ctx} → Term (ctx , type.int , type.int) (ctx , type.int)
    mul : ∀{ctx} → Term (ctx , type.int , type.int) (ctx , type.int)

    nil : ∀{ctx a} → Term ctx (ctx , list a)
    cons : ∀{ctx a} → Term (ctx , list a , a) (ctx , list a)

    nothing : ∀{ctx a} → Term ctx (ctx , maybe a)
    just : ∀{ctx a} → Term (ctx , a) (ctx , maybe a)

    bind : ∀{ctx a b} → ((∀{ctx} → Term ctx (ctx , a)) → Term ctx b) → Term (ctx , a) b

    fix : ∀{ctx a} → (Term ctx a → Term ctx a) → Term ctx a

    drop : ∀{ctx a} → Term (ctx , a) ctx
    drop-all : ∀{ctx} → Term ctx ◆

    match-list :
      ∀{ctx ctx' a} →
      (nil-case : Term ctx ctx') →
      (cons-case : Term (ctx , list a , a) ctx') →
      Term (ctx , list a) ctx'
    match-maybe :
      ∀{ctx ctx' a} →
      (nothing-case : Term ctx ctx') →
      (just-case : Term (ctx , a) ctx') →
      Term (ctx , maybe a) ctx'
    match-char :
      ∀{ctx ctx'} →
      List (Char × Term ctx ctx') →
      Term ctx ctx' →
      Term (ctx , type.char) ctx'

    undefined : ∀{ctx a} → Term ctx a

  infixl 50 _∘_
  infix 60 #_

open Cat {{...}} public

open import Data.List using ([]; _∷_)
open import Data.Product using (_,′_; proj₁; proj₂; Σ-syntax)
open import Relation.Binary.PropositionalEquality using (subst)

data Scope : Context → Set where
  ◆ : Scope ◆
  _,[_∷_] : ∀{ctx} → Scope ctx → String → (a : Type) → Scope (ctx , a)

lookup-scope : ∀{ctx ty} → (ix : ℕ) → ctx [ ix ] ≡ Maybe.just ty → Scope ctx → String × Σ[ a ∈ Type ](a ≡ ty)
lookup-scope zero _≡_.refl (scope ,[ x ∷ a ]) = x ,′ a Data.Product., _≡_.refl
lookup-scope (suc ix) prf (scope ,[ x ∷ a ]) = lookup-scope ix prf scope

Haskell : Context → Context → Set
Haskell ctx ctx' = Scope ctx → Scope ctx' × List String

catHaskell : Cat Haskell
catHaskell = record
  { id = λ scope → scope ,′ []
  ; _∘_ = λ f g scope →
     let res = f scope in
     let res' = g (proj₁ res) in
     proj₁ res' ,′ (proj₂ res Data.List.++ proj₂ res')
  ; #_ = λ {ctx} ix {prf} scope →
    let res = lookup-scope ix prf scope in
    subst
      (λ x → Scope (◆ , x) )
      (proj₂ (proj₂ res))
      (◆ ,[ proj₁ res ∷ proj₁ (proj₂ res) ])
        ,′
    (proj₁ res ∷ [])
  ; par = {!!}
  ; fn = {!!}
  ; app = {!!}
  ; pair = {!!}
  ; unpair = {!!}
  ; inl = {!!}
  ; inr = {!!}
  ; match-sum = {!!}
  ; char = {!!}
  ; eq-char = {!!}
  ; if = {!!}
  ; string = {!!}
  ; uncons = {!!}
  ; cons-string = {!!}
  ; int = {!!}
  ; add = {!!}
  ; mul = {!!}
  ; nil = {!!}
  ; cons = {!!}
  ; nothing = {!!}
  ; just = {!!}
  ; bind = {!!}
  ; fix = {!!}
  ; drop = {!!}
  ; drop-all = {!!}
  ; match-list = {!!}
  ; match-maybe = {!!}
  ; match-char = {!!}
  ; undefined = {!!}
  }

{-
open import Data.List using (List; []; _∷_)
open import Data.String using (_++_)
open import Data.Nat using (ℕ; _+_; _⊔_)
import Data.Nat.Show

Asm : Context → Context → Set
Asm _ _ = List String

ptr-size : ℕ
ptr-size = 8

size-of : Type → ℕ
size-of (product a b) = size-of a + size-of b
size-of (Type.sum a b) = 8 + (size-of a ⊔ size-of b)
size-of (exponent a b) = ptr-size
size-of Type.int = 8
size-of Type.char = 8
size-of Type.string = ptr-size
size-of bool = 8
size-of (list ty) = 8 + size-of ty + ptr-size
size-of (maybe ty) = 8 + size-of ty

get-stack-offset : ∀{ty} → (ctx : Context) → (ix : ℕ) → ctx [ ix ] ≡ Maybe.just ty → ℕ
get-stack-offset (ctx , x) zero prf = size-of x
get-stack-offset (ctx , x) (suc ix) prf = size-of x + get-stack-offset ctx ix prf

catAsm : Cat Asm
catAsm = record
  { id = []
  ; _∘_ = Data.List._++_
  ; #_ = 
    λ {ctx} ix {prf} →
    ("mov %rax [%rsp - " ++ Data.Nat.Show.show (get-stack-offset ctx ix prf) ++ "]") ∷
    []
  ; par = {!!}
  ; fn = {!!}
  ; app = {!!}
  ; pair = {!!}
  ; unpair = {!!}
  ; inl = {!!}
  ; inr = {!!}
  ; match-sum = {!!}
  ; char = {!!}
  ; eq-char = {!!}
  ; if = {!!}
  ; string = {!!}
  ; uncons = {!!}
  ; cons-string = {!!}
  ; int = {!!}
  ; add = {!!}
  ; mul = {!!}
  ; nil = {!!}
  ; cons = {!!}
  ; nothing = {!!}
  ; just = {!!}
  ; bind = {!!}
  ; fix = {!!}
  ; drop = {!!}
  ; drop-all = {!!}
  ; match-list = {!!}
  ; match-maybe = {!!}
  ; match-char = {!!}
  ; undefined = {!!}
  }
-}
