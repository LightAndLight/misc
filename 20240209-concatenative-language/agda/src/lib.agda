module lib where

{-
open Type hiding (sum)

open import Data.Char
open import Data.Integer
open import Data.Maybe using (Maybe; just; nothing)
open import Data.Nat
open import Relation.Binary.PropositionalEquality hiding ([_])
open import Data.List using (List; []; _∷_)
open import Data.Product using (_,′_; _×_)
open import Data.String using (String)

data Var : Type → Set where

data Fn : Context → Context → Set where


open import Data.Bool using (Bool; if_then_else_)

mutual
  data Value : Type → Set where
    pair : ∀{a b} → Value a → Value b → Value (product a b)
    closure : ∀{ctx a b} → Values ctx → Term (ctx , a) (◆ , b) → Value (exponent b a)

    inl : ∀{a b} → Value a → Value (Type.sum a b)
    inr : ∀{a b} → Value b → Value (Type.sum a b)

    char : Char → Value char
    bool : Bool → Value bool
    string : String → Value string
    int : ℤ -> Value int

    just : ∀{a} → Value a → Value (maybe a)
    nothing : ∀{a} → Value (maybe a)

    cons : ∀{a} → Value a → Value (list a) → Value (list a)
    nil : ∀{a} → Value (list a)

  data Values : Context → Set where
    ◆ : Values ◆
    _,_ : ∀{ctx ty} → Values ctx → Value ty → Values (ctx , ty)

get-value : ∀{ctx ty} → (ix : ℕ) → {prf : ctx [ ix ] ≡ just ty} → Values ctx → Value ty
get-value zero {prf = refl} (values , x) = x
get-value (ℕ.suc ix) {prf} (values , x) = get-value ix {prf} values

open import Data.Maybe using (_>>=_)

{-
eval : ∀{ctx ctx'} → Term ctx ctx' → Values ctx → Maybe (Values ctx')
eval id values = just values
eval (f ∘ g) values = eval f values >>= eval g
eval ((# ix) {prf}) values = just (◆ , (get-value ix {prf} values))
eval (par f g) values = do
  values' ← eval f values
  ◆ , b ← eval g values
  just (values' , b)
eval (fn term) values = just (values , closure values term)
eval app (values , closure env f , x) = do
  -- fails totality checking
  ◆ , b ← eval f (env , x)
  just (values , b)
eval pair (values , a , b) = just (values , pair a b)
eval unpair (values , pair a b) = just (values , a , b)
eval inl (values , a) = just (values , inl a)
eval inr (values , b) = just (values , inr b)
eval (match-sum left-case right-case) (values , inl a) = eval left-case (values , a)
eval (match-sum left-case right-case) (values , inr b) = eval right-case (values , b)
eval (char c) values = just (values , char c)
eval eq-char (values , char a , char b) = just (values , bool (a Data.Char.== b))
eval (if true-case false-case) (values , bool b) = 
  if b
  then eval true-case values
  else eval false-case values
eval (string s) values = just (values , string s)
eval uncons (values , string s) with (Data.String.uncons s)
... | just (c Data.Product., s') = just (values , just (pair (string s') (char c)))
... | nothing = just (values , nothing)
eval cons-string (values , string s , char c) = just (values , string (Data.String.fromChar c Data.String.++ s))
eval (int i) values = just (values , int i)
eval add (values , int a , int b) = just (values , int (a Data.Integer.+ b))
eval mul (values , int a , int b) = just (values , int (a Data.Integer.- b))
eval nil values = just (values , nil)
eval cons (values , xs , x) = just (values , cons x xs)
eval nothing values = just (values , nothing)
eval just (values , a) = just (values , just a)
eval (bind f) values = {!!}
eval (fix f) values = {!!}
eval drop (values , a) = just values
eval drop-all values = just ◆
eval (match-list nil-case cons-case) (values , cons x xs) = eval cons-case (values , xs , x)
eval (match-list nil-case cons-case) (values , nil) = eval nil-case values
eval (match-maybe nothing-case just-case) (values , just x) = eval just-case (values , x)
eval (match-maybe nothing-case just-case) (values , nothing) = eval nothing-case values
eval (match-char [] default-case) (values , c) = eval default-case values
eval (match-char ((c' Data.Product., f) ∷ cases) default-case) (values , char c) with c' Data.Char.== c
... | Bool.false = eval (match-char cases default-case) (values , char c)
... | Bool.true = eval f values
eval undefined values = nothing
-}

data Location : Type → Set where

data Locations : Context → Set where
  ◆ : Locations ◆
  _,_ : ∀{ctx a} → Locations ctx → Location a → Locations (ctx , a)

open import Data.Product using (_,′_; Σ-syntax; proj₁; proj₂) renaming (_,_ to sigma)
open import Data.List using (_++_)

{- Find the location of a value, given the context.

e.g.

get-location 0 (ctx , int) ≡ "%rax"
get-location 1 (ctx , int , int) ≡ "%rbx"
-}
get-location : ∀{a} → (ix : ℕ) → (ctx : Context) → (prf : ctx [ ix ] ≡ just a) → String
get-location = {!!}

compile : ∀{ctx ctx'} → Term ctx ctx' → Σ[ ctx-val ∈ Context ](ctx-val ≡ ctx) → (Σ[ ctx'-val ∈ Context ](ctx'-val ≡ ctx')) × List String
compile id (sigma ctx refl) = sigma ctx refl ,′ []
compile (f ∘ g) ctx =
  let res = compile f ctx in
  let res' = compile g (proj₁ res) in
  proj₁ res' ,′ (proj₂ res ++ proj₂ res')
compile (# ix) (sigma ctx refl) = 
  sigma {!◆ , ctx [ ix ]!} {!!} ,′
  ("mov {!!} %rax" ∷ [])
compile (par term term₁) = {!!}
compile (fn term) = {!!}
compile app = {!!}
compile pair = {!!}
compile unpair = {!!}
compile inl = {!!}
compile inr = {!!}
compile (match-sum term term₁) = {!!}
compile (char x) = {!!}
compile eq-char = {!!}
compile (if term term₁) = {!!}
compile (string x) = {!!}
compile uncons = {!!}
compile cons-string = {!!}
compile (int x) = {!!}
compile add = {!!}
compile mul = {!!}
compile nil = {!!}
compile cons = {!!}
compile nothing = {!!}
compile just = {!!}
compile (bind x) = {!!}
compile (fix x) = {!!}
compile drop = {!!}
compile drop-all = {!!}
compile (match-list term term₁) = {!!}
compile (match-maybe term term₁) = {!!}
compile (match-char x term) = {!!}
compile undefined = {!!}
-}

open import Relation.Binary.PropositionalEquality using (refl)

open import cat
open import type hiding (sum; int; char)
open import Data.Integer using (+_)
open import Data.Product using (_,′_)
open import Data.List using ([]; _∷_)

map-sum :
  ∀{Term : Context → Context → Set} {{_ : Cat Term}} →
  ∀{a a' b b' ctx} →
  Term (◆ , a) (◆ , a') →
  Term (◆ , b) (◆ , b') →
  Term (ctx , type.sum a b) (ctx , type.sum a' b')
map-sum f g =
  match-sum
    (par drop ((# 0) {prf = refl} ∘ f ∘ inl))
    (par drop ((# 0) {prf = refl} ∘ g ∘ inr))

map-product :
  ∀{Term : Context → Context → Set} {{_ : Cat Term}} →
  ∀{a a' b b' ctx} →
  Term (◆ , a) (◆ , a') →
  Term (◆ , b) (◆ , b') →
  Term (ctx , product a b) (ctx , product a' b')
map-product f g = 
  unpair ∘
  par
    (par
      (drop ∘ drop)
      ((# 1) {prf = refl} ∘ f)
    )
    ((# 0) {prf = refl} ∘ g) ∘
  pair

split :
  ∀{Term : Context → Context → Set} {{_ : Cat Term}} →
  ∀{ctx} →
  Term (ctx , type.string , type.char) (ctx , type.sum type.string (product type.string type.string))
split = 
  fix λ self →
  bind λ c →
  bind λ str →
  str ∘ uncons ∘ match-maybe
    (str ∘ inl)
    (
      unpair ∘
      bind λ c' →
      c ∘ c' ∘ eq-char ∘ if
        (cat.string "" ∘ pair ∘ inr)
        (c ∘ self ∘ map-sum (c' ∘ cons-string) (map-product id (c' ∘ cons-string)))
    )

splits :
  ∀{Term : Context → Context → Set} {{_ : Cat Term}} →
  ∀{ctx} →
  Term (ctx , type.string , type.char) (ctx , list type.string)
splits =
  fix λ self →
  bind λ c →
  c ∘ split ∘
  match-sum
    (bind λ str → nil ∘ str ∘ cons)
    (unpair ∘ par (drop ∘ c ∘ self) ((# 0) {prf = refl}) ∘ cons)

foldr :
  ∀{Term : Context → Context → Set} {{_ : Cat Term}} →
  ∀{ctx a b} →
  Term (ctx , list a , exponent b (product b a) , b) (ctx , b)
foldr =
  fix λ self →
  bind λ z →
  bind λ f →
  match-list
    z
    (
      bind λ a →
      f ∘ z ∘ self ∘
      (bind λ b → f ∘ b) ∘ a ∘ pair ∘ app
    )

map :
  ∀{Term : Context → Context → Set} {{_ : Cat Term}} →
  ∀{ctx a b} →
  Term (ctx , list a , exponent b a) (ctx , list b)
map =
  bind λ f →
  fn ((# 0) {prf = refl} ∘ unpair ∘ (bind λ a → f ∘ a) ∘ app ∘ cons) ∘
  nil ∘
  foldr

foldl :
  ∀{Term : Context → Context → Set} {{_ : Cat Term}} →
  ∀{ctx a b} →
  Term (ctx , list a , exponent b (product a b) , b) (ctx , b)
foldl = 
  fix λ self →
  bind λ z →
  bind λ f →
  match-list
    z
    (
      (bind λ a → f ∘ a ∘ z) ∘ pair ∘ app ∘
      bind λ z' →
      f ∘ z' ∘ self
    )

sum :
  ∀{Term : Context → Context → Set} {{_ : Cat Term}} →
  ∀{ctx} →
  Term (ctx , list type.int) (ctx , type.int)
sum = 
  fn ((# 0) {prf = refl} ∘ unpair ∘ add) ∘ int (+ 0) ∘ foldl

chars :
  ∀{Term : Context → Context → Set} {{_ : Cat Term}} →
  ∀{ctx} →
  Term (ctx , type.string) (ctx , list type.char)
chars =
  fix λ self →
  uncons ∘ match-maybe
    nil
    (unpair ∘ (bind λ c → self ∘ c) ∘ cons)

decimal-digit :
  ∀{Term : Context → Context → Set} {{_ : Cat Term}} →
  ∀{ctx} →
  Term (ctx , type.char) (ctx , maybe type.int)
decimal-digit =
  match-char
    (
      ('0' ,′ int (+ 0) ∘ just) ∷
      ('1' ,′ int (+ 1) ∘ just) ∷
      ('2' ,′ int (+ 2) ∘ just) ∷
      ('3' ,′ int (+ 3) ∘ just) ∷
      ('4' ,′ int (+ 4) ∘ just) ∷
      ('5' ,′ int (+ 5) ∘ just) ∷
      ('6' ,′ int (+ 6) ∘ just) ∷
      ('7' ,′ int (+ 7) ∘ just) ∷
      ('8' ,′ int (+ 8) ∘ just) ∷
      ('9' ,′ int (+ 9) ∘ just) ∷
      []
   )
   nothing

filter-map :
  ∀{Term : Context → Context → Set} {{_ : Cat Term}} →
  ∀{ctx a b} →
  Term (ctx , list a , exponent (maybe b) a) (ctx , list b)
filter-map =
  fix λ self →
  bind λ f →
  match-list
    nil
    (
      (bind λ a → f ∘ a ∘ app) ∘
      match-maybe
        (f ∘ self)
        (
          par
            (drop ∘ f ∘ self)
            ((# 0) {prf = refl}) ∘
          cons
        )
    )

first :
  ∀{Term : Context → Context → Set} {{_ : Cat Term}} →
  ∀{ctx a} →
  Term (ctx , list a) (ctx , a)
first =
  match-list
    undefined
    (par (drop ∘ drop) ((# 0) {prf = refl}))

last :
  ∀{Term : Context → Context → Set} {{_ : Cat Term}} →
  ∀{ctx a} →
  Term (ctx , list a) (ctx , a)
last =
  fix λ self →
  match-list
    undefined
    (
      bind λ x →
      match-list
        x
        (drop ∘ self)
    )

aoc2024-1 :
  ∀{Term : Context → Context → Set} {{_ : Cat Term}} →
  Term (◆ , type.string) (◆ , type.int)
aoc2024-1 =
  char '\n' ∘ splits ∘
  fn (
    (# 0) {prf = refl} ∘ chars ∘
    fn ((# 0) {prf = refl} ∘ decimal-digit) ∘
    filter-map ∘
    par (first ∘ int (+ 10) ∘ mul) last ∘
    add
  ) ∘
  map ∘ sum
