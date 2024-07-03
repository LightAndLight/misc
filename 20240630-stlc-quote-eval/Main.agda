module Main where

module One where
  open import Data.List using (List; _∷_; [])

  data Ty : Set where
    Bool : Ty
    Nat : Ty
    Fun : Ty → Ty → Ty
    Code : Ty → Ty

  _⇒_ : Ty → Ty → Ty
  _⇒_ = Fun

  infixr 5 _⇒_

  data CtxEntry : Set where
    lock : CtxEntry
    type : Ty → CtxEntry

  Ctx : Set
  Ctx = List CtxEntry

  unlock : Ctx → Ctx
  unlock [] = []
  unlock (lock ∷ rest) = rest
  unlock (type a ∷ rest) = unlock rest

  data Index : Ctx → Ty → Set where
    here : ∀{a : Ty} {rest : Ctx} → Index (type a ∷ rest) a
    there : ∀{a a' : Ty} {rest : Ctx} → Index rest a → Index (type a' ∷ rest) a

  data Expr (ctx : Ctx) : Ty → Set where
    var : ∀{a : Ty} → Index ctx a → Expr ctx a
    lam : ∀{a b : Ty} → Expr (type a ∷ ctx) b → Expr ctx (Fun a b)
    app : ∀{a b : Ty} → Expr ctx (Fun a b) → Expr ctx a → Expr ctx b

    true : Expr ctx Bool
    false : Expr ctx Bool

    zero : Expr ctx Nat
    suc : Expr ctx Nat → Expr ctx Nat

    delay : ∀{a : Ty} → Expr (lock ∷ ctx) a → Expr ctx (Code a)
    splice : ∀{a : Ty} → Expr (unlock ctx) (Code a) → Expr ctx a
    quot : ∀{a : Ty} → Expr ctx a → Expr ctx (Code a)
    eval : ∀{a : Ty} → Expr ctx (Code a) → Expr ctx a

  example1 : ∀{a b : Ty} → Expr [] (Code (a ⇒ b)) → Expr [] (Code a) → Expr [] (Code b)
  example1 f x = delay (app (splice f) (splice x))

  example2 : ∀{a b : Ty} → Expr [] (Code (a ⇒ b) ⇒ Code a ⇒ Code b)
  example2 = lam (lam (delay (app (splice (var (there here))) (splice (var here)))))

module Two where
  open import Data.List using (List; _∷_; [])

  data Ty : Set where
    Bool : Ty
    Nat : Ty
    Fun : Ty → Ty → Ty
    Code : Ty → Ty
    Box : Ty → Ty

  _⇒_ : Ty → Ty → Ty
  _⇒_ = Fun

  infixr 5 _⇒_

  data CtxEntry : Set where
    lock : CtxEntry
    type : Ty → CtxEntry

  Ctx : Set
  Ctx = List CtxEntry

  unlock : Ctx → Ctx
  unlock [] = []
  unlock (lock ∷ rest) = rest
  unlock (type a ∷ rest) = unlock rest

  data Index : Ctx → Ty → Set where
    here : ∀{a : Ty} {rest : Ctx} → Index (type a ∷ rest) a
    there : ∀{a a' : Ty} {rest : Ctx} → Index rest a → Index (type a' ∷ rest) a

  data Expr (ctx : Ctx) : Ty → Set where
    var : ∀{a : Ty} → Index ctx a → Expr ctx a
    lam : ∀{a b : Ty} → Expr (type a ∷ ctx) b → Expr ctx (Fun a b)
    app : ∀{a b : Ty} → Expr ctx (Fun a b) → Expr ctx a → Expr ctx b

    true : Expr ctx Bool
    false : Expr ctx Bool

    zero : Expr ctx Nat
    suc : Expr ctx Nat → Expr ctx Nat

    delay : ∀{a : Ty} → Expr (lock ∷ ctx) a → Expr ctx (Code a)
    splice : ∀{a : Ty} → Expr (unlock ctx) (Code a) → Expr ctx a
    quot : ∀{a : Ty} → Expr ctx a → Expr ctx (Code a)
    eval : ∀{a : Ty} → Expr ctx (Code a) → Expr ctx a

    box : ∀{a : Ty} → Expr ctx (Code a) → Expr ctx a → Expr ctx (Box a)
    code : ∀{a : Ty} → Expr ctx (Box a) → Expr ctx (Code a)
    value : ∀{a : Ty} → Expr ctx (Box a) → Expr ctx a

module Translation where
  open import Data.List using (List; _∷_; [])

  transTy' : One.Ty → Two.Ty
  transTy' One.Bool = Two.Bool
  transTy' One.Nat = Two.Nat
  transTy' (One.Fun a b) = Two.Fun (Two.Box (transTy' a)) (Two.Box (transTy' b))
  transTy' (One.Code a) = Two.Code (transTy' a)

  transTy : One.Ty → Two.Ty
  transTy a = Two.Box (transTy' a)

  transCtx : One.Ctx → Two.Ctx
  transCtx [] = []
  transCtx (One.lock ∷ rest) = Two.lock ∷ transCtx rest
  transCtx (One.type x ∷ rest) = Two.type (transTy x) ∷ transCtx rest

  transIndex : ∀{a : One.Ty} {ctx : One.Ctx} → One.Index ctx a → Two.Index (transCtx ctx) (transTy a)
  transIndex One.here = Two.here
  transIndex (One.there rest) = Two.there (transIndex rest)

  open import Relation.Binary.PropositionalEquality using (_≡_)

  transUnlock : ∀{ctx : One.Ctx} → transCtx (One.unlock ctx) ≡ Two.unlock (transCtx ctx)
  transUnlock {[]} = _≡_.refl
  transUnlock {One.lock ∷ rest} = _≡_.refl
  transUnlock {One.type x ∷ rest} = transUnlock {rest}

  transExpr : ∀{a : One.Ty} {ctx : One.Ctx} → One.Expr ctx a → Two.Expr (transCtx ctx) (transTy a)
  transExpr (One.var x) = Two.var (transIndex x)
  transExpr (One.lam body) = 
    let body' = transExpr body in
    let body'value = Two.value body' in
    Two.box (Two.delay (Two.lam {!Two.code body'!})) (Two.lam {!body'value!})
  transExpr (One.app f x) = 
    let f' = transExpr f in
    let x' = transExpr x in
    Two.box
      -- Code(Box a → Box b)
      -- Code a
      (Two.delay (Two.app {!!} {!!}))
      (Two.value (Two.app (Two.value f') x'))
  transExpr One.true = Two.box (Two.delay Two.true) Two.true
  transExpr One.false = Two.box (Two.delay Two.false) Two.false
  transExpr One.zero = Two.box (Two.delay Two.zero) Two.zero
  transExpr (One.suc expr) =
    let expr' = transExpr expr in
    Two.box (Two.delay (Two.suc (Two.splice (Two.code expr')))) (Two.suc (Two.value expr'))
  transExpr (One.delay expr) =
    let expr' = transExpr expr in
    Two.box (Two.delay (Two.delay (Two.splice (Two.code expr')))) (Two.delay (Two.value expr'))
  transExpr {a} {ctx} (One.splice expr) =
    let expr' = transExpr expr in
    go expr'
    where
      go : Two.Expr (transCtx (One.unlock ctx)) (transTy (One.Code a)) → Two.Expr (transCtx ctx) (transTy a)
      go expr' rewrite transUnlock {ctx} = 
        Two.box (Two.delay (Two.splice (Two.splice (Two.code expr')))) (Two.splice (Two.value expr'))

  transExpr (One.quot expr) = 
    let expr' = transExpr expr in
    Two.box (Two.delay (Two.quot (Two.splice (Two.code expr')))) (Two.code expr')
  transExpr (One.eval expr) = 
    let expr' = transExpr expr in 
    Two.box (Two.delay (Two.eval (Two.splice (Two.code expr')))) (Two.eval (Two.value expr'))
