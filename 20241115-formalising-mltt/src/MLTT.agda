{-# OPTIONS --safe --without-K #-}

module MLTT where

open import Function using (case_of_; _∘_)
open import Data.Empty using (⊥-elim)
open import Data.Nat using (ℕ; zero; suc; pred)
open import Data.Nat.Properties using (suc-injective)
open import Data.Product using (Σ-syntax; _×_; proj₂) renaming (_,_ to _,,_)
open import Data.Sum using (_⊎_; inj₁; inj₂)
open import Relation.Binary.PropositionalEquality as Eq using (_≡_; refl; subst; sym; cong; cong₂; trans)
open Eq.≡-Reasoning
open import Relation.Nullary using (¬_; Dec; yes; no)

open import Rename
open import Rename.Properties
open import Substitute
open import Substitute.Properties
open import Syntax
open import Syntax.Properties
open import Presyntax
open import Presyntax.Properties
open import Presyntax.Properties.Rename
open import Presyntax.Properties.Substitute

⊢-lookup :
  {Γ : Ctx} {A : Term} →
  Γ ∋ A →
  ⊢ Γ ctx →
  Γ ⊢ A type
⊢-lookup (here {B} {B'} r) (⊢-, ctx B'type) =
  subst (λ x → B , B' ⊢ x type) r (⊢-type-rename Renaming-weaken1 B'type)
⊢-lookup (there {X} {X'} {Y} r ix) (⊢-, {A = B} ctx _) =
  subst
    (λ x → X , B ⊢ x type)
    r
    (⊢-type-rename Renaming-weaken1 (⊢-lookup ix ctx))

-- Type reconstruction: valid terms have valid types.
⊢-typeOf :
  {Γ : Ctx} {a A : Term} →
  ⊢ Γ ctx →
  Γ ⊢ a ∶ A →
  Γ ⊢ A type
⊢-typeOf ctx (⊢-var ix) = ⊢-lookup ix ctx
⊢-typeOf ctx ⊢-Void = ⊢-Type
⊢-typeOf ctx ⊢-Unit = ⊢-Type
⊢-typeOf ctx (⊢-Pi _ _) = ⊢-Type
⊢-typeOf ctx (⊢-Sigma _ _) = ⊢-Type
⊢-typeOf ctx (⊢-lam A e) = ⊢-El (⊢-Pi A (⊢-typeOf (⊢-, ctx A) e))
⊢-typeOf {Γ} ctx (⊢-app {A} {B} {B'} s f∶PiAB x∶A) with ⊢-typeOf ctx f∶PiAB
... | ⊢-El (⊢-Pi Atype Btype) =
  subst (λ x → Γ ⊢ x type) s (⊢-type-substitute (Substitution-inst1 x∶A) Btype)

{-
consistent : {x : Term} → ¬(◆ ⊢ x ∶ Void)
consistent (⊢-app prf f x) = {!!}
-}

rename-underₙ-f-≡-substitute-sunderₙ-inst1 :
  {r : Term} {f : ℕ → ℕ} {ctx : Ctx} →
  (T B : Term) →
  rename (underₙ ctx f) T ≡ substitute (sunderₙ ctx (inst1 (rename f r))) B →
  Σ[ B' ∈ Term ] (rename (under (underₙ ctx f)) B' ≡ B)
rename-underₙ-f-≡-substitute-sunderₙ-inst1 (var zero) (var zero) prf =
  var zero ,, refl
rename-underₙ-f-≡-substitute-sunderₙ-inst1 {r} {f} {◆} (var zero) (var (suc ix')) prf =
  var (suc zero) ,, cong var (cong suc (var-injective prf))
rename-underₙ-f-≡-substitute-sunderₙ-inst1 {r} {f} {ctx , x} (var zero) (var (suc ix')) prf =
  let
    prf' =
      begin
        var zero
      ≡⟨⟩
        var (under (underₙ ctx f) zero)
      ≡⟨ cong var (sym (underₙ-under-comm ctx zero)) ⟩
        var (underₙ ctx (under f) zero)
      ≡⟨ prf ⟩
        sunderₙ ctx (sunder (inst1 (rename f r))) (suc ix')
      ≡⟨ sunderₙ-sunder-comm ctx ⟩
        sunder (sunderₙ ctx (inst1 (rename f r))) (suc ix')
      ≡⟨⟩
        rename suc (sunderₙ ctx (inst1 (rename f r)) ix')
      ∎
  in
    ⊥-elim (rename-suc-≡-zero-⊥ _ (sym prf'))
rename-underₙ-f-≡-substitute-sunderₙ-inst1 (var (suc ix)) (var zero) prf =
  var zero ,, refl
rename-underₙ-f-≡-substitute-sunderₙ-inst1 {ctx = ◆} (var (suc ix)) (var (suc ix')) prf =
  var (suc (suc ix)) ,, cong var (cong suc (var-injective prf))
rename-underₙ-f-≡-substitute-sunderₙ-inst1 {r} {f} {ctx = ctx , x} (var (suc ix)) (var (suc ix')) prf =
  let
    prf' =
      begin
        rename suc (var (underₙ ctx f ix))
      ≡⟨⟩
        var (suc (underₙ ctx f ix))
      ≡⟨⟩
        var (under (underₙ ctx f) (suc ix))
      ≡⟨ cong var (sym (underₙ-under-comm ctx (suc ix))) ⟩
        var (underₙ ctx (under f) (suc ix))
      ≡⟨ prf ⟩
        sunderₙ ctx (sunder (inst1 (rename f r))) (suc ix')
      ≡⟨ sunderₙ-sunder-comm ctx ⟩
        sunder (sunderₙ ctx (inst1 (rename f r))) (suc ix')
      ≡⟨⟩
        rename suc (sunderₙ ctx (inst1 (rename f r)) ix')
      ∎

    prf'' : var (underₙ ctx f ix) ≡ sunderₙ ctx (inst1 (rename f r)) ix'
    prf'' = rename-injective suc-injective _ _ prf'
  in
    case sunderₙ-var ctx (sym prf'') of λ{
      (inj₁ (ix'<length-ctx ,, refl)) → {!!}
      ;
      (inj₂ (inj₁ (suc-ix'>length-ctx ,, prf'''))) → {!!}
      ;
      (inj₂ (inj₂ (ix'>length-ctx ,, prf'''))) → {!!}
    }
rename-underₙ-f-≡-substitute-sunderₙ-inst1 {r} {f} {ctx} Type (var ix) prf = 
  let
    ≡Type ,, ix≡length-ctx = sunderₙ-Type {x = rename f r} {ix = ix} {ctx = ctx} (sym prf)
    r≡Type = rename-Type _ ≡Type
  in
    var (length ctx) ,, cong var (trans (under-underₙ-ctx-length-ctx ctx) (sym ix≡length-ctx))
rename-underₙ-f-≡-substitute-sunderₙ-inst1 Type Type refl = Type ,, refl
rename-underₙ-f-≡-substitute-sunderₙ-inst1 Void B prf = {!!}
rename-underₙ-f-≡-substitute-sunderₙ-inst1 Unit B prf = {!!}
rename-underₙ-f-≡-substitute-sunderₙ-inst1 (Pi T T₁) B prf = {!!}
rename-underₙ-f-≡-substitute-sunderₙ-inst1 (Sigma T T₁) B prf = {!!}
rename-underₙ-f-≡-substitute-sunderₙ-inst1 (lam T T₁) B prf = {!!}
rename-underₙ-f-≡-substitute-sunderₙ-inst1 (app T T₁) B prf = {!!}

rename-under-f-≡-substitute-sunder-inst1 :
  {r : Term} {f : ℕ → ℕ} →
  (let g = under) (let h = sunder) →
  ((T B : Term) → rename ((under ∘ g) f) T ≡ substitute ((sunder ∘ h) (inst1 (rename f r))) B → Σ[ B' ∈ Term ] (rename (under ((under ∘ g) f)) B' ≡ B)) →
  (T B : Term) → rename (g f) T ≡ substitute (h (inst1 (rename f r))) B → Σ[ B' ∈ Term ] (rename (under (g f)) B' ≡ B)
rename-under-f-≡-substitute-sunder-inst1 rec (var zero) (var zero) refl = 
  var zero ,, refl
rename-under-f-≡-substitute-sunder-inst1 {r = r} {f = f} rec (var zero) (var (suc ix')) prf =
  let
    prf =
      begin
        var zero
      ≡⟨ prf ⟩
        rename suc (inst1 (rename f r) ix')
      ≡⟨ rename∘inst1 {x = rename f r} _ ⟩
        inst1 (rename suc (rename f r)) (under suc ix')
      ≡⟨ cong (λ x → inst1 x (under suc ix')) (rename-∘ r) ⟩
        inst1 (rename (suc ∘ f) r) (under suc ix')
      ∎

    prf' : (ix' ≡ zero × rename (suc ∘ f) r ≡ var zero) ⊎ Σ[ ix'' ∈ ℕ ] (ix' ≡ suc ix'' × (suc ix'' ≡ zero))
    prf' = inst1-under-var {f = suc} {x = rename (suc ∘ f) r} (sym prf)
  in
    case prf' of λ{
      (inj₁ (ix'≡zero ,, rename-r≡var-zero)) → 
        var (suc zero) ,, cong var (cong suc (sym ix'≡zero))
      ;
      (inj₂ (_ ,, _ ,, prf'')) →
        case prf'' of λ()
    }
rename-under-f-≡-substitute-sunder-inst1 {r = r} {f = f} rec (var (suc ix)) (var (suc ix')) prf =
  let
    prf =
      begin
        var (suc (f ix))
      ≡⟨ prf ⟩
        rename suc (inst1 (rename f r) ix')
      ≡⟨ rename∘inst1 {x = rename f r} _ ⟩
        inst1 (rename suc (rename f r)) (under suc ix')
      ≡⟨ cong (λ x → inst1 x (under suc ix')) (rename-∘ r) ⟩
        inst1 (rename (suc ∘ f) r) (under suc ix')
      ∎

    prf' : (ix' ≡ zero × rename (suc ∘ f) r ≡ var (suc (f ix))) ⊎ Σ[ ix'' ∈ ℕ ] (ix' ≡ suc ix'' × (suc ix'' ≡ (suc (f ix))))
    prf' = inst1-under-var {f = suc} {x = rename (suc ∘ f) r} (sym prf)
  in
    case prf' of λ{
      (inj₁ (ix'≡zero ,, rename-r)) →
        var (suc zero) ,, cong var (cong suc (sym ix'≡zero))
      ;
      (inj₂ (ix'' ,, ix'≡suc-ix'' ,, suc-ix''-≡-suc-f-ix)) →
        let
          ix''-≡-f-ix : ix'' ≡ f ix
          ix''-≡-f-ix = suc-injective suc-ix''-≡-suc-f-ix

          prf'' : suc (f ix) ≡ ix'
          prf'' = trans (cong suc (sym ix''-≡-f-ix)) (sym ix'≡suc-ix'')
        in
          var (suc (suc ix))
          ,,
          cong var (cong suc prf'')
    }
rename-under-f-≡-substitute-sunder-inst1 {r = r} {f = f} rec Type (var ix) prf =
  let
    _ ,, ix≡1 = sunder-Type {x = rename f r} {ix = ix} (sym prf)
  in
    var (suc zero) ,, cong var (sym ix≡1)
rename-under-f-≡-substitute-sunder-inst1 rec Type Type refl = Type ,, refl
rename-under-f-≡-substitute-sunder-inst1 rec Void B prf = {!!}
rename-under-f-≡-substitute-sunder-inst1 rec Unit B prf = {!!}
rename-under-f-≡-substitute-sunder-inst1 {r = r} {f = f} rec (Pi A B) (var ix) prf =
  let _ ,, _ ,, _ ,, ix≡1 = sunder-Pi {x = rename f r} {ix = ix} (sym prf) in
  var (suc zero) ,, cong var (sym ix≡1)
rename-under-f-≡-substitute-sunder-inst1 rec (Pi A B) (Pi A' B') prf =
  let
    rename-A = Pi-injective₁ prf
    A'' ,, rename-A'' = rename-under-f-≡-substitute-sunder-inst1 rec A A' rename-A

    rename-B = Pi-injective₂ prf
    B'' ,, rename-B'' = rec B B' rename-B
  in
    Pi A'' B'' ,, cong₂ Pi rename-A'' rename-B''
rename-under-f-≡-substitute-sunder-inst1 rec (Sigma T T₁) B prf = {!!}
rename-under-f-≡-substitute-sunder-inst1 rec (lam T T₁) B prf = {!!}
rename-under-f-≡-substitute-sunder-inst1 {r = r} {f = f} rec (app a b) (var ix) prf =
  let _ ,, _ ,, _ ,, ix≡1 = sunder-app {x = rename f r} {ix = ix} (sym prf) in
  var (suc zero) ,, cong var (sym ix≡1)
rename-under-f-≡-substitute-sunder-inst1 rec (app a b) (app a' b') prf =
  let
    rename-a = app-injective₁ prf
    a'' ,, rename-a'' = rename-under-f-≡-substitute-sunder-inst1 rec a a' rename-a

    rename-b = app-injective₂ prf
    b'' ,, rename-b'' = rename-under-f-≡-substitute-sunder-inst1 rec b b' rename-b
  in
    app a'' b'' ,, cong₂ app rename-a'' rename-b''

rename-f-≡-substitute-inst1 :
  {Γ' : Ctx} {r : Term} {f : ℕ → ℕ} →
  (let g x = x) (let h x = x) →
  ((T B : Term) → rename ((under ∘ g) f) T ≡ substitute ((sunder ∘ h) (inst1 (rename f r))) B → Σ[ B' ∈ Term ] (rename (under ((under ∘ g) f)) B' ≡ B)) →
  (T B : Term) → rename (g f) T ≡ substitute (h (inst1 (rename f r))) B → Σ[ B' ∈ Term ] (rename (under (g f)) B' ≡ B)
rename-f-≡-substitute-inst1 {Γ' = Γ'} rec T (var zero) prf-subst =
  var zero ,, refl
rename-f-≡-substitute-inst1 rec (var ix) (var (suc ix')) prf-subst =
  var (suc ix) ,, cong var (cong suc (var-injective prf-subst))
rename-f-≡-substitute-inst1 rec Type Type refl = 
  Type ,, refl
rename-f-≡-substitute-inst1 rec Void Void refl =
  Void ,, refl
rename-f-≡-substitute-inst1 rec Unit Unit refl =
  Unit ,, refl
rename-f-≡-substitute-inst1 {Γ'} {r} {f} rec (Pi A B) (Pi A' B') prf-subst = 
  let
    A'' ,, rename-A'' = rename-f-≡-substitute-inst1 {Γ'} rec A A' (Pi-injective₁ prf-subst)
    B'' ,, rename-B'' = rec B B' (Pi-injective₂ prf-subst)
  in
    Pi A'' B'' ,, cong₂ Pi rename-A'' rename-B''
rename-f-≡-substitute-inst1 {Γ'} {r} {f} rec (Sigma A B) (Sigma A' B') prf-subst =
  let
    A'' ,, rename-A'' = rename-f-≡-substitute-inst1 {Γ'} rec A A' (Sigma-injective₁ prf-subst)

    B'' ,, rename-B'' = rec B B' (Sigma-injective₂ prf-subst)
  in
    Sigma A'' B'' ,, cong₂ Sigma rename-A'' rename-B''
rename-f-≡-substitute-inst1 rec (lam A e) (lam A' e') prf-subst = {!!}
rename-f-≡-substitute-inst1 {Γ'} {r} {f} rec (app a b) (app a' b') prf-subst =
  let
    a'' ,, rename-a'' = rename-f-≡-substitute-inst1 {Γ'} rec a a' (app-injective₁ prf-subst)
    b'' ,, rename-b'' = rename-f-≡-substitute-inst1 {Γ'} rec b b' (app-injective₂ prf-subst)
  in
    app a'' b'' ,, cong₂ app rename-a'' rename-b''

∋-strengthen :
  {Γ : Ctx} {X a A A' : Term} →
  (Γ' : Ctx) →
  (ix : (Γ , X ++ rename-ctx suc Γ') ∋ A') →
  rename (underₙ Γ' suc) a ≡ var (∋-to-ℕ ix) →
  rename (underₙ Γ' suc) A ≡ A' →
  Σ[ ix' ∈ (Γ ++ Γ' ∋ A) ] (a ≡ var (∋-to-ℕ ix'))
∋-strengthen ◆ (here refl) rename-a rename-A = 
  ⊥-elim (rename-suc-≡-zero-⊥ _ rename-a)
∋-strengthen {Γ} ◆ (there refl ix) rename-a rename-A =
  subst (λ x → Γ ∋ x) (rename-injective suc-injective _ _ (sym rename-A)) ix
  ,,
  trans
    (rename-suc-var _ rename-a)
    (cong var (sym (∋-to-ℕ-≡ (rename-injective suc-injective _ _ (sym rename-A)) ix)))
∋-strengthen {a = a} {A = A} (ctx , x) (here refl) rename-a rename-A = 
  here
    (rename-injective
      (under-injective (underₙ-injective suc-injective ctx))
      _
      _
      (sym (
        begin
          rename (under (underₙ ctx suc)) A
        ≡⟨ rename-cong (λ ix → sym (underₙ-under-comm ctx ix)) A ⟩
          rename (underₙ ctx (under suc)) A
        ≡⟨ rename-A ⟩
          rename suc (rename (underₙ ctx suc) x)
        ≡⟨ rename-∘ x ⟩
          rename (suc ∘ underₙ ctx suc) x
        ≡⟨⟩
          rename (under (underₙ ctx suc) ∘ suc) x
        ≡⟨ sym (rename-∘ x) ⟩
          rename (under (underₙ ctx suc)) (rename suc x)
        ∎)))
  ,,
  rename-under-a-≡-zero _ (trans (rename-cong (λ ix → sym (underₙ-under-comm ctx ix)) a) rename-a)
∋-strengthen {Γ} {X} {a = a} {A = A} {A' = A'} (ctx , x) (there {A = Y} refl ix) rename-a rename-A =
  let
    rename-a' : rename (under (underₙ ctx suc)) a ≡ var (suc (∋-to-ℕ ix))
    rename-a' =
      begin
        rename (under (underₙ ctx suc)) a
      ≡⟨ rename-cong (λ ix → sym (underₙ-under-comm ctx ix)) _ ⟩
        rename (underₙ ctx (under suc)) a
      ≡⟨ rename-a ⟩
        var (suc (∋-to-ℕ ix))
      ∎

    rename-a-pred : rename (underₙ ctx suc) (rename pred a) ≡ var (∋-to-ℕ ix)
    rename-a-pred =
      begin
        rename (underₙ ctx suc) (rename pred a)
      ≡⟨ rename-∘ a ⟩
        rename (underₙ ctx suc ∘ pred) a
      ≡⟨ rename-under-f-≡-var-suc _ _ rename-a' ⟩
        var (∋-to-ℕ ix)
      ∎

    rename-A' : rename (under (underₙ ctx suc)) A ≡ rename suc Y
    rename-A' =
      begin
        rename (under (underₙ ctx suc)) A
      ≡⟨ rename-cong (λ ix → sym (underₙ-under-comm ctx ix)) A ⟩
        rename (underₙ ctx (under suc)) A
      ≡⟨ rename-A ⟩
        rename suc Y
      ∎

    rename-A-pred : rename (underₙ ctx suc) (rename pred A) ≡ Y
    rename-A-pred =
      begin
        rename (underₙ ctx suc) (rename pred A)
      ≡⟨ rename-∘ A ⟩
        rename (underₙ ctx suc ∘ pred) A
      ≡⟨ rename-under-f-≡-rename-sucᵣ _ _ rename-A' ⟩
        Y
      ∎

    ix-prf' : Σ[ ix' ∈ ((Γ ++ ctx) ∋ rename pred A) ] (rename pred a ≡ var (∋-to-ℕ ix'))
    ix-prf' = ∋-strengthen ctx ix rename-a-pred rename-A-pred

    xx : rename (suc ∘ pred) a ≡ a
    xx = rename-under-f-≡-rename-sucₗ a (var (∋-to-ℕ ix)) (trans (rename-cong (λ ix → sym (underₙ-under-comm ctx ix)) a) rename-a)

    ix-prf'' : Σ[ ix' ∈ ((Γ ++ ctx , x) ∋ rename suc (rename pred A)) ] (a ≡ var (∋-to-ℕ ix'))
    ix-prf'' =
      let ix' ,, prf' = ix-prf' in
      there refl ix'
      ,,
      (
      begin
        a
      ≡⟨ sym xx ⟩
        rename (suc ∘ pred) a
      ≡⟨ sym (rename-∘ a) ⟩
        rename suc (rename pred a)
      ≡⟨ cong (rename suc) prf' ⟩
        var (suc (∋-to-ℕ ix'))
      ∎)

    ix-prf''' : Σ[ ix' ∈ ((Γ ++ ctx , x) ∋ A) ] (a ≡ var (∋-to-ℕ ix'))
    ix-prf''' =
      let ix'' ,, prf'' = ix-prf'' in
      subst
        (λ hole → Σ[ ix' ∈ ((Γ ++ ctx , x) ∋ hole) ] (a ≡ var (∋-to-ℕ ix')))
        (begin
          rename suc (rename pred A)
        ≡⟨ rename-∘ A ⟩
          rename (suc ∘ pred) A
        ≡⟨ rename-under-f-≡-rename-sucₗ A Y rename-A' ⟩
          A
        ∎)
        (ix'' ,, prf'')
  in
    ix-prf'''

thing :
  {r : Term} →
  (Γ' : Ctx) →
  (A Y : Term) →
  rename (underₙ Γ' suc) A ≡ substitute (inst1 r) Y →
  (Σ[ r' ∈ Term ] (rename (underₙ Γ' suc) r' ≡ r)) ⊎
  (Σ[ Y' ∈ Term ] (rename (under (underₙ Γ' suc)) Y' ≡ Y))
thing Γ' A (var ix') prf = {!!}
thing Γ' Type Type refl = inj₂ (Type ,, refl)
thing Γ' Void Void prf = {!!}
thing Γ' A Unit prf = {!!}
thing Γ' A (Pi Y Y₁) prf = {!!}
thing Γ' A (Sigma Y Y₁) prf = {!!}
thing Γ' A (lam Y Y₁) prf = {!!}
thing Γ' A (app a b) prf = {!!}

mutual
  ⊢-strengthen :
    {Γ : Ctx} {a a' A A' X : Term} →
    (Γ' : Ctx) →
    ⊢ (Γ , X) ++ rename-ctx suc Γ' ctx →
    rename (underₙ Γ' suc) a ≡ a' →
    rename (underₙ Γ' suc) A ≡ A' →
    (Γ , X) ++ rename-ctx suc Γ' ⊢ a' ∶ A' →
    Γ ++ Γ' ⊢ a ∶ A
  ⊢-strengthen {Γ} {A = A} Γ' ctx rename-a rename-A (⊢-var ix) =
    let ix' ,, ix'-correct = ∋-strengthen Γ' ix rename-a rename-A in
    let tm = ⊢-var ix' in
    subst (λ x → Γ ++ Γ' ⊢ x ∶ A) (sym ix'-correct) tm
  ⊢-strengthen {Γ} {A = A} Γ' ctx rename-a rename-A ⊢-Void = 
    subst
      (λ x → Γ ++ Γ' ⊢ x ∶ A)
      (sym (rename-Void (underₙ-injective suc-injective Γ') _ rename-a))
      (subst (λ x → Γ ++ Γ' ⊢ Void ∶ x) (sym (rename-Type _ rename-A)) ⊢-Void)
  ⊢-strengthen {Γ} {A = A} Γ' ctx rename-a rename-A ⊢-Unit =
    subst
      (λ x → Γ ++ Γ' ⊢ x ∶ A)
      (sym (rename-Unit (underₙ-injective suc-injective Γ') _ rename-a))
      (subst (λ x → Γ ++ Γ' ⊢ Unit ∶ x) (sym (rename-Type _ rename-A)) ⊢-Unit)
  ⊢-strengthen {Γ} {A = A} {X = X} Γ' ctx rename-a rename-A (⊢-Pi {B} {C} Btype Ctype) with rename-Pi (underₙ-injective suc-injective Γ') _ rename-a
  ... | B' ,, C' ,, ≡Pi ,, rename-B' ,, rename-C' =
    subst
      (λ x → Γ ++ Γ' ⊢ x ∶ A)
      (sym ≡Pi)
      (subst
        (λ x → Γ ++ Γ' ⊢ Pi B' C' ∶ x)
        (sym (rename-Type _ rename-A))
        (⊢-Pi
          (⊢-type-strengthen Γ' rename-B' Btype)
          (⊢-type-strengthen
            (Γ' , B')
            (trans (rename-cong (underₙ-under-comm Γ') C') rename-C')
            (subst (λ x → Γ , X ++ rename-ctx suc Γ' , x ⊢ C type) (sym rename-B') Ctype))))
  ⊢-strengthen {Γ} {A = A} {X = X} Γ' ctx rename-a rename-A (⊢-Sigma {B} {C} Btype Ctype) with rename-Sigma (underₙ-injective suc-injective Γ') _ rename-a
  ... | B' ,, C' ,, ≡Sigma ,, rename-B' ,, rename-C' =
    subst
      (λ x → Γ ++ Γ' ⊢ x ∶ A)
      (sym ≡Sigma)
      (subst
        (λ x → Γ ++ Γ' ⊢ Sigma B' C' ∶ x)
        (sym (rename-Type _ rename-A))
        (⊢-Sigma
          (⊢-type-strengthen Γ' rename-B' Btype)
          (⊢-type-strengthen
            (Γ' , B')
            (trans (rename-cong (underₙ-under-comm Γ') C') rename-C')
            (subst (λ x → Γ , X ++ rename-ctx suc Γ' , x ⊢ C type) (sym rename-B') Ctype))))
  ⊢-strengthen {Γ} {A = A} {X = X} Γ' ctx rename-a rename-A (⊢-lam {B} {C} {e} Btype e∶C) with rename-lam (underₙ-injective suc-injective Γ') _ rename-a
  ... | B' ,, e' ,, ≡lam ,, rename-B' ,, rename-e' =
    let
      Bstrengthened = ⊢-type-strengthen Γ' rename-B' Btype

      B'' ,, C' ,, ≡Pi ,, rename-B'' ,, rename-C' = rename-Pi (underₙ-injective suc-injective Γ') A rename-A

      estrengthened : Γ ++ Γ' , B' ⊢ e' ∶ C'
      estrengthened =
        ⊢-strengthen
          (Γ' , B')
          (⊢-, ctx (subst (λ x → Γ , X ++ rename-ctx suc Γ' ⊢ x type) (sym rename-B') Btype))
          (trans (rename-cong (underₙ-under-comm Γ') e') rename-e')
          (trans (rename-cong (underₙ-under-comm Γ') C') rename-C')
          (subst (λ x → Γ , X ++ rename-ctx suc Γ' , x ⊢ e ∶ C) (sym rename-B') e∶C)
    in
      subst
        (λ x → Γ ++ Γ' ⊢ x ∶ A)
        (sym ≡lam)
        (subst
          (λ x → Γ ++ Γ' ⊢ lam B' e' ∶ x)
          (trans
            (cong₂
              Pi
              (rename-injective
                (underₙ-injective suc-injective Γ')
                _
                _
                (trans rename-B' (sym rename-B'')))
              refl)
            (sym ≡Pi))
          (⊢-lam Bstrengthened estrengthened))
  ⊢-strengthen {Γ} {A = A} Γ' ctx rename-a rename-A (⊢-app {A = X} {B = Y} {B' = Y'} {l} {r} refl l∶PiXY r∶X) with rename-app (underₙ-injective suc-injective Γ') _ rename-a
  ... | l' ,, r' ,, ≡app ,, rename-l' ,, rename-r' =
    let
      rename-A : rename (underₙ Γ' suc) A ≡ substitute (inst1 r) Y
      rename-A = rename-A

      PiXYtype : Γ , _ ++ rename-ctx suc Γ' ⊢ Pi X Y type
      PiXYtype = ⊢-typeOf ctx l∶PiXY

      test :
        Σ[ rX ∈ Term ]
        Σ[ rY ∈ Term ]
        ((rename (underₙ Γ' suc) rX ≡ X) × rename (under (underₙ Γ' suc)) rY ≡ Y)
      test = {!!}

      rX : Term
      rX = {!!}

      rename-X : rename (underₙ Γ' suc) rX ≡ X
      rename-X = {!!}

      rY : Term
      rY = {!!}

      rename-Y : rename (under (underₙ Γ' suc)) rY ≡ Y
      rename-Y = {!!}

      lstrengthened : Γ ++ Γ' ⊢ l' ∶ Pi rX rY
      lstrengthened = ⊢-strengthen Γ' {!!} rename-l' (cong₂ Pi rename-X rename-Y) l∶PiXY

      rstrengthened : Γ ++ Γ' ⊢ r' ∶ rX
      rstrengthened = ⊢-strengthen Γ' {!!} rename-r' rename-X r∶X

      ≡A : rY [ r' ] ≡ A
      ≡A = {!!}
    in
      subst
        (λ x → Γ ++ Γ' ⊢ x ∶ A)
        (sym ≡app)
        (subst
          (λ x → Γ ++ Γ' ⊢ app l' r' ∶ x)
          ≡A
          (⊢-app refl lstrengthened rstrengthened))

  ⊢-type-strengthen :
    {Γ : Ctx} {A A' X : Term} →
    (Γ' : Ctx) →
    rename (underₙ Γ' suc) A ≡ A' →
    (Γ , X) ++ rename-ctx suc Γ' ⊢ A' type →
    Γ ++ Γ' ⊢ A type
  ⊢-type-strengthen Γ' prf (⊢-El tm) =
    ⊢-El (⊢-strengthen Γ' {!!} prf refl tm)
  ⊢-type-strengthen {Γ} Γ' prf ⊢-Type = 
    subst (λ x → Γ ++ Γ' ⊢ x type) (sym (rename-Type _ prf)) ⊢-Type

strengthen-ctx :
  {Γ : Ctx} {X : Term} →
  (Γ' : Ctx) →
  ⊢ (Γ , X ++ rename-ctx suc Γ') ctx →
  ⊢ (Γ ++ Γ') ctx
strengthen-ctx ◆ (⊢-, ctx x) = ctx
strengthen-ctx (ctx , X) (⊢-, ctx' Xtype) = 
  ⊢-, (strengthen-ctx ctx ctx') (⊢-type-strengthen ctx refl Xtype)

record Lookup (Γ : Ctx) (n : ℕ) : Set where
  field
    {A} : Term
    ix : Γ ∋ A
    Atype : Γ ⊢ A type
    correct : ∋-to-ℕ ix ≡ n

lookup :
  {Γ : Ctx} →
  (ix : ℕ) →
  ⊢ Γ ctx →
  Dec (Lookup Γ ix)
lookup zero ⊢-◆ = no λ{ record { ix = ix } → case ix of λ() }
lookup zero (⊢-, ctx Atype) = yes (record { ix = here refl ; Atype = ⊢-type-rename Renaming-weaken1 Atype ; correct = refl })
lookup (suc ix) ⊢-◆ = no λ{ record { ix = ix } → case ix of λ() }
lookup (suc ix) (⊢-, ctx Atype) with lookup ix ctx
... | no ¬prf =
  no λ{
    record { ix = here refl ; Atype = Atype' ; correct = correct' } → case correct' of λ()
    ;
    record { ix = there refl ix' ; Atype = Atype' ; correct = correct' } → ¬prf (record { ix = ix' ; Atype = {!!} ; correct = suc-injective correct' })
  }
... | yes record { ix = ix ; Atype = Atype ; correct = correct } =
  yes (record { ix = there refl ix ; Atype = ⊢-type-rename Renaming-weaken1 Atype ; correct = cong suc correct })

{-


Lookup-proj : {Γ : Ctx} {A : Term} {ix : ℕ} → Lookup (Γ , A) (suc ix) → Lookup Γ ix
Lookup-proj {Γ} {A} record { ix = there {X} {X'} {Y} r ix ; Atype = Atype ; correct = correct } =
  record {
    ix = ix
    ;
    Atype =
      let
        Atype' : Γ , A ⊢ rename suc X' type
        Atype' = subst (λ x → Γ , A ⊢ x type) (sym r) Atype
      in
        ⊢-type-strengthen1 refl Atype'
    ;
    correct = suc-injective correct
    }


mutual
  check-type :
    {Γ : Ctx} →
    ⊢ Γ ctx →
    (A : Term) →
    Dec (Γ ⊢ A type)
  check-type ctx (var ix) = {!!}
  check-type ctx Type = yes ⊢-Type
  check-type ctx Void = yes (⊢-El ⊢-Void)
  check-type ctx Unit = yes (⊢-El ⊢-Unit)
  check-type ctx (Pi A B) with check-type ctx A 
  ... | no ¬prf = {!!}
  ... | yes Atype with check-type (⊢-, ctx Atype) B 
  ... | no ¬prf = {!!}
  ... | yes Btype = yes (⊢-El (⊢-Pi Atype Btype))
  check-type ctx (Sigma A B) = {!!}
  check-type ctx (lam A e) = no {!!}
  check-type ctx (app A A₁) = {!!}


  check-term :
    {Γ : Ctx} {A : Term} →
    ⊢ Γ ctx →
    Γ ⊢ A type →
    (a : Term) →
    Dec (Γ ⊢ a ∶ A)

  check-term ctx ty Type = no λ()
  check-term ctx ty (var ix) = {!!}
  check-term ctx ty Void with ty
  ... | ⊢-Type = yes ⊢-Void
  ... | ⊢-El A∶Type = no λ{ ⊢-Void → ¬Type∶Type A∶Type }
  check-term ctx ty Unit with ty
  ... | ⊢-Type = yes ⊢-Unit
  ... | ⊢-El A∶Type = no λ{ ⊢-Unit → ¬Type∶Type A∶Type }
  check-term ctx ty (Pi a a₁) = {!!}
  check-term ctx ty (Sigma a a₁) = {!!}
  check-term ctx ty (lam a a₁) = {!!}
  check-term ctx ty (app a a₁) = {!!}

{-

record Lookup (n : ℕ) (Γ : Ctx) : Set where
  field
    {A} : Term
    ix : A ∈ Γ
    ix-correct : ∈-to-ℕ ix ≡ n
    Atype : Γ ⊢ A type

Lookup-here : {Γ : Ctx} {A : Term} → Γ ⊢ A type → Lookup zero (Γ , A)
Lookup-here Atype = record { ix = here ; ix-correct = refl ; Atype = {!!} }

Lookup-there :
  {Γ : Ctx} {ix : ℕ} {A : Term} →
  Γ ⊢ A type →
  Lookup ix Γ →
  Lookup (suc ix) (Γ , A)
Lookup-there Atype Lookup-ix-Γ =
  record {
    ix = there (Lookup.ix Lookup-ix-Γ)
    ;
    ix-correct = cong suc (Lookup.ix-correct Lookup-ix-Γ)
    ;
    Atype = {!!} -- ⊢-type-weak Atype (Lookup.Atype Lookup-ix-Γ)
  }

Lookup-thereᵢ :
  {Γ : Ctx} {ix : ℕ} {X : Term} →
  Lookup (suc ix) (Γ , X) →
  Lookup ix Γ
Lookup-thereᵢ record { ix = there ix ; ix-correct = ix-correct ; Atype = Atype } =
  record { ix = ix ; ix-correct = suc-injective ix-correct ; Atype = {!!} }

lookup :
  {Γ : Ctx} →
  (ix : ℕ) →
  ⊢ Γ ctx →
  Dec (Lookup ix Γ)
lookup zero ⊢-◆ =
  no λ{ record{ ix = ix } → case ix of λ() }
lookup zero (⊢-, ctx Atype) =
  yes (Lookup-here Atype)
lookup (suc ix) ⊢-◆ =
  no λ{ record{ ix = ix } → case ix of λ() }
lookup (suc ix) (⊢-, ctx Atype) with lookup ix ctx
... | no ¬prf = no λ{ l → ¬prf {!!} }
... | yes Lookup-ix-Γ = yes (Lookup-there Atype Lookup-ix-Γ)

data _⊢_eq_type (Γ : Ctx) : Term → Term → Set where
  ⊢-eq-Void : Γ ⊢ Void eq Void type
  ⊢-eq-Unit : Γ ⊢ Unit eq Unit type

⊢-eq-type-refl : {Γ : Ctx} → ⊢ Γ ctx → (A : Term) → Γ ⊢ A eq A type
⊢-eq-type-refl ctx (var x) = {!!}
⊢-eq-type-refl ctx Type = {!!}
⊢-eq-type-refl ctx Void = ⊢-eq-Void 
⊢-eq-type-refl ctx Unit = ⊢-eq-Unit
⊢-eq-type-refl ctx (Pi A A₁) = {!!}
⊢-eq-type-refl ctx (Sigma A A₁) = {!!}
⊢-eq-type-refl ctx (lam A A₁) = {!!}
⊢-eq-type-refl ctx (app A A₁) = {!!}

⊢-eq-type-sym :
  {Γ : Ctx} {A B : Term} →
  Γ ⊢ A eq B type →
  Γ ⊢ B eq A type
⊢-eq-type-sym ⊢-eq-Void = ⊢-eq-Void
⊢-eq-type-sym ⊢-eq-Unit = ⊢-eq-Unit

⊢eq-type-conv :
  {Γ : Ctx} {A B x : Term} →
  Γ ⊢ A eq B type →
  Γ ⊢ x ∶ A →
  Γ ⊢ x ∶ B
⊢eq-type-conv ⊢-eq-Void x∶A = x∶A
⊢eq-type-conv ⊢-eq-Unit x∶A = x∶A

check-type-≡-weaken :
  {Γ : Ctx} {X A A' : Term} →
  Γ ⊢ X type →
  Dec (Γ ⊢ A eq A' type) →
  Dec (Γ , X ⊢ A eq A' type)
check-type-≡-weaken Xtype (yes ⊢-eq-Void) = yes ⊢-eq-Void
check-type-≡-weaken Xtype (yes ⊢-eq-Unit) = yes ⊢-eq-Unit
check-type-≡-weaken Xtype (no ¬prf) =
  no λ{
    ⊢-eq-Void → ¬prf ⊢-eq-Void
    ;
    ⊢-eq-Unit → ¬prf ⊢-eq-Unit
  }

{-
check-type-≡ :
  {Γ : Ctx} {A A' : Term} →
  Γ ⊢ A type →
  Γ ⊢ A' type →
  Dec (Γ ⊢ A eq A' type)
check-type-≡ (⊢-El x) A'type = {!!}
check-type-≡ (⊢-Type x) A'type = {!!}
check-type-≡ (⊢-type-weak Atype Atype₁) A'type = {!!}

∈-cong :
  {Γ : Ctx} {A B : Term}
  (ix : A ∈ Γ) →
  (ix' : B ∈ Γ) →
  ∈-to-ℕ ix ≡ ∈-to-ℕ ix' →
  A ≡ B
∈-cong here here prf = refl
∈-cong (there ix) (there ix') prf = ∈-cong ix ix' (suc-injective prf)
-}




{-
check-term :
  {Γ : Ctx} {A : Term} →
  ⊢ Γ ctx →
  Γ ⊢ A type →
  (a : Term) →
  Dec (Γ ⊢ a ∶ A)

check-term ctx ty Type = no λ ()

check-term {Γ} {A = A} ctx ty (var ix) with lookup ix ctx
... | no ¬prf = no λ{ (⊢-var Γctx A∈Γ) → ¬prf (A ,, A∈Γ ,, refl ,, ty) }
... | yes (A' ,, A'∈Γ ,, ∈-to-ℕ-A'∈Γ≡ix ,, A'type) with check-type-≡ ty A'type
... | no ¬prf =
  no λ{
    (⊢-var Γctx A∈Γ) →
      ¬prf
        (subst
          (λ x → Γ ⊢ A eq x type)
          (sym (∈-cong A'∈Γ A∈Γ ∈-to-ℕ-A'∈Γ≡ix))
          (⊢-eq-type-refl Γctx A))
  }
... | yes prf =
  yes
    (subst
      (λ x → Γ ⊢ var x ∶ A)
      ∈-to-ℕ-A'∈Γ≡ix
      (⊢eq-type-conv (⊢-eq-type-sym prf) (⊢-var ctx A'∈Γ)))

check-term ctx (⊢-Type ctx') Void = yes {!!}
check-term ctx _ Void = {!!}

check-term ctx ty Unit = {!!}
check-term ctx ty (Pi tm tm₁) = {!!}
check-term ctx ty (Sigma tm tm₁) = {!!}
check-term ctx ty (lam tm tm₁) = {!!}
check-term ctx ty (app tm tm₁) = {!!}

-}

-}

-}
