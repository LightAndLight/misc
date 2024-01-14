module lib where

record Semiring (F : Set) : Set where
  field
    zero : F
    add : F → F → F

    one : F
    mul : F → F → F

open import Data.Nat
open import Data.Product hiding (map)
open import Data.Vec hiding ([_])

module Matrix (F : Set) (SF : Semiring F) where
  𝟙 : F
  𝟙 = Semiring.one SF 

  𝟘 : F
  𝟘 = Semiring.zero SF 

  data Matrix : ℕ → ℕ → Set where
    [] : Matrix 0 0
    [_] : F → Matrix 1 1
    _↕_ : ∀{x a b} → Matrix x a → Matrix x b → Matrix x (a + b)
    _↔_ : ∀{x a b} → Matrix a x → Matrix b x → Matrix (a + b) x

  _∙_ : ∀{m n} → F → Matrix m n → Matrix m n
  s ∙ [] = []
  s ∙ [ x ] = [ Semiring.mul SF s x ]
  s ∙ (top ↕ bottom) = (s ∙ top) ↕ (s ∙ bottom)
  s ∙ (left ↔ right) = (s ∙ left) ↔ (s ∙ right)

  sm≤sn→m≤n : ∀{a b} → suc a ≤ suc b → a ≤ b
  sm≤sn→m≤n (s≤s prf) = prf

  open import Relation.Binary.PropositionalEquality
  open import Data.Nat.Properties

  a+b≡0 : ∀{a b} → a + b ≡ 0 → a ≡ 0 × b ≡ 0
  a+b≡0 {zero} prf = refl , prf
  a+b≡0 {suc a} {b} ()

  a+b≡a+b'→b≡b' : (a b b' : ℕ) → a + b ≡ a + b' → b ≡ b'
  a+b≡a+b'→b≡b' zero b b' prf = prf
  a+b≡a+b'→b≡b' (suc a) b b' prf = a+b≡a+b'→b≡b' a b b' (suc-injective prf)

  open import Relation.Nullary
  open import Data.Sum

  a≤b+c→a≤b? : ∀{a b c} → a ≤ b + c → (a ≤ b) ⊎ (a ≰ b × (a ∸ b) ≤ c)
  a≤b+c→a≤b? {b = zero} z≤n = inj₁ z≤n
  a≤b+c→a≤b? {b = zero} (s≤s prf) = inj₂ ((λ ()) , s≤s prf)
  a≤b+c→a≤b? {b = suc b} z≤n = inj₁ z≤n
  a≤b+c→a≤b? {b = suc b} (s≤s prf) with a≤b+c→a≤b? {b = b} prf
  ... | inj₁ prf = inj₁ (s≤s prf)
  ... | inj₂ (m≰b , m≤c) = inj₂ ((λ{ (s≤s m≤b) → m≰b m≤b }) , m≤c)

  ∸-thing : ∀{a b} → a + (suc b ∸ a) ≡ suc b
  ∸-thing {a = zero} = refl
  ∸-thing {a = suc a} {b} with ∸-thing {a} {b}
  ... | res = trans {!!} res

  Matrix-split-↕ : ∀{c x} → (a : ℕ) → {a≤c : a ≤ c} → Matrix x c → Σ[ b ∈ ℕ ] (Matrix x a × Matrix x b × (a + b ≡ c))
  Matrix-split-↕ {c = c} zero f = c , {!!} , f , refl
  Matrix-split-↕ (suc a) {a≤c} [ x ] with Matrix-split-↕ a {a≤c = sm≤sn→m≤n a≤c} []
  ... | b , xa , xb , prf rewrite proj₁ (a+b≡0 {a = a} {b = b} prf) | proj₂ (a+b≡0 {a = a} {b = b} prf) =
    zero , [ x ] , {!!} , refl
  Matrix-split-↕ (suc a) {a≤c} (_↕_ {a = topHeight} top bottom) with a≤b+c→a≤b? {b = topHeight} a≤c
  Matrix-split-↕ (suc a) {a≤c} (_↕_ {a = topHeight} top bottom) | inj₁ a≤topHeight with Matrix-split-↕ (suc a) {a≤topHeight} top
  ... | b , xa , xb , refl =
    b + _ , xa , (xb ↕ bottom) , cong suc (sym (+-assoc a b _))
  Matrix-split-↕ (suc a) {a≤c} (_↕_ {a = topHeight} {b = b} top bottom) | inj₂ (suca≰topHeight , _≤b) with Matrix-split-↕ (suc a ∸ topHeight) {a≤c = _≤b} bottom
  ... | n , xa , xb , prf = {!!} , subst (λ x → Matrix _ x) {!!} (top ↕ xa) , xb , {!!}
  Matrix-split-↕ (suc a) {a≤c} (left ↔ right) with Matrix-split-↕ (suc a) {a≤c} left | Matrix-split-↕ (suc a) {a≤c} right
  ... | lb , lxa , lxb , lprf | rb , rxa , rxb , rprf
    rewrite a+b≡a+b'→b≡b' a lb rb (suc-injective (trans lprf (sym rprf))) =
      rb , (lxa ↔ rxa) , (lxb ↔ rxb) , rprf

  Matrix-split-↔ : ∀{c x} → (a : ℕ) → Matrix c x → Σ[ b ∈ ℕ ] (Matrix a x × Matrix b x × (a + b ≡ c))
  Matrix-split-↔ a f = {!!}

  Matrix-map : ∀{a b} → (F → F) → Matrix a b → Matrix a b
  Matrix-map f [] = []
  Matrix-map f [ x ] = [ f x ]
  Matrix-map f (m ↕ m₁) = Matrix-map f m ↕ Matrix-map f m₁
  Matrix-map f (m ↔ m₁) = Matrix-map f m ↔ Matrix-map f m₁

  _⊙_ : ∀{a b} → Matrix a b → Matrix a b → (F → F → F) → Matrix a b
  ([] ⊙ g) f = []
  ([ x ] ⊙ g) f = Matrix-map (f x) g
  ((_↕_ {a = a} {b = b} top bottom) ⊙ g) f with Matrix-split-↕ a {a≤c = m≤m+n a b} g
  ... | b' , top' , bottom' , prf rewrite (a+b≡a+b'→b≡b' a b' b prf) =
    (top ⊙ top') f ↕ (bottom ⊙ bottom') f
  ((_↔_ {a = a} {b = b} left right) ⊙ g) f with Matrix-split-↔ a g
  ... | b' , left' , right' , prf rewrite (a+b≡a+b'→b≡b' a b' b prf) =
    (left ⊙ left') f ↔ (right ⊙ right') f

  _∩_ : ∀{a b} → Matrix a b → Matrix a b → Matrix a b
  a ∩ b = (a ⊙ b) (Semiring.mul SF)

  _∪_ : ∀{a b} → Matrix a b → Matrix a b → Matrix a b
  a ∪ b = (a ⊙ b) (Semiring.add SF)

  _∘_ : ∀{m n o} → Matrix n o → Matrix m n → Matrix m o
  [] ∘ g = g
  [ x ] ∘ g = x ∙ g
  (top ↕ bottom) ∘ g = (top ∘ g) ↕ (bottom ∘ g)
  (_↔_ {a = a} {b = b} left right) ∘ g with Matrix-split-↕ a {a≤c = {!!}} g
  ... | n , top , bottom , prf rewrite a+b≡a+b'→b≡b' a n b prf =
    let first = left ∘ top in
    let second = right ∘ bottom in
    first ∪ second

  row : ∀{n} → Vec F (suc n) → Matrix (suc n) 1
  row (x ∷ xs) with xs
  ... | [] = [ x ]
  ... | _ ∷ _ = [ x ] ↔ row xs

  col : ∀{n} → Vec F (suc n) → Matrix 1 (suc n)
  col (x ∷ xs) with xs
  ... | [] = [ x ]
  ... | _ ∷ _ = [ x ] ↕ col xs

  idₙ : (n : ℕ) → {prf : n > 0} → Matrix n n
  idₙ (suc n) {s≤s prf} with n
  ... | zero = [ Semiring.one SF ]
  idₙ (suc n) {s≤s prf} | suc n' =
    ([ Semiring.one SF ] ↔ row (replicate {n = suc n'} (Semiring.zero SF)))
    ↕
    (col (replicate {n = suc n'} (Semiring.zero SF)) ↔ idₙ (suc n') {prf = s≤s z≤n})

  apply : ∀{m n} → Matrix m n → Vec F m → Vec F n
  apply [] nil = nil
  apply [ s ] (x ∷ []) = Semiring.mul SF s x ∷ []
  apply (top ↕ bottom) x =
    apply top x ++ apply bottom x
  apply (_↔_ {a = a} {b = b} left right) x =
    zipWith (Semiring.add SF) (apply left (take a x)) (apply right (drop a x))

  open import Data.Fin hiding (_>_; _+_; _≤_)

  record V (A : ℕ × Set) : Set where
    field
      key : Fin (proj₁ A) → proj₂ A
      values : Vec F (proj₁ A)
∞
  record VMap (A : ℕ × Set) (B : ℕ × Set) : Set where
    field
      inKey : Fin (proj₁ A) → proj₂ A
      outKey : Fin (proj₁ B) → proj₂ B
      f : Matrix (proj₁ A) (proj₁ B)

  V-id : ∀{n} {prf : n > 0} {A} → (Fin n → A) → VMap (n , A) (n , A)
  V-id {n} {prf} key = record { inKey = key ; outKey = key ; f = idₙ n {prf}}

  _V-∘_ : ∀{m n o A B C} → VMap (n , B) (o , C) → VMap (m , A) (n , B) → VMap (m , A) (o , C)
  f V-∘ g = record { inKey = VMap.inKey g ; outKey = VMap.outKey f ; f = VMap.f f ∘ VMap.f g }

  zeros : (m n : ℕ) → {m>0 : m > 0} → {n>0 : n > 0} → Matrix m n
  zeros m (suc n) {s≤s prf} {s≤s n'>0} with n
  ... | zero = row (replicate {n = m} 𝟘)
  ... | suc n' = row (replicate {n = m} 𝟘) ↕ zeros m (suc n') {m>0 = s≤s prf} {n>0 = s≤s z≤n}

  outₗ : (a b : ℕ) → {a>0 : a > 0} {b>0 : b > 0} → Matrix (a * b) a
  outₗ (suc a) b {s≤s a'>0} {s≤s 0≤b} with a
  ... | zero rewrite +-identityʳ b = row (replicate {n = b} 𝟙)
  ... | suc a' =
    (row (replicate {n = b} 𝟙) ↔ row (replicate {n = suc a'} 𝟘 >>= λ v → replicate {n = b} v))
    ↕
    (zeros b (suc a') {m>0 = s≤s 0≤b} {n>0 = s≤s z≤n} ↔ (outₗ (suc a') b) {a>0 = s≤s z≤n} {b>0 = s≤s 0≤b})

  outᵣ : (a b : ℕ) → {a>0 : a > 0} → {b>0 : b > 0} → Matrix (a * b) b
  outᵣ (suc a) b {a>0} {b>0} with a
  ... | zero rewrite +-identityʳ b = idₙ b {b>0}
  ... | suc a' = idₙ b {b>0} ↔ outᵣ (suc a') b {a>0 = s≤s z≤n} {b>0}

  _ᵒ : ∀{a b} → Matrix a b → Matrix b a
  [] ᵒ = []
  [ x ] ᵒ = [ x ]
  (top ↕ bottom) ᵒ = (top ᵒ) ↔ (bottom ᵒ)
  (left ↔ right) ᵒ = (left ᵒ) ↕ (right ᵒ)

  ⟨_,_⟩ : ∀{x a b} → Matrix x a → Matrix x b → ∀{a>0 : a > 0} {b>0 : b > 0} → Matrix x (a * b)
  ⟨_,_⟩ {x} {a} {b} f g {a>0} {b>0} = ((outₗ a b {a>0} {b>0} ᵒ) ∘ f) ∩ ((outᵣ a b {a>0} {b>0} ᵒ) ∘ g)

  Fin-a+b : (a b : ℕ) → Fin (a + b) → Fin a ⊎ Fin b
  Fin-a+b zero b ix = inj₂ ix
  Fin-a+b (suc a) b zero = inj₁ zero
  Fin-a+b (suc a) b (suc ix) with Fin-a+b a b ix
  ... | inj₁ ix' = inj₁ (suc ix')
  ... | inj₂ ix' = inj₂ ix'

  Fin-a*b : (a b : ℕ) → Fin (a * b) → Fin a × Fin b
  Fin-a*b zero b ()
  Fin-a*b (suc a) b ix with Fin-a+b b (a * b) ix
  ... | inj₁ ix' = zero , ix'
  Fin-a*b (suc a) b ix | inj₂ ix' with Fin-a*b a b ix'
  ... | ixa , ixb = suc ixa , ixb

  _V-×_ :
    ∀{x a b X A B} →
    ∀{a>0 : a > 0} {b>0 : b > 0} →
    (f : VMap (x , X) (a , A)) →
    (g : VMap (x , X) (b , B)) →
    ∀{_ : VMap.inKey f ≡ VMap.inKey g} →
    VMap (x , X) ((a * b) , (A × B))
  _V-×_ {a = a} {b = b} {a>0 = a>0} {b>0 = b>0} f g {refl} = record {
    inKey = VMap.inKey f
    ; outKey = λ ix → let ixa , ixb = Fin-a*b a b ix in VMap.outKey f ixa , VMap.outKey g ixb
    ; f = ⟨ VMap.f f , VMap.f g ⟩ {a>0} {b>0}
    }

  summarise : (a : ℕ) → {a>0 : a > 0} → Matrix a 1
  summarise (suc zero) = [ 𝟙 ]
  summarise (suc (suc a)) = [ 𝟙 ] ↔ summarise (suc a) {a>0 = s≤s z≤n}

  members : (a : ℕ) → {a>0 : a > 0} → Matrix 1 a
  members a {a>0}= summarise a {a>0} ᵒ

  open import Data.Unit

  V-summarise : ∀{A} {a : ℕ} → (Fin a → A) → {a>0 : a > 0} → VMap (a , A) (1 , ⊤)
  V-summarise {a = a} key {a>0} = record { inKey = key ; outKey = λ{ zero → tt } ; f = summarise a {a>0} }

  V-members : ∀{A} {a : ℕ} → (Fin a → A) → {a>0 : a > 0} → VMap (1 , ⊤) (a , A) 
  V-members {a = a} key {a>0} = record { inKey = λ{ zero → tt } ; outKey = key ; f = members a {a>0} }

  V-apply : ∀{m n A B} → VMap (m , A) (n , B) → V (m , A) → V (n , B)
  V-apply f x = record { key = VMap.outKey f ; values = apply (VMap.f f) (V.values x) }

  V-⊤ : V (1 , ⊤)
  V-⊤ = (record { key = λ{ Fin.zero → tt } ; values = 𝟙 ∷ [] })

  V-point : ∀{n A} {n>0 : n > 0} → V (n , A) → VMap (1 , ⊤) (n , A)
  V-point {n>0 = s≤s _} a = record { inKey = λ{ Fin.zero → tt } ; outKey = V.key a ; f = col (V.values a) }

  Matrix-assoc1ᵣ : ∀{b} → (a : ℕ) → Matrix a b → Matrix 1 (a * b)
  Matrix-assoc1ᵣ zero m = {!!}
  Matrix-assoc1ᵣ (suc a) m with Matrix-split-↔ 1 m
  ... | n , left , right , refl = left ↕ Matrix-assoc1ᵣ a right

  Matrix-assoc1ₗ : ∀{b} → (a : ℕ) → Matrix 1 (a * b) → Matrix a b
  Matrix-assoc1ₗ zero m = {!!}
  Matrix-assoc1ₗ {b} (suc a) m with Matrix-split-↕ b {a≤c = m≤m+n b (a * b)} m
  ... | n , top , bottom , prf rewrite a+b≡a+b'→b≡b' b n (a * b) prf = top ↔ Matrix-assoc1ₗ a bottom

  Matrix-assocᵣ : ∀{a b c} → Matrix (a * b) c → Matrix a (b * c)
  Matrix-assocᵣ {zero} {b} {c} m = {!!}
  Matrix-assocᵣ {suc a} {b} {c} m with Matrix-split-↔ b m
  ... | n , l , r , prf rewrite a+b≡a+b'→b≡b' b n (a * b) prf = 
    let res = Matrix-assocᵣ {a} r in
    Matrix-assoc1ᵣ b l ↔ res

module Test1 where
  open import Data.Bool
  open import Relation.Binary.PropositionalEquality

  SemiringBool : Semiring Bool
  SemiringBool = record {
    zero = false ;
    add = _∨_ ;
    one = true ;
    mul = _∧_
    }

  open Matrix Bool SemiringBool

  applyOutₗ-test1 : apply (outₗ 2 3 {a>0 = s≤s z≤n} {b>0 = s≤s z≤n}) (𝟙 ∷ 𝟘 ∷ 𝟘 ∷ 𝟘 ∷ 𝟙 ∷ 𝟘 ∷ []) ≡ (𝟙 ∷ 𝟙 ∷ [])
  applyOutₗ-test1 = refl

  applyOutₗ-test2 : apply (outₗ 2 3 {a>0 = s≤s z≤n} {b>0 = s≤s z≤n}) (𝟙 ∷ 𝟘 ∷ 𝟘 ∷ 𝟘 ∷ 𝟘 ∷ 𝟘 ∷ []) ≡ (𝟙 ∷ 𝟘 ∷ [])
  applyOutₗ-test2 = refl

  applyOutₗ-test3 : apply (outₗ 2 3 {a>0 = s≤s z≤n} {b>0 = s≤s z≤n}) (𝟘 ∷ 𝟘 ∷ 𝟘 ∷ 𝟘 ∷ 𝟙 ∷ 𝟙 ∷ []) ≡ (𝟘 ∷ 𝟙 ∷ [])
  applyOutₗ-test3 = refl


  applyOutᵣ-test1 : apply (outᵣ 2 3 {a>0 = s≤s z≤n} {b>0 = s≤s z≤n}) (𝟙 ∷ 𝟘 ∷ 𝟘 ∷ 𝟘 ∷ 𝟙 ∷ 𝟘 ∷ []) ≡ (𝟙 ∷ 𝟙 ∷ 𝟘 ∷ [])
  applyOutᵣ-test1 = refl

  applyOutᵣ-test2 : apply (outᵣ 2 3 {a>0 = s≤s z≤n} {b>0 = s≤s z≤n}) (𝟙 ∷ 𝟘 ∷ 𝟘 ∷ 𝟘 ∷ 𝟘 ∷ 𝟘 ∷ []) ≡ (𝟙 ∷ 𝟘 ∷ 𝟘 ∷ [])
  applyOutᵣ-test2 = refl

  applyOutᵣ-test3 : apply (outᵣ 2 3 {a>0 = s≤s z≤n} {b>0 = s≤s z≤n}) (𝟘 ∷ 𝟘 ∷ 𝟘 ∷ 𝟘 ∷ 𝟙 ∷ 𝟙 ∷ []) ≡ (𝟘 ∷ 𝟙 ∷ 𝟙 ∷ [])
  applyOutᵣ-test3 = refl

module Test2 where
  Semiringℕ : Semiring ℕ
  Semiringℕ = record {
    zero = 0 ;
    add = _+_ ;
    one = 1 ;
    mul = _*_
    }

  open Matrix ℕ Semiringℕ
  open import Data.String
  open import Data.Unit
  open import Data.Fin using (Fin)

  salaries : V (5 , String)
  salaries = record {
    key = λ{
      Fin.zero →
        "a" ;
      (Fin.suc Fin.zero) →
        "b" ;
      (Fin.suc (Fin.suc Fin.zero)) →
        "c" ;
      (Fin.suc (Fin.suc (Fin.suc Fin.zero))) →
        "d" ;
      (Fin.suc (Fin.suc (Fin.suc (Fin.suc Fin.zero)))) →
        "e"
    } ;
    values =
      200 ∷
      300 ∷
      100 ∷
      350 ∷
      400 ∷
      []
    }

  spending : V (1 , ⊤)
  spending = V-apply (V-summarise (V.key salaries) {a>0 = s≤s z≤n}) salaries

  open import Relation.Binary.PropositionalEquality

  _ : spending ≡ record { key = λ{ Fin.zero → tt } ; values = (200 + 300 + 100 + 350 + 400) ∷ [] }
  _ = refl

  spending' : V (1 , ⊤)
  spending' =
    V-apply
      (V-summarise (V.key salaries) {a>0 = s≤s z≤n} V-∘ V-point {n>0 = s≤s z≤n} salaries)
      V-⊤

  _ : spending' ≡ record { key = λ{ Fin.zero → tt } ; values = (200 + 300 + 100 + 350 + 400) ∷ [] }
  _ = refl

  salariesSize : V (1 , ⊤)
  salariesSize =
    V-apply
      (V-summarise (V.key salaries) {a>0 = s≤s z≤n} V-∘ V-members (V.key salaries) {a>0 = s≤s z≤n})
      V-⊤

  _ : salariesSize ≡ record { key = λ{ Fin.zero → tt } ; values = 5 ∷ [] }
  _ = refl
