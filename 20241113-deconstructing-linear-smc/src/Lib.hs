{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Lib where

import Data.Kind (Type)
import Control.Category (Category(..))
import Prelude hiding (id, (.), abs)
import Data.Functor.Identity (Identity (..))
import Data.List (intercalate)

data HList (f :: k -> Type) :: [k] -> Type where
  Nil :: HList f '[]
  Cons :: f x -> HList f xs -> HList f (x ': xs)

deriving instance (forall x. Show (f x)) => Show (HList f xs)

mapHList :: (forall x. f x -> g x) -> HList f xs -> HList g xs
mapHList _ Nil = Nil
mapHList f (Cons x xs) = Cons (f x) (mapHList f xs)

foldMapHList :: Monoid m => (forall x. f x -> m) -> HList f xs -> m
foldMapHList _ Nil = mempty
foldMapHList f (Cons x xs) = f x <> foldMapHList f xs

indexHList :: Index xs x -> HList f xs -> f x
indexHList Z (Cons x _) = x
indexHList (S ix) (Cons _ xs) = indexHList ix xs

data Index :: [k] -> k -> Type where
  Z :: Index (x ': xs) x
  S :: Index xs x -> Index (y ': xs) x

deriving instance Show (Index xs x)

indexToInt :: Index xs x -> Int
indexToInt Z = 0
indexToInt (S ix) = 1 + indexToInt ix

class Category arr => Cartesian (arr :: k -> k -> Type) where
  type Product arr :: [k] -> k
  tuple :: HList (arr x) xs -> arr x (Product arr xs)
  index :: Index xs a -> arr (Product arr xs) a
  
  type Exponential arr :: k -> k -> k
  abstract :: arr (Product arr (a ': xs)) b -> arr (Product arr xs) (Exponential arr a b)
  apply :: arr (Product arr '[Exponential arr a b, a]) b

type Unit arr = Product arr '[]

terminal :: Cartesian arr => arr x (Unit arr)
terminal = tuple Nil

first :: Cartesian arr => arr (Product arr (x ': xs)) x
first = index Z

second :: (Cartesian arr, Context xs) => arr (Product arr (x ': xs)) (Product arr xs)
second = tuple $ mapHList index (indices S)

pair :: (Cartesian arr, Context ys) => arr x y -> arr x (Product arr ys) -> arr x (Product arr (y ': ys))
pair y ys = tuple (Cons y (mapHList (\ix -> index ix . ys) $ indices id))

instance Cartesian (->) where
  type Product (->) = HList Identity
  
  tuple Nil _ = Nil
  tuple (Cons f fs) x = Cons (Identity $ f x) (tuple fs x)
  
  index Z (Cons x _) = runIdentity x
  index (S ix) (Cons _ xs) = index ix xs

  type Exponential (->) = (->)
  abstract f x a = f (Cons (Identity a) x)
  apply (Cons f (Cons x Nil)) = runIdentity f (runIdentity x)

data CCC (arr :: k -> k -> Type) (a :: k) (b :: k) where
  Embed :: !(arr a b) -> CCC arr a b
  Id :: CCC arr a a
  Compose :: CCC arr b c -> CCC arr a b -> CCC arr a c
  Tuple :: HList (CCC arr x) xs -> CCC arr x (Product arr xs)
  Index :: Index xs x -> CCC arr (Product arr xs) x
  Abstract :: CCC arr (Product arr (a ': xs)) b -> CCC arr (Product arr xs) (Exponential arr a b)
  Apply :: CCC arr (Product arr '[Exponential arr a b, a]) b

deriving instance (forall x y. Show (arr x y)) => Show (CCC arr a b)

composeC :: CCC arr b c -> CCC arr a b -> CCC arr a c
composeC f Id = f
composeC Id g = g
composeC (Index ix) (Tuple fs) = indexHList ix fs
composeC f g = Compose f g

instance Category (CCC arr) where
  id = Id
  (.) = composeC

instance Cartesian (CCC arr) where
  type Product (CCC arr) = Product arr
  tuple = Tuple
  index = Index

  type Exponential (CCC arr) = Exponential arr
  abstract = Abstract
  apply = Apply

runCCC :: Cartesian arr => CCC arr a b -> arr a b
runCCC (Embed arr) = arr
runCCC Id = id
runCCC (Compose f g) = runCCC f . runCCC g
runCCC (Tuple fs) = tuple (mapHList runCCC fs)
runCCC (Index ix) = index ix
runCCC (Abstract f) = abstract (runCCC f)
runCCC Apply = apply

renderCCC :: (forall x y. arr x y -> String) -> CCC arr a b -> String
renderCCC renderArr (Embed arr) = renderArr arr
renderCCC _ Id = "id"
renderCCC renderArr (Compose f g) = renderCCC renderArr f <> " ∘ " <> renderCCC renderArr g
renderCCC renderArr (Tuple fs) = "⟨" <> intercalate "; " (foldMapHList (pure . renderCCC renderArr) fs) <> "⟩"
renderCCC _ (Index ix) = "ix(" <> show (indexToInt ix) <> ")"
renderCCC renderArr (Abstract f) = "λ(" <> renderCCC renderArr f <> ")"
renderCCC _ Apply = "app"

data NoEmbed (a :: k) (b :: k)

renderNoEmbed :: NoEmbed a b -> String
renderNoEmbed = \case

newtype Term arr ctx a = Term { unTerm :: CCC arr (Product arr ctx) a }

class Context (xs :: [k]) where
  indices :: (forall x. Index xs x -> Index xs' x) -> HList (Index xs') xs

instance Context '[] where
  indices _ = Nil

instance Context xs => Context (x ': xs) where
  indices f = Cons (f Z) (indices (f . S))

fromProduct :: forall xs arr r. Context xs => Term arr r (Product arr xs) -> HList (Term arr r) xs
fromProduct (Term f) = mapHList (\ix -> Term $ Index ix `composeC` f) (indices @_k @xs id)

toProduct :: HList (Term arr r) xs -> Term arr r (Product arr xs)
toProduct = Term . Tuple . mapHList unTerm

fromExponential :: Term arr r (Exponential arr a b) -> Term arr r a -> Term arr r b
fromExponential (Term f) (Term x) = Term $ Apply `composeC` Tuple (Cons f $ Cons x Nil)

toExponential :: forall arr r a b. (forall s. Term arr s a -> Term arr s b) -> Term arr r (Exponential arr a b)
toExponential f = Term $ Abstract (unTerm $ f (Term $ Index Z)) `composeC` Tuple (Cons Id Nil)

class Weaken (ctx :: [k]) (ctx' :: [k]) where
  weaken :: Term arr ctx a -> Term arr ctx' a

instance Weaken ctx ctx where
  weaken = id

instance {-# OVERLAPPING #-} (Context ctx) => Weaken ctx (x ': ctx) where
  weaken (Term f) = Term $ f `composeC` second

app :: Term arr r (Exponential arr a b) -> Term arr r a -> Term arr r b
app = fromExponential

type Var arr ctx a = forall ctx'. Weaken (a ': ctx) ctx' => Term arr ctx' a

lam :: forall arr ctx a b. (Var arr ctx a -> Term arr (a ': ctx) b) -> Term arr ctx (Exponential arr a b)
lam f = Term . Abstract $ unTerm (f go)
  where
    go :: forall ctx'. Weaken (a ': ctx) ctx' => Term arr ctx' a
    go = weaken @_ @(a ': ctx) @ctx' (Term first)

eval :: Cartesian arr => (forall r. Context r => Term arr r a -> Term arr r b) -> arr (Product arr '[a]) b
eval f = runCCC $ unTerm (f $ Term (Index Z))

{-# COMPLETE Pair #-}
pattern Pair :: Term arr r a -> Term arr r b -> Term arr r (Product arr '[a, b])
pattern Pair a b <- (fromProduct -> Cons a (Cons b Nil))
  where
    Pair a b = toProduct $ Cons a (Cons b Nil)

{-# COMPLETE Unit #-}
pattern Unit :: Term arr r (Product arr '[])
pattern Unit <- (fromProduct -> Nil)
  where
    Unit = toProduct Nil

data Nat = Zero | Succ Nat

data SNat :: Nat -> Type where
  SZero :: SNat 'Zero
  SSucc :: SNat n -> SNat ('Succ n)

deriving instance Show (SNat n)

type family Replicate (n :: Nat) (x :: a) :: [a] where
  Replicate 'Zero a = '[]
  Replicate ('Succ n) a = a ': Replicate n a

data HList2 :: (k -> k -> Type) -> [k] -> [k] -> Type where
  Nil2 :: HList2 f '[] '[]
  Cons2 :: f x y -> HList2 f xs ys -> HList2 f (x ': xs) (y ': ys)

data CCC' (arr :: k -> k -> Type) (a :: k) (b :: k) where
  Embed' :: !(arr a b) -> CCC' arr a b
  Id' :: CCC' arr a a
  Compose' :: CCC' arr b c -> CCC' arr a b -> CCC' arr a c
  Dup' :: SNat n -> CCC' arr x (Product arr (Replicate n x))
  Map' :: HList2 arr xs ys -> CCC' arr (Product arr xs) (Product arr ys)
  Abstract' :: CCC' arr (Product arr (a ': xs)) b -> CCC' arr (Product arr xs) (Exponential arr a b)
  Apply' :: CCC' arr (Product arr '[Exponential arr a b, a]) b

deriving instance (forall x y. Show (arr x y)) => Show (CCC' arr a b)
