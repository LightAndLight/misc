{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
module Term where

import Data.Kind (Type)

data Index :: [k] -> k -> Type where
  Z :: Index (x ': xs) x
  S :: Index xs x -> Index (y ': xs) x

deriving instance Show (Index ctx a)

data Term :: [Type] -> Type -> Type where
  Var :: Index ctx a -> Term ctx a  
  Lam :: Term (a ': ctx) b -> Term ctx (a -> b)
  App :: Term ctx (a -> b) -> Term ctx a -> Term ctx b

deriving instance Show (Term ctx a)

rename :: (forall x. Index ctx x -> Index ctx' x) -> Term ctx a -> Term ctx' a
rename f (Var ix) = Var (f ix)
rename f (Lam body) = Lam (rename (\case; Z -> Z; S ix -> S (f ix)) body)
rename f (App a b) = App (rename f a) (rename f b)

class Weaken ctx ctx' where
  weaken :: Index ctx a -> Index ctx' a

instance Weaken xs xs' => Weaken xs (x ': xs') where
  weaken = S . weaken

instance {-# OVERLAPPING #-} Weaken xs xs' => Weaken (x ': xs) (x ': xs') where
  weaken Z = Z
  weaken (S ix) = S (weaken ix)

instance Weaken '[] ctx where
  weaken = \case

type Var ctx a = forall ctx'. Weaken (a ': ctx) ctx' => Term ctx' a

lam ::
  forall ctx a b.
  (Var ctx a -> Term (a ': ctx) b) ->
  Term ctx (a -> b)
lam f = Lam (f (Var (weaken @(a ': ctx) Z)))

app :: Term ctx (a -> b) -> Term ctx a -> Term ctx b
app = App
