{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module FRP where

import Data.Kind (Type)
import Control.Monad (join)
import Control.Monad.Fix (MonadFix)

data Fun t a = K a | Fun (t -> a)
  deriving (Functor)

instance Applicative (Fun t) where
  pure = K
  K f <*> K a = K (f a)
  K f <*> Fun a = Fun (\t -> f (a t))
  Fun f <*> K a = Fun (\t -> f t a)
  Fun f <*> Fun a = Fun (\t -> f t (a t))

newtype Behaviour m a = Behaviour (Reactive m (Fun Double a))

instance FRP m => Functor (Behaviour m) where
  fmap f (Behaviour r) = Behaviour (fmap (fmap f) r)

instance FRP m => Applicative (Behaviour m) where
  pure = Behaviour . pure . pure
  (<*>) (Behaviour a) (Behaviour b) = Behaviour $ liftA2 (<*>) a b

time :: FRP m => Behaviour m Double
time = Behaviour (pure $ Fun id)

behaviour :: FRP m => (Double -> a) -> Behaviour m a
behaviour f = Behaviour (pure $ Fun f)
  
stepperB :: FRP m => Fun Double a -> Event m (Fun Double a) -> m (Behaviour m a)
stepperB a ea = Behaviour <$> stepperR a ea

switcherB :: FRP m => Behaviour m a -> Event m (Behaviour m a) -> m (Behaviour m a)
switcherB (Behaviour ra) eba = Behaviour <$> switcherR ra (fmap (\(Behaviour x) -> x) eba)

class (MonadFix m, Functor (Event m), Monad (Reactive m)) => FRP m where  
  type Event m :: Type -> Type
  never :: Event m a
  accumE :: a -> Event m (a -> a) -> m (Event m a)
  
  type Reactive m :: Type -> Type
  stepperR :: a -> Event m a -> m (Reactive m a)
  accumR :: a -> Event m (a -> a) -> m (Reactive m a)
  tagR :: Reactive m a -> Event m b -> Event m (a, b)
  
  tagB :: Behaviour m a -> Event m b -> Event m (a, b)
  
switcherR :: FRP m => Reactive m a -> Event m (Reactive m a) -> m (Reactive m a)
switcherR ra era = join <$> stepperR ra era
