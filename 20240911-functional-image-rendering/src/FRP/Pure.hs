{-# language GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeApplications #-}
module FRP.Pure where

import Data.Functor.Identity (Identity)
import FRP (FRP(..), Time)

newtype PureFRP a = PureFRP (Identity a)
  deriving (Functor, Applicative, Monad)

newtype PureEvent a = PureEvent [(Double, a)]
  deriving (Functor)

data PureReactive a = PureReactive a (PureEvent a)
  deriving (Functor)

instance Applicative PureReactive where
  pure a = PureReactive a (never @PureFRP)
  (<*>) (PureReactive f (PureEvent ef)) (PureReactive a (PureEvent ea)) =
    PureReactive (f a) (PureEvent $ _ ef ea)

instance Monad PureReactive where
  (>>=) = _

instance FRP PureFRP where
  type Time PureFRP = Double
  
  type Event PureFRP = PureEvent
  never = PureEvent []
  
  type Reactive PureFRP = PureReactive
  stepperR a ea = pure $ PureReactive a ea
