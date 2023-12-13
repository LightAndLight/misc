{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module SPWA.Interact (
  Interact (..),
  stepper,
  stepperM,
  perform,
  request,
  onLoad,
  mkTrigger,
  element,
  textInput,
) where

import Control.Monad.Fix (MonadFix (..))
import Data.Kind (Type)
import SPWA.Behavior (Behavior)
import SPWA.DomEvent (DomEvent)
import SPWA.Element (Element)
import SPWA.Event (Event)
import SPWA.Html (Html)
import SPWA.Reactive (Reactive)
import SPWA.Send (Send)
import SPWA.Session (Session)

data Interact :: Type -> Type where
  Pure :: a -> Interact a
  Apply :: Interact (a -> b) -> Interact a -> Interact b
  Bind :: Interact a -> (a -> Interact b) -> Interact b
  TextInput :: Interact (Behavior String, Element)
  Element :: Html -> Interact Element
  DomEvent :: DomEvent -> Element -> Interact (Event ())
  Perform :: (Send a) => Event a -> (a -> IO ()) -> Interact ()
  Request :: (Send a, Send b) => Event a -> (a -> IO b) -> Interact (Event b)
  Stepper :: (Send a) => a -> Event a -> Interact (Reactive a)
  StepperM :: (Send a) => IO a -> Event a -> Interact (Reactive a)
  MFix :: (a -> Interact a) -> Interact a
  OnLoad :: Session () -> Interact ()
  MkTrigger :: (Send a) => Interact (a -> Session (), Event a)

instance Functor Interact where
  fmap f m = Pure f `Apply` m

instance Applicative Interact where
  pure = Pure
  (<*>) = Apply

instance Monad Interact where
  (>>=) = Bind

instance MonadFix Interact where
  mfix = MFix

stepper :: (Send a) => a -> Event a -> Interact (Reactive a)
stepper = Stepper

stepperM :: (Send a) => IO a -> Event a -> Interact (Reactive a)
stepperM = StepperM

perform :: (Send a) => Event a -> (a -> IO ()) -> Interact ()
perform = Perform

request :: (Send a, Send b) => Event a -> (a -> IO b) -> Interact (Event b)
request = Request

onLoad :: Session () -> Interact ()
onLoad = OnLoad

mkTrigger :: (Send a) => Interact (a -> Session (), Event a)
mkTrigger = MkTrigger

element :: Html -> Interact Element
element = Element

textInput :: Interact (Behavior String, Element)
textInput = TextInput