{-# LANGUAGE GADTs #-}
{-# options_ghc -Wall #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
module Main where

import Form (Form)
import qualified Form
import Data.Functor.Identity (Identity)
import GHC.Generics (Generic)
import Barbies
import Data.Functor.Contravariant.Barbie (ContraversableB (..))
import Data.Functor.Contravariant.Divisible (divided)
import Data.Functor.Contravariant (contramap)

data PersonT f
  = Person {name :: f String, age :: f Int}
  deriving (Generic, FunctorB, TraversableB)
  
deriving instance (forall x. Show x => Show (f x)) => Show (PersonT f)

instance ContraversableB PersonT where
  bcontraverse f (Person a b) =
    contramap (\(Person ga gb) -> (ga, gb)) (divided (f a) (f b))

personForm :: PersonT Form
personForm =
  Person
  { name = Form.simple "name" Form.string
  , age = Form.simple "age" Form.int
  }


data CoupleT f = Couple { person1 :: f (PersonT Identity), person2 :: f (PersonT Identity) }
  deriving (Generic, FunctorB, TraversableB)

deriving instance (forall x. Show x => Show (f x)) => Show (CoupleT f)

instance ContraversableB CoupleT where
  bcontraverse f (Couple a b) =
    contramap (\(Couple ga gb) -> (ga, gb)) (divided (f a) (f b))

coupleForm :: CoupleT Form
coupleForm =
  Couple
  { person1 = Form.compound "person1" (Form.toForm personForm)
  , person2 = Form.compound "person2" (Form.toForm personForm)
  }


data CoupleT' f = Couple' { person1' :: PersonT f, person2' :: PersonT f }
  deriving (Generic, FunctorB, TraversableB)

deriving instance (forall x. Show x => Show (f x)) => Show (CoupleT' f)

instance ContraversableB CoupleT' where
  bcontraverse f (Couple' a b) =
    contramap
      (\(Couple' ga gb) -> (ga, gb))
      (divided (bcontraverse f a) (bcontraverse f b))

coupleForm' :: CoupleT' Form
coupleForm' =
  Couple'
  { person1' = Form.nested "person1" personForm
  , person2' = Form.nested "person2" personForm
  }


newtype CouplesT f = Couples (f [CoupleT Identity])
  deriving stock (Generic)
  deriving anyclass (FunctorB, TraversableB)

deriving instance (forall x. Show x => Show (f x)) => Show (CouplesT f)

instance ContraversableB CouplesT where
  bcontraverse f (Couples a) = contramap (\(Couples ga) -> ga) (f a)

couplesForm :: CouplesT Form
couplesForm = 
  Couples
    (Form.compound "couples" $ Form.list (Form.toForm coupleForm))

main :: IO ()
main = do
  do
    let 
      c1 =
        Couple'
          (Person (pure "ie") (pure 28))
          (Person (pure "xg") (pure 26))
    let e1 = Form.encode (Form.toForm coupleForm') c1
    print e1

    print $ Form.decode (Form.toForm coupleForm) e1

  putStrLn ""
  
  let c1 = Couple (pure $ Person (pure "ie") (pure 28)) (pure $ Person (pure "xg") (pure 26))

  do
    let e1 = Form.encode (Form.toForm coupleForm) c1
    print e1

    print $ Form.decode (Form.toForm coupleForm) e1
  
  putStrLn ""

  print $
    Form.encode
      (Form.toForm couplesForm) 
      (Couples $ 
        pure 
        [ c1
        , Couple 
            (pure $ Person (pure "jw") (pure 33)) 
            (pure $ Person (pure "jl") (pure 26))
        ])
  
  putStrLn ""

  let
    listListIntForm = Form.list $ Form.list $ Form.simple "item" Form.int
    e2 = Form.encode listListIntForm [[1,2,3],[4,5],[6,7,8,9], [10]]
  print e2

  print $ Form.decode listListIntForm e2
