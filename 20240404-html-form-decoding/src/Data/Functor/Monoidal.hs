module Data.Functor.Monoidal where
import Data.Functor.Contravariant (Contravariant)
import Data.Functor.Contravariant.Divisible (Divisible, conquered, divided)

class Monoidal f where
  unit :: f ()
  pair :: f a -> f b -> f (a, b)

newtype DivisibleMonoidal f a = DivisibleMonoidal (f a)
  deriving Contravariant

instance Divisible f => Monoidal (DivisibleMonoidal f) where
  unit = DivisibleMonoidal conquered
  pair (DivisibleMonoidal a) (DivisibleMonoidal b) = DivisibleMonoidal (divided a b)

newtype ApplicativeMonoidal f a = ApplicativeMonoidal (f a)
  deriving Functor

instance Applicative f => Monoidal (ApplicativeMonoidal f) where
  unit = ApplicativeMonoidal (pure ())
  pair (ApplicativeMonoidal a) (ApplicativeMonoidal b) = ApplicativeMonoidal (liftA2 (,) a b)
