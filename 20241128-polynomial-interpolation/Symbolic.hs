module Symbolic where
import Data.String (IsString (..))

data Symbolic
  = Var String
  | Integer Integer
  | Negate Symbolic
  | Add Symbolic Symbolic
  | Mul Symbolic Symbolic
  | Recip Symbolic
  | Pow Symbolic Integer
  deriving Eq

instance Show Symbolic where
  showsPrec d (Var s) = showsPrec d s
  showsPrec d (Integer n) = showsPrec d n
  showsPrec d (Negate n) = showString "-" . showsPrec 11 n
  showsPrec d (Add a (Negate b)) =
    showParen (d > 6) $
    showsPrec 6 a . showString " - " . showsPrec 6 b
  showsPrec d (Add a b) =
    showParen (d > 6) $
    showsPrec 6 a . showString " + " . showsPrec 6 b
  showsPrec d (Mul a b) =
    showParen (d > 7) $
    showsPrec 7 a . showString " * " . showsPrec 7 b
  showsPrec d (Recip a) =
    showParen (d > 7) $
    showString "1 / " . showsPrec 7 a
  showsPrec d (Pow a n) =
    showParen (d > 8) $
    showsPrec 8 a . showString "^" . showsPrec 8 n

instance IsString Symbolic where
  fromString = Var

instance Num Symbolic where
  fromInteger = Integer
  
  negate (Integer 0) = Integer 0
  negate (Negate x) = x
  negate (Add a b) = negate a - b
  negate x = Negate x


  (+) (Integer 0) b = b
  (+) a (Integer 0) = a
  (+) a (Add b c) = (a + b) + c
  (+) a (Negate b) | a == b = Integer 0
  (+) (Negate a) b | a == b = Integer 0
  (+) a b = Add a b


  (*) (Integer 0) b = Integer 0
  (*) a (Integer 0) = Integer 0
  (*) (Integer 1) b = b
  (*) a (Integer 1) = a
  (*) a (Mul b c) = (a * b) * c
  (*) a (Add b c) = (a * b) + (a * c)
  (*) (Add a b) c = (a * c) + (b * c)
  (*) a b | a == b = Pow a 2

  (*) (Negate a) b = negate (a * b)
  (*) a (Negate b) = negate (a * b)
  
  (*) (Pow a n) b | a == b = Pow a (n + 1)
  (*) (Mul x (Pow a n)) b | a == b = x * Pow a (n + 1)
  
  (*) (Pow a m) (Pow b n) | a == b = Pow a (m + n)
  (*) (Mul x (Pow a m)) (Pow b n) | a == b = x * Pow a (m + n)

  (*) a (Pow b n) | a == b = Pow a (n + 1)
  (*) (Mul x a) (Pow b n) | a == b = Mul x (Pow a (n + 1))

  (*) a (Recip b) | a == b = Integer 1
  (*) (Recip a) b | a == b = Integer 1
  
  (*) a b = Mul a b

instance Fractional Symbolic where
  recip (Recip x) = x
  recip x = x
