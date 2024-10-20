module File where

import Data.List ()
import Data.Maybe ()

data X = X{ a :: Int, b :: Int, c :: Int }

rewrite1 :: X
rewrite1 = X{ a = 1, b = 2, c = 3 }

rewrite2 :: X
rewrite2 =
  X
  { a = 1
  , b = 2
  , c = 3
  }

data Ignore = Ignore{ a :: Int, b :: Int, c :: Int }

ignore :: X
ignore =
  X
  { a = 1
  , b = 2
  , c = 3
  }

rewrite3 :: X
rewrite3 =
  X
    { a = 1
    , b = 2
    , c = 3
    }

rewrite4 :: X
rewrite4 =
  X{
    a = 1,
    b = 2,
    c = 3
  }
