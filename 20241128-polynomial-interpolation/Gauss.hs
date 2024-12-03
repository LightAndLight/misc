{-# LANGUAGE TypeFamilies #-}
module Gauss where

import Matrix
import Data.Foldable (toList)
import Data.Functor.Classes (eq1, liftEq)

-- | @rowScale i x@ scales row @i@ by @x@.
mrowScale :: Num a => Int -> a -> Matrix a -> Matrix a
mrowScale i x m = m{data_ = fmap (\col -> (i ~ x * col!i) col) (data_ m)}

-- | @rowSub1 i j@ subtracts row @j@ from row @i@.
mrowSub1 :: Num a => Int -> Int -> Matrix a -> Matrix a
mrowSub1 i j = mrowSub i (1, j)

-- | @rowSub i j@ subtracts a multiple of row @j@ from row @i@.
mrowSub :: Num a => Int -> (a, Int) -> Matrix a -> Matrix a
mrowSub i (x, j) m =
  m{data_ = fmap (\col -> (i ~ col!i - x*col!j) col) (data_ m)}

-- | @rowSwap i j@ swaps row @i@ and row @j@.
mrowSwap :: Num a => Int -> Int -> Matrix a -> Matrix a
mrowSwap i j m =
  m{data_ = fmap (\col -> (j ~ col!i) . (i ~ col!j) $ col) (data_ m)}

data Augmented a = Augmented{ augmentee :: Matrix a, augmentation :: Matrix a }
  deriving Show

-- | @rowScale i x@ scales row @i@ by @x@.
rowScale :: Num a => Int -> a -> Augmented a -> Augmented a
rowScale i x (Augmented m1 m2) =
  Augmented (mrowScale i x m1) (mrowScale i x m2)

-- | @rowSub1 i j@ subtracts row @j@ from row @i@.
rowSub1 :: Num a => Int -> Int -> Augmented a -> Augmented a
rowSub1 i j = rowSub i (1, j)

-- | @rowSub i j@ subtracts a multiple of row @j@ from row @i@.
rowSub :: Num a => Int -> (a, Int) -> Augmented a -> Augmented a
rowSub i (x, j) (Augmented m1 m2) =
  Augmented (mrowSub i (x, j) m1) (mrowSub i (x, j) m2)

-- | @rowSwap i j@ swaps row @i@ and row @j@.
rowSwap :: Num a => Int -> Int -> Augmented a -> Augmented a
rowSwap i j (Augmented m1 m2) =
  Augmented (mrowSwap i j m1) (mrowSwap i j m2)

-- | Convert an augmented matrix to row echelon form.
elim :: (Num a, Fractional a, Eq a) => Augmented a -> Augmented a
elim = go (0, 0)
  where
    go (piv_row, piv_col) m
      | piv_row >= rows (augmentee m) || piv_col >= cols (augmentee m) = m
      | otherwise =
        let
          col = augmentee m!piv_col
          x = col ! piv_row
        in
          if x == 0
          then
            let
              nonZeros =
                filter (\(ix, val) -> ix > piv_row && val /= 0) $
                zip [0..] (toList col)
            in
            case nonZeros of
              [] ->
                -- The remainder of this column is zero
                go (piv_row, piv_col + 1) m
              (ix, _) : _ ->
                go (piv_row, piv_col) (rowSwap piv_row ix m)
          else
            let
              m' =
                foldr
                  (\row ->
                    let x' = col!row in
                    if x' == 0
                    then id
                    else rowSub row (x'/x, piv_row)
                  )
                  m
                  [piv_row + 1 .. rows (augmentee m) - 1]
            in
              go (piv_row+1, piv_col+1) m'

-- | Perform back-substitution on an augmented upper-triangular matrix.
subst :: (Fractional a, Eq a) => Augmented a -> Augmented a
subst aug = 
  case nonZeros of
    [] -> aug
    (row, col) : _ -> go (row, col) aug
  where
    nonZeros =
      [ (row, col)
      | row <- [rows (augmentee aug) - 1, rows (augmentee aug) - 2 .. 0]
      , col <- [0..cols (augmentee aug) - 1]
      , augmentee aug!col!row /= 0
      ]
    
    go (piv_row, piv_col) m
      | piv_col == 0 =
          let x = augmentee m!piv_col!piv_row in
          if x == 0
          then m
          else rowScale piv_row (1/x) m
      | otherwise =
          let x = augmentee m!piv_col!piv_row in
          if x == 0
          then
            go (piv_row, piv_col - 1) m
          else
            let
              m' =
                foldr
                  (\row ->
                    let x' = augmentee m!piv_col!row in
                    if x' == 0
                    then id
                    else rowSub row (x', piv_row)
                  )
                  (rowScale piv_row (1/x) m)
                  [piv_row - 1, piv_row - 2 .. 0]
            in
              go (piv_row - 1, piv_col - 1) m'

-- | Convert an augmented matrix to reduced row echelon form.
rref :: (Fractional a, Eq a) => Augmented a -> Augmented a
rref = subst . elim

-- | Invert a (square) matrix using Gauss-Jordan elimination.
invert :: (Fractional a, Eq a) => Matrix a -> Matrix a
invert m =
  let Augmented i m' = rref $ Augmented m (identity (cols m)) in
  if i == identity (cols m)
  then m'
  else error "matrix not invertible"

-- | Solve @Ax = b@ using Gauss-Jordan elimination.
solve ::
  (Fractional a, Ord a) =>
  -- | @A@
  Matrix a ->
  -- | @b@
  Column a ->
  -- | @x@
  Column a
solve a b =
  let Augmented a' x = rref (Augmented a $ Matrix (rows a) 1 [b]) in
  if liftEq (\x y -> abs (x - y) < 1.11e16) a' (identity (cols a))
  then x!0
  else error "linear system does not have a unique solution"
