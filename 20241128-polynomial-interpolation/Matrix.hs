{-# LANGUAGE TypeFamilies #-}
module Matrix where

import GHC.Stack (HasCallStack)
import Data.Kind (Type)
import Data.Maybe (mapMaybe)
import Data.List (intercalate)

import Zip (zipWithDefault)
import Data.Functor.Classes (Eq1 (..), eq1)

newtype Column a = Column [a]
  deriving (Eq, Eq1, Show, Foldable)

scaleColumn :: Num a => a -> Column a -> Column a
scaleColumn s (Column xs) = Column (fmap (s *) xs)

zeroColumn :: Num a => Int -> Column a
zeroColumn n = Column $ replicate n 0

addColumn :: Num a => Column a -> Column a -> Column a
addColumn (Column xs) (Column ys) = Column (zipWithDefault 0 (+) xs ys)

data Matrix a = Matrix{ rows :: !Int, cols :: !Int, data_ :: ![Column a] }

instance Eq1 Matrix where
  liftEq f (Matrix m n xs) (Matrix m' n' xs') =
    m == m' &&
    n == n' &&
    (liftEq . liftEq) f xs xs'

instance Eq a => Eq (Matrix a) where (==) = eq1

instance Show a => Show (Matrix a) where
  showsPrec d matrix@(Matrix m n xs) =
    showParen (d > 5) $
      showString (show (m, n)) . showString " #\n" .
      showString
        (let
          xs' = fmap show (rowOrder matrix)
        in
          showRowOrder (colWidths n xs') 0 xs'
        )
    where
      rowOrder :: Matrix a -> [a]
      rowOrder (Matrix _ _ xs) = go xs
        where
          go [] = []
          go ys =
            let
              (row, ys') =
                unzip $
                mapMaybe
                  (\(Column els) ->
                    case els of
                      [] -> Nothing
                      el : els' -> Just (el, Column els'))
                  ys
            in
              row ++ go ys'

      colWidths :: Int -> [String] -> [Int]
      colWidths n = go (replicate n minBound)
        where
          go acc [] =
            acc
          go acc ys =
            let (row, rest) = splitAt n ys in
            go (zipWith max (fmap length row) acc) rest
      
      padTo :: String -> Int -> String
      padTo "" 0 = ""
      padTo "" n = ' ' : padTo "" (n-1)
      padTo (x:xs) 0 = undefined
      padTo (x:xs) n = x : padTo xs (n-1)

      showRowOrder :: [Int] -> Int -> [String] -> String
      showRowOrder ws i ys
        | i >= m = "  ]"
        | otherwise =
            let (row, rest) = splitAt n ys in
            if i == 0
            then
              "  [ " <> intercalate ", " (zipWith padTo row ws) <> "\n" <>
              showRowOrder ws (i+1) rest
            else
              "  , " <> intercalate ", " (zipWith padTo row ws) <> "\n" <>
              showRowOrder ws (i+1) rest

(#) :: (Int, Int) -> [a] -> Matrix a
(#) (m, n) xs =
  Matrix m n $
  foldr
    (zipWith (\x (Column xs) -> Column (x : xs)))
    (replicate n (Column []))
    (toRows xs)
  where
    toRows ys =
      let (prefix, suffix) = splitAt n ys in
      prefix :
      case suffix of
        [] -> []
        _ -> toRows suffix

infix 5 #

scaleMatrix :: Num a => a -> Matrix a -> Matrix a
scaleMatrix s (Matrix m n xs) = Matrix m n (fmap (scaleColumn s) xs)

zeroMatrix :: Num a => (Int, Int) -> Matrix a
zeroMatrix (m, n) = (m, n) # replicate (m * n) 0

addMatrix :: Num a => Matrix a -> Matrix a -> Matrix a
addMatrix (Matrix m n xs) (Matrix m' n' xs') =
  Matrix (max m m') (max n n') $
  zipWithDefault (Column $ replicate (max m m') 0) addColumn xs xs'

-- | The identity linear map (identity matrix).
identity :: Num a => Int -> Matrix a
identity n = let cs = go n in Matrix n n cs
  where
    go 1 = [Column [1]]
    go n' =
      let cs = go (n' - 1) in
      Column (1 : replicate (n' - 1) 0) : fmap (\(Column c) -> Column (0 : c)) cs

-- | Right-to-left composition of linear maps (matrix multiplication).
compose :: Num a => Matrix a -> Matrix a -> Matrix a
compose (Matrix m n xs) (Matrix n' o xs')
  | n == n' =
      Matrix m o $
        fmap
          (\(Column els) -> foldr addColumn (zeroColumn m) $ zipWith scaleColumn els xs)
          xs'
  | otherwise =
    error $
    "number of columns in matrix 1 doesn't match the number of rows in matrix 2 " ++
    "(" ++ show n ++ " /= " ++ show n' ++ ")"

class Index a where
  type Element a :: Type
  (!) :: HasCallStack => a -> Int -> Element a
  (~) :: Int -> Element a -> a -> a

infixl 8 !
infix 5 ~

instance Index (Matrix a) where
  type Element (Matrix a) = Column a
  (!) (Matrix _ _ cs) n = cs !! n
  (~) n col m =
    let (prefix, suffix) = splitAt n (data_ m) in
    let suffix' = col : drop 1 suffix in
    m{data_ = prefix <> suffix'}

instance Index (Column a) where
  type Element (Column a) = a
  (!) (Column xs) n = xs !! n
  (~) n x (Column xs) =
    let (prefix, suffix) = splitAt n xs in
    let suffix' = x : drop 1 suffix in
    Column $ prefix <> suffix'
