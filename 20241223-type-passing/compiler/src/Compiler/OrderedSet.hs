module Compiler.OrderedSet
  ( OrderedSet
  , toList
  , toSet
  , empty
  , null
  , singleton
  , snoc
  , intersect
  , minus
  )
where

import Data.DList (DList)
import qualified Data.DList as DList
import Data.Foldable (foldl')
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.Hashable (Hashable)
import Prelude hiding (null)

data OrderedSet a = OrderedSet (HashSet a) (DList a)

instance Hashable a => Semigroup (OrderedSet a) where
  set <> OrderedSet _ items = foldl' snoc set items

instance Hashable a => Monoid (OrderedSet a) where
  mempty = empty

toList :: OrderedSet a -> [a]
toList (OrderedSet _ items) = DList.toList items

toSet :: OrderedSet a -> HashSet a
toSet (OrderedSet members _) = members

empty :: OrderedSet a
empty = OrderedSet HashSet.empty []

null :: OrderedSet a -> Bool
null (OrderedSet members _) = HashSet.null members

singleton :: Hashable a => a -> OrderedSet a
singleton a = OrderedSet (HashSet.singleton a) [a]

snoc :: Hashable a => OrderedSet a -> a -> OrderedSet a
snoc old@(OrderedSet seen items) a
  | HashSet.member a seen = old
  | otherwise = OrderedSet (HashSet.insert a seen) (DList.snoc items a)

intersect :: Hashable a => OrderedSet a -> OrderedSet a -> OrderedSet a
intersect (OrderedSet _seen items) (OrderedSet seen' _items') =
  foldl'
    ( \acc item ->
        if HashSet.member item seen'
          then snoc acc item
          else acc
    )
    empty
    (DList.toList items)

minus :: Hashable a => OrderedSet a -> OrderedSet a -> OrderedSet a
minus (OrderedSet _seen items) (OrderedSet seen' _items') =
  foldl'
    ( \acc item ->
        if HashSet.member item seen'
          then acc
          else snoc acc item
    )
    empty
    (DList.toList items)
