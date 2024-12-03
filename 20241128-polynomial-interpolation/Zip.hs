module Zip where

zipWithDefault :: a -> (a -> a -> b) -> [a] -> [a] -> [b]
zipWithDefault def f [] [] = []
zipWithDefault def f [] (y:ys) = f def y : zipWithDefault def f [] ys
zipWithDefault def f (x:xs) [] = f x def : zipWithDefault def f xs []
zipWithDefault def f (x:xs) (y:ys) = f x y : zipWithDefault def f xs ys
