module Main where

import Assignment_01_Monads_et_al
import Lib

a :: Tree Int
a = Node (Leaf 1) (Node (Leaf 2) (Leaf 3))

f :: Tree (Int -> Int)
f = Leaf (+ 1)

main :: IO ()
main = print $ f <*> a
