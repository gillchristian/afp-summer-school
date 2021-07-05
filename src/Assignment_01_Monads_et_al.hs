{-# LANGUAGE InstanceSigs #-}

module Assignment_01_Monads_et_al where

data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving (Show, Eq)

instance Functor Tree where
  fmap :: (a -> b) -> Tree a -> Tree b
  fmap f (Leaf a) = Leaf $ f a
  fmap f (Node l r) = Node (fmap f l) (fmap f r)

instance Applicative Tree where
  pure :: a -> Tree a
  pure = Leaf

  (<*>) :: Tree (a -> b) -> Tree a -> Tree b
  Leaf f <*> Leaf a = Leaf $ f a
  Leaf f <*> Node r l = Node (fmap f r) (fmap f l)
  Node lf rf <*> a = Node (lf <*> a) (rf <*> a)

instance Monad Tree where
  (>>=) :: Tree a -> (a -> Tree b) -> Tree b
  Leaf a >>= f = f a
  Node l r >>= f = Node (l >>= f) (r >>= f)
