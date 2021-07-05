{-# LANGUAGE InstanceSigs #-}

import Assignment_01_Monads_et_al
import Data.Proxy
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Classes

-- https://stackoverflow.com/questions/65775588/how-does-an-instance-of-arbitrary-looks-for-a-tree
instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary :: Gen (Tree a)
  -- arbitrary =
  --   frequency
  --     [ (3, Leaf <$> arbitrary),
  --       (1, Node <$> arbitrary <*> arbitrary)
  --     ]
  arbitrary = sized go
    where
      go 0 = Leaf <$> arbitrary
      go n = oneof [Leaf <$> arbitrary, Node <$> go' <*> go']
        where
          go' = go (n -1)

proxyTree :: Proxy Tree
proxyTree = Proxy

main :: IO ()
main = do
  lawsCheck $ functorLaws proxyTree
  lawsCheck $ monadLaws proxyTree
  lawsCheck $ applicativeLaws proxyTree
