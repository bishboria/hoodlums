{-# language TypeSynonymInstances, FlexibleInstances #-}

module Traverse where

data Tree a = Node a [Tree a]
            deriving Show

class Pretty a where
    pretty :: a -> String

instance Pretty String where
    pretty s = s

instance (Pretty a) => Pretty (Tree a) where
    pretty tree = pprint 0 tree
        where
            pprint n (Node name ns) =
                replicate (n*2) ' ' ++ pretty name ++ "\n" ++ concatMap (pprint (n+1)) ns

testTree = Node "root" [(Node "one" []), (Node "two" [(Node "three" [])])]
