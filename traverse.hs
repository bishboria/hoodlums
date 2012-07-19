module Traverse where

data Tree a = Node a [Tree a] deriving Show

pretty :: Show a => Tree a -> String
pretty tree = pprint 0 tree
    where
        pprint n (Node name ns) =
            replicate (n*2) ' ' ++ show name ++ "\n" ++ concatMap (pprint (n+1)) ns

testTree = Node "root" [(Node "one" []), (Node "two" [(Node "three" [])])]
