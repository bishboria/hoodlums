{-# language TypeSynonymInstances, FlexibleInstances #-}

-- Trying to create an unfixed structure so you can pretty print
-- some other (recursive) data structure

module Traverse where

data TreeF a r = Node a [r]
            deriving Show

newtype Fix f = Fix (f (Fix f))

type Tree a = Fix (TreeF a)

{-class Pretty a where-}
    {-pretty :: a -> String-}

{-instance Pretty String where-}
    {-pretty s = s-}

{-instance (Pretty a) => Pretty (Tree a) where-}
    {-pretty tree = pprint 0 tree-}
        {-where-}
            {-pprint n (Node name ns) =-}
                {-replicate (n*2) ' ' ++ pretty name ++ "\n" ++ concatMap (pprint (n+1)) ns-}

{-testTree = Node "root" [(Node "one" []), (Node "two" [(Node "three" [])])]-}
