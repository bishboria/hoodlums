{-# language TypeSynonymInstances, FlexibleInstances, DeriveDataTypeable #-}

module Traverse where

import Data.Data
import Data.Typeable
import Data.Generics.Aliases

data Tree a = Node a [Tree a]
            deriving (Show, Data, Typeable)

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


-- copied from Hoogle's gshow and then modified to name as we need...
gpretty :: Data a => a -> String
gpretty x = gprettys 0 x ""

-- | Generic shows
gprettys :: Data a => Int -> a -> ShowS

-- -- This is a prefix-show using surrounding "(" and ")",
-- -- where we recurse into subterms with gmapQ.
gprettys n = ( \t ->
               showString (replicate (n*2) ' ')
             . (showString . showConstr . toConstr $ t)
             . showChar '\n'
             . (foldr (.) id . gmapQ (gprettys (n+1)) $ t)
           ) `extQ` ((\s -> (showString $ replicate (n*2) ' ') . shows s . showChar '\n') :: String -> ShowS)
