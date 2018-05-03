module Data.Structure.Tree.Binary (Binary, insert) where

import Control.Comonad.Cofree

type Binary a = Cofree Crotch a

data Crotch a = End | Less a | Greater a | Crotch a a deriving Show

instance Functor Crotch where
    fmap f End = End
    fmap f (Less l) = Less $ f l
    fmap f (Greater r) = Greater $ f r
    fmap f (Crotch l r) = Crotch (f l) (f r)

-- pre-order traversal only
instance Foldable Crotch where
    foldr f acc End = acc
    foldr f acc (Less l) = f l acc
    foldr f acc (Greater r) = f r acc
    foldr f acc (Crotch l r) = f l $ f r acc

insert :: Ord a => Binary a -> a -> Binary a
insert (y :< End) x@((>) y -> True) = y :< Less (x :< End)
insert (y :< End) x@((<) y -> True) = y :< Greater (x :< End)
insert (y :< Less lt) x@((>) y -> True) = y :< Less (insert lt x)
insert (y :< Less lt) x@((<) y -> True) = y :< Crotch lt (x :< End)
insert (y :< Greater gt) x@((>) y -> True) = y :< Crotch (x :< End) gt
insert (y :< Greater gt) x@((<) y -> True) = y :< Greater (insert gt x)
insert (y :< Crotch lt gt) x@((>) y -> True) = y :< Crotch (insert lt x) gt
insert (y :< Crotch lt gt) x@((<) y -> True) = y :< Crotch lt (insert gt x)
insert binary x = binary
