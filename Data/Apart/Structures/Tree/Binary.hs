module Data.Apart.Structures.Tree.Binary (Binary, insert, height, factor, rotate) where

import Control.Comonad.Cofree (Cofree (..))

type Binary = Cofree Crotch

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
    foldr f acc (Greater g) = f g acc
    foldr f acc (Crotch l g) = f l $ f g acc

instance Traversable Crotch where
    traverse f End = pure End
    traverse f (Less x) = Less <$> f x
    traverse f (Greater x) = Greater <$> f x
    traverse f (Crotch l g) = Crotch <$> f l <*> f g

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

-- the way to the most remote branch
height :: Binary a -> Int
height (a :< End) = 1
height (a :< Less l) = 1 + height l
height (a :< Greater g) = 1 + height g
height (a :< Crotch l g) = 1 + max (height l) (height g)

-- balance factor for root node
factor :: Binary a -> Int
factor (a :< End) = 1
factor (a :< Less l) = 1 + factor l
factor (a :< Greater g) = 1 + factor g
factor (a :< Crotch l g) = abs $ (height l) - (height g)

data Complexity = Simple | Double
data Direction = Leftward | Rightward
data Rotation a = Rotation Complexity Direction (Binary a)

rotate :: Rotation a -> Binary a
rotate (Rotation Simple Leftward (x :< Crotch a (y :< Crotch b c))) = y :< Crotch (x :< Crotch a b) c
rotate (Rotation Simple Rightward (y :< Crotch (x :< Crotch a b) c)) = x :< Crotch a (y :< Crotch b c)
rotate (Rotation Double Leftward (p :< Crotch a (q :< Crotch (s :< Crotch b c) d))) = s :< Crotch (p :< Crotch a b) (q :< Crotch c d)
rotate (Rotation Double Rightward (p :< Crotch (q :< Crotch a (s :< Crotch b c)) d)) = s :< Crotch (q :< Crotch a b) (p :< Crotch c d)
rotate (Rotation _ _ tree) = tree
