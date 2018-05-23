module Data.Apart.Structures.Tree.Binary.Internal
	( Binary, Crotch (..)
	, less, greater, singleton, insert, height, factor) where

import Control.Comonad.Cofree (Cofree (..))
import Control.Lens (Prism', prism')

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

less :: Prism' (Binary a) (Binary a)
less = prism' id $ \case
	(x :< Crotch ls gt) -> Just ls
	(x :< Less ls) -> Just ls
	_ -> Nothing

greater :: Prism' (Binary a) (Binary a)
greater = prism' id $ \case
	(x :< Crotch ls gt) -> Just gt
	(x :< Greater ls) -> Just ls
	_ -> Nothing

singleton :: a -> Binary a
singleton x = x :< End

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
factor (a :< Less l) = (1 + factor l) - 1
factor (a :< Greater g) = (1 + factor g) - 1
factor (a :< Crotch l g) = (height l) - (height g)
