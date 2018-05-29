module Data.Apart.Structures.Tree.Binary
	(Binary, Crotch (..), ls, gt, singleton, insert, height, factor) where

import Control.Comonad.Cofree (Cofree (..))
import Data.Functor.Apply (Apply (..))
import Data.Functor.Alt (Alt (..))
import Data.Functor.Bind (Bind (..))
import Data.Semigroup (Semigroup (..))

import Data.Apart.Apart (Segment (..))

type Binary = Cofree Crotch

data Crotch a = End | Less a | Greater a | Crotch a a deriving Show

instance Semigroup (Crotch a) where
	End <> x = x
	Less x <> Less y = Less x
	Greater x <> Greater y = Greater x
	Less x <> Greater y = Crotch x y
	Greater y <> Less x = Crotch x y
	Crotch x y <> _ = Crotch x y
	_ <> End = End

instance Apply Crotch where
	End <.> _ = End
	_ <.> End = End
	Less f <.> Less x = Less $ f x
	Less f <.> Greater x = Less $ f x
	Less f <.> Crotch x y = Greater $ f y
	Greater f <.> Greater x = Greater $ f x
	Greater f <.> Less x = Greater $ f x
	Greater f <.> Crotch x y = Greater $ f x
	Crotch f g <.> Less x = Less $ f x
	Crotch f g <.> Greater x = Greater $ g x
	Crotch f g <.> Crotch x y = Crotch (f x) (g y)

instance Alt Crotch where
	End <!> x = x
	x <!> End = x
	Less x <!> Greater y = Crotch x y
	Less x <!> y = y
	Greater y <!> Less x = Crotch x y
	Greater y <!> x = x
	Crotch x y <!> _ = Crotch x y

instance Bind Crotch where
	End >>- f = End
	Less x >>- f = f x
	Greater x >>- f = f x
	Crotch x y >>- f = f x <> f y

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

ls :: Binary a -> Segment Binary a
ls (_ :< Less x) = Less x
ls (_ :< Crotch x _) = Less x
ls (_ :< _) = End

gt :: Binary a -> Segment Binary a
gt (_ :< Greater x) = Greater x
gt (_ :< Crotch _ x) = Greater x
gt (_ :< _) = End

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
