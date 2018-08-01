module Data.Apart.Structures.Tree.Binary
	(Binary, Branches (..), ls, gt, singleton, insert, height, factor) where

import "base" Data.Semigroup (Semigroup (..))
import "free" Control.Comonad.Cofree (Cofree (..))
import "semigroupoids" Data.Functor.Apply (Apply (..))
import "semigroupoids" Data.Functor.Alt (Alt (..))
import "semigroupoids" Data.Functor.Bind (Bind (..))

import Data.Apart.Transformations (Segmented (..))

type Binary = Cofree Branches

data Branches a
	= End -- ^ No children
	| Less a -- ^ Left child
	| Greater a -- ^ Right child
	| Branches a a -- ^ Both
	deriving Show

instance Semigroup (Branches a) where
	End <> x = x
	Less x <> Less y = Less x
	Greater x <> Greater y = Greater x
	Less x <> Greater y = Branches x y
	Greater y <> Less x = Branches x y
	Branches x y <> _ = Branches x y
	_ <> End = End

instance Apply Branches where
	End <.> _ = End
	_ <.> End = End
	Less f <.> Less x = Less $ f x
	Less f <.> Greater x = Less $ f x
	Less f <.> Branches x y = Greater $ f y
	Greater f <.> Greater x = Greater $ f x
	Greater f <.> Less x = Greater $ f x
	Greater f <.> Branches x y = Greater $ f x
	Branches f g <.> Less x = Less $ f x
	Branches f g <.> Greater x = Greater $ g x
	Branches f g <.> Branches x y = Branches (f x) (g y)

instance Alt Branches where
	End <!> x = x
	x <!> End = x
	Less x <!> Greater y = Branches x y
	Less x <!> y = y
	Greater y <!> Less x = Branches x y
	Greater y <!> x = x
	Branches x y <!> _ = Branches x y

instance Bind Branches where
	End >>- f = End
	Less x >>- f = f x
	Greater x >>- f = f x
	Branches x y >>- f = f x <> f y

instance Functor Branches where
	fmap f End = End
	fmap f (Less l) = Less $ f l
	fmap f (Greater r) = Greater $ f r
	fmap f (Branches l r) = Branches (f l) (f r)

-- pre-order traversal only
instance Foldable Branches where
	foldr f acc End = acc
	foldr f acc (Less l) = f l acc
	foldr f acc (Greater g) = f g acc
	foldr f acc (Branches l g) = f l $ f g acc

instance Traversable Branches where
	traverse f End = pure End
	traverse f (Less x) = Less <$> f x
	traverse f (Greater x) = Greater <$> f x
	traverse f (Branches l g) = Branches <$> f l <*> f g

-- | Get @x@ from @Branches x y@ or from @Less x@.
ls :: Binary a -> Segmented Binary a
ls (_ :< Less x) = Less x
ls (_ :< Branches x _) = Less x
ls (_ :< _) = End

-- | Get @y@ from @Branches x y@ or from @Greater y@.
gt :: Binary a -> Segmented Binary a
gt (_ :< Greater x) = Greater x
gt (_ :< Branches _ x) = Greater x
gt (_ :< _) = End

singleton :: a -> Binary a
singleton x = x :< End

insert :: Ord a => Binary a -> a -> Binary a
insert (y :< End) x@((>) y -> True) = y :< Less (x :< End)
insert (y :< End) x@((<) y -> True) = y :< Greater (x :< End)
insert (y :< Less lt) x@((>) y -> True) = y :< Less (insert lt x)
insert (y :< Less lt) x@((<) y -> True) = y :< Branches lt (x :< End)
insert (y :< Greater gt) x@((>) y -> True) = y :< Branches (x :< End) gt
insert (y :< Greater gt) x@((<) y -> True) = y :< Greater (insert gt x)
insert (y :< Branches lt gt) x@((>) y -> True) = y :< Branches (insert lt x) gt
insert (y :< Branches lt gt) x@((<) y -> True) = y :< Branches lt (insert gt x)
insert binary x = binary

-- | The way to the most remote branch.
height :: Binary a -> Int
height (a :< End) = 1
height (a :< Less l) = 1 + height l
height (a :< Greater g) = 1 + height g
height (a :< Branches l g) = 1 + max (height l) (height g)

-- | Balance factor for root node.
factor :: Binary a -> Int
factor (a :< End) = 1
factor (a :< Less l) = (1 + factor l) - 1
factor (a :< Greater g) = (1 + factor g) - 1
factor (a :< Branches l g) = (height l) - (height g)
