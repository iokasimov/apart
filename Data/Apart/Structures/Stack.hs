module Data.Apart.Structures.Stack
	(Stack, insert, singleton, foldaway, final, latest) where

import "contravariant" Data.Functor.Contravariant (Predicate (..))
import "free" Control.Comonad.Cofree (Cofree (..), unwrap)

import Data.Apart.Transformations (Segmented (..))

-- | Or non-empty list.
type Stack = Cofree Maybe

singleton :: a -> Stack a
singleton x = x :< Nothing

insert :: a -> Stack a -> Stack a
insert x = (:<) x . Just

-- when I understand how to use partially applied
-- type families correctly, it can be rewritten
-- slightly as natural transformation
foldaway :: Foldable t => t a -> Segmented Stack a
foldaway = foldr (\el -> Just . (:<) el) Nothing

final :: Eq a => Predicate (Stack a)
final = Predicate $ \s -> unwrap s == Nothing

latest :: Stack a -> a
latest (x :< Nothing) = x
latest (x :< Just xs) = latest xs
