module Data.Apart.Structures.Stack (Stack, insert, foldaway, final) where

import Control.Comonad.Cofree (Cofree (..), unwrap)
import Data.Functor.Contravariant (Predicate (..))

import Data.Apart.Apart (Segment (..))

-- | Or non-empty list.
type Stack = Cofree Maybe

insert :: a -> Stack a -> Stack a
insert x = (:<) x . Just

-- when I understand how to use partially applied
-- type families correctly, it can be rewritten
-- slightly as natural transformation
foldaway :: Foldable t => t a -> Segment Stack a
foldaway = foldr (\el -> Just . (:<) el) Nothing

final :: Eq a => Predicate (Stack a)
final = Predicate $ \s -> unwrap s == Nothing
