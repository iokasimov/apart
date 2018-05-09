module Data.Apart.Structures.Stack (Stack, insert, foldaway) where

import Control.Comonad.Cofree (Cofree (..), unwrap)

import Data.Apart.Apart (Segment (..))

type Stack = Cofree Maybe

insert :: a -> Stack a -> Stack a
insert x = (:<) x . Just

-- when I understand how to use partially applied
-- type families correctly, it can be rewritten
-- slightly as natural transformation
foldaway :: Foldable t => t a -> Segment Stack a
foldaway = foldr (\el -> Just . (:<) el) Nothing
