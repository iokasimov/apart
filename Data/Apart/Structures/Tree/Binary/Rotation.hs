module Data.Apart.Structures.Tree.Binary.Rotation
	( Direction (..), Complexity (..), Rotation (..)
	, factor, rotate ) where

import Control.Comonad.Cofree (Cofree (..))

import Data.Apart.Structures.Tree.Binary.Internal (Binary, Crotch (..), height)

-- balance factor for root node
factor :: Binary a -> Int
factor (a :< End) = 1
factor (a :< Less l) = 1 + factor l
factor (a :< Greater g) = 1 + factor g
factor (a :< Crotch l g) = (height l) - (height g)

data Direction = L | R
data Complexity = I | II
data Rotation = Rotation Complexity Direction

rotate :: Rotation -> Binary a -> Binary a
rotate (Rotation I L) (x :< Crotch a (y :< Crotch b c)) =
	y :< Crotch (x :< Crotch a b) c
rotate (Rotation I R) (y :< Crotch (x :< Crotch a b) c) =
	x :< Crotch a (y :< Crotch b c)
rotate (Rotation II L) (p :< Crotch a (q :< Crotch (s :< Crotch b c) d)) =
	s :< Crotch (p :< Crotch a b) (q :< Crotch c d)
rotate (Rotation II R) (p :< Crotch (q :< Crotch a (s :< Crotch b c)) d) =
	s :< Crotch (q :< Crotch a b) (p :< Crotch c d)
rotate (Rotation _ _) tree = tree
