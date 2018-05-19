module Data.Apart.Structures.Tree.Binary.AVL
	(AVL, insert) where

import Control.Comonad.Cofree (Cofree (..))

import Data.Apart.Structures.Tree.Binary (Binary)
import Data.Apart.Structures.Tree.Binary.Rotation
	(Direction (..), Complexity (..), Rotation (..), factor, rotate)
import qualified Data.Apart.Structures.Tree.Binary.Internal as Binary (insert)

type AVL = Binary

insert :: Ord a => a -> AVL a -> AVL a
insert x tree = balancing $ Binary.insert tree x

balancing :: Binary a -> AVL a
balancing tree@(factor -> -2) = rotate (Rotation II L) tree
balancing tree@(factor -> 2) = rotate (Rotation II R) tree
balancing tree = tree
