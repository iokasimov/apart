module Data.Tree.Binary (Binary) where

import Control.Comonad.Cofree

import Data.Tree (Tree)

type Binary a = Tree Crotch a

data Crotch a = End | Less a | Greater a | Crotch a a

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
