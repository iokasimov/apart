module Data.Tree.Prefix (Prefix) where

import Control.Comonad.Cofree

import Data.Tree (Tree)

type Prefix s a = Tree (Labeled s) a

data Labeled s a = forall t . Traversable t => Hop s (t a)

instance Functor (Labeled s) where
    fmap f (Hop s as) = Hop s $ f <$> as

instance Foldable (Labeled s) where
    foldr f acc (Hop s as) = foldr f acc as

instance Traversable (Labeled s) where
    traverse f (Hop s as) = Hop s <$> traverse f as
