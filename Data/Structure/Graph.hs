module Data.Structure.Graph (Graph) where

import Control.Comonad.Cofree

-- directed acyclic graph
type Graph a = Cofree Edge a

data Edge a = Empty | Connect a | Overlay a deriving Show

instance Functor Edge where
    fmap f Empty = Empty
    fmap f (Connect l) = Connect $ f l
    fmap f (Overlay r) = Overlay $ f r

instance Foldable Edge where
    foldr f acc Empty = acc
    foldr f acc (Connect l) = f l acc
    foldr f acc (Overlay r) = f r acc

instance Traversable Edge where
    traverse f Empty = pure Empty
    traverse f (Connect x) = Connect <$> f x
    traverse f (Overlay x) = Overlay <$> f x
