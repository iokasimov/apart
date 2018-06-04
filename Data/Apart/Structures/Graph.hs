module Data.Apart.Structures.Graph (Graph, Edge (..), isolated, star, remove) where

import Control.Comonad.Cofree (Cofree (..), unwrap)
import Control.Comonad (Comonad (..))

import Data.Apart.Apart (Segment (..))

-- | Directed acyclic graph.
type Graph = Cofree Edge

data Edge a = Empty | Single a | Connect a | Overlay a deriving Show

instance Functor Edge where
	fmap f Empty = Empty
	fmap f (Single x) = Single $ f x
	fmap f (Connect x) = Connect $ f x
	fmap f (Overlay x) = Overlay $ f x

instance Foldable Edge where
	foldr f acc Empty = acc
	foldr f acc (Single x) = f x acc
	foldr f acc (Connect x) = f x acc
	foldr f acc (Overlay x) = f x acc

instance Traversable Edge where
	traverse f Empty = pure Empty
	traverse f (Single x) = Connect <$> f x
	traverse f (Connect x) = Connect <$> f x
	traverse f (Overlay x) = Overlay <$> f x

single, connect, overlay, empty :: Segment Graph a -> Segment Graph a
single = foldr (\x _ -> Single x) Empty
connect = foldr (\x _ -> Connect x) Empty
overlay = foldr (\x _ -> Overlay x) Empty
empty = const Empty

isolated :: Foldable t => t a -> Segment Graph a
isolated = foldr (\el -> Overlay . (:<) el) Empty

star :: Foldable t => a -> t a -> Graph a
star x structure = x :< connect (isolated structure)

-- | Remove vertex and all of its edges.
remove :: Eq a => a -> Cofree Edge a -> Edge (Cofree Edge a)
remove x graph@((==) x . extract -> True) = overlay $ unwrap graph
remove x graph@(y :< segment) = ((:<) y . overlay . remove x) <$> segment
