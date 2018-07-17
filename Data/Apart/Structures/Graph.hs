module Data.Apart.Structures.Graph (Graph, Edges (..), isolated, star, remove) where

import Control.Comonad.Cofree (Cofree (..), unwrap)
import Control.Comonad (Comonad (..))

import Data.Apart.Abilities (Segmented (..))

-- | Directed acyclic graph.
type Graph = Cofree Edges

data Edges a = Empty | Connect a | Overlay a deriving Show

instance Functor Edges where
	fmap f Empty = Empty
	fmap f (Connect x) = Connect $ f x
	fmap f (Overlay x) = Overlay $ f x

instance Foldable Edges where
	foldr f acc Empty = acc
	foldr f acc (Connect x) = f x acc
	foldr f acc (Overlay x) = f x acc

instance Traversable Edges where
	traverse f Empty = pure Empty
	traverse f (Connect x) = Connect <$> f x
	traverse f (Overlay x) = Overlay <$> f x

connect, overlay, empty :: Segmented Graph a -> Segmented Graph a
connect = foldr (\x _ -> Connect x) Empty
overlay = foldr (\x _ -> Overlay x) Empty
empty = const Empty

isolated :: Foldable t => t a -> Segmented Graph a
isolated = foldr (\el -> Overlay . (:<) el) Empty

star :: Foldable t => a -> t a -> Graph a
star x structure = x :< connect (isolated structure)

-- | Remove vertex and all of its edges.
remove :: Eq a => a -> Graph a -> Segmented Graph a
remove x graph@((==) x . extract -> True) = overlay $ unwrap graph
remove x graph@(y :< segment) = ((:<) y . overlay . remove x) <$> segment

-- Take a degree of focused value
degree :: Graph a -> Int
degree (x :< Empty) = 0
degree (x :< Overlay xs) = 0
degree (x :< Connect xs) = length xs
