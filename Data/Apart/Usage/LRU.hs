module Data.Apart.Usage.LRU (LRU, cache) where

import Control.Comonad.Cofree (Cofree (..))
import Data.Functor.Alt (Alt (..))
import Data.Functor.Bind (Bind (..))

import Data.Apart.Abilities.Segmented (Segmented (..))
import Data.Apart.Structures.Tree.Binary (Binary, Branches (..))
import Data.Apart.Structures.Tree.Binary.Splay (insert)

{-|
	In this usage example we'll try to implement LRU cache.
	This policy discards the least recently used items first,
	so we need to add priority for the most recently used elements.
	Very resembles behavoir of Splay trees, actually.
-}
type LRU a = Segmented Binary a

-- | Insert sortable value to cache, aftear that, value moved to root
cache :: Ord a => a -> LRU a -> LRU a
cache x lru = (lru >>- insert x) <!> Less (x :< End)
