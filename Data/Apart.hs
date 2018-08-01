-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Apart
-- Copyright   :  (C) 2018 Murat Kasimov
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Murat Kasimov <iokasimov.m@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Get all your structure and rip it apart.
--
-- The main idea: if you can describe your data structure via Cofree, with apart you can serialize, persistent or hash a segment of your structure!
--
-- A simple introduction to this library can be found here: https://iokasimov.github.io/posts/2018/05/cofree-will-tear-us-apart
----------------------------------------------------------------------------

module Data.Apart (Apart (..)) where

import "base" Data.Bifoldable (Bifoldable (..))
import "base" Data.Bifunctor (Bifunctor (..))
import "base" Data.Bitraversable (Bitraversable (..))
import "free" Control.Comonad.Cofree (Cofree (..))
import "semigroupoids" Data.Functor.Apply (Apply (..))

import Data.Apart.Shape (Shape (..))

-- | Structure with scattered segments.
newtype Apart t raw value = Apart
	{ part :: (Cofree (Shape t raw) value) }

instance Functor t => Functor (Apart t raw) where
	fmap f (Apart structure) = Apart $ f <$> structure

instance Apply t => Apply (Apart t raw) where
	Apart fs <.> Apart structure = Apart $ fs <.> structure

instance Functor t => Bifunctor (Apart t) where
	bimap g f (Apart (x :< Ready values)) = Apart $
		f x :< Ready (part . bimap g f . Apart <$> values)
	bimap g f (Apart (x :< Converted raw)) = Apart $
		f x :< (Converted $ g raw)

instance Foldable t => Bifoldable (Apart t) where
	bifoldr g f acc (Apart (x :< Ready values)) = f x $
		foldr (\st a -> bifoldr g f a $ Apart st) acc values
	bifoldr g f acc (Apart (x :< Converted raw)) = f x $ g raw acc

instance Traversable t => Bitraversable (Apart t) where
	bitraverse g f (Apart (x :< Ready values)) = (<$>) Apart $ (:<) <$> f x <*>
		(Ready <$> traverse ((<$>) part . bitraverse g f . Apart) values)
	bitraverse g f (Apart (x :< Converted raw)) = (<$>) Apart $
		(:<) <$> f x <*> (Converted <$> g raw)
