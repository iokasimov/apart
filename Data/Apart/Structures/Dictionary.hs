module Data.Apart.Structures.Dictionary (Dictionary, Association (..)) where

import "base" Data.Functor.Compose (Compose)
import "free" Control.Comonad.Cofree (Cofree (..))

type Dictionary value key = Cofree (Association value :.: Maybe) key

type (:.:) = Compose

data Association value key = Association value key

instance Functor (Association value) where
	fmap f (Association value key) = Association value $ f key

instance Foldable (Association value) where
	foldr f acc (Association value key) = f key acc

instance Traversable (Association value) where
	traverse f (Association value key) = Association value <$> f key
