module Data.Apart.Structures.Dictionary (Dictionary, Association (..)) where

import "base" Data.Functor.Compose (Compose)
import "free" Control.Comonad.Cofree (Cofree (..))

type Dictionary = Cofree (Association :.: Maybe)

type (:.:) = Compose

data Association a = Association a

instance Functor Association where
	fmap f (Association x) = Association $ f x

instance Foldable Association where
	foldr f acc (Association x) = f x acc

instance Traversable Association where
	traverse f (Association x) = Association <$> f x
