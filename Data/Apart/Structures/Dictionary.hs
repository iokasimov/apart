module Data.Apart.Structures.Dictionary () where

import Control.Comonad.Cofree (Cofree (..))
import Data.Functor.Compose (Compose)

type Dictionary = Cofree (Association :.: Maybe)

type (:.:) = Compose

data Association a = Association a

instance Functor Association where
	fmap f (Association x) = Association $ f x

instance Foldable Association where
	foldr f acc (Association x) = f x acc

instance Traversable Association where
	traverse f (Association x) = Association <$> f x
