module Data.Partially (Partially (..), Shape (..)) where

import Control.Comonad.Cofree

data Shape f raw value = Ready (f value) | Converted raw

instance Functor f => Functor (Shape f raw) where
    fmap f (Ready rest) = Ready $ f <$> rest
    fmap f (Converted raw) = Converted raw

instance Foldable f => Foldable (Shape f raw) where
    foldr f acc (Ready rest) = foldr f acc rest
    foldr f acc (Converted raw) = acc

type Partially f raw value = Cofree (Shape f raw) value
