module Data.Partially (Partially, Shape (..)) where

import Data.Foldable (Foldable (..))
import Data.Bifunctor (Bifunctor (..))
import Data.Bifoldable (Bifoldable (..))
import Data.Bitraversable (Bitraversable (..))
import Control.Comonad.Cofree (Cofree (..))

data Shape t raw value = Ready (t value) | Converted raw

instance Functor t => Functor (Shape t raw) where
    fmap f (Ready values) = Ready $ f <$> values
    fmap f (Converted raw) = Converted raw

instance Foldable t => Foldable (Shape t raw) where
    foldr f acc (Ready values) = foldr f acc values
    foldr f acc (Converted raw) = acc

instance Traversable t => Traversable (Shape t raw) where
    traverse f (Ready values) = Ready <$> traverse f values
    traverse _ (Converted raw) = pure $ Converted raw

instance Functor t => Bifunctor (Shape t) where
    bimap _ f (Ready values) = Ready $ f <$> values
    bimap g _ (Converted raw) = Converted $ g raw

instance Foldable t => Bifoldable (Shape t) where
    bifoldr _ f acc (Ready values) = foldr f acc values
    bifoldr g _ acc (Converted raw) = g raw acc

instance Traversable t => Bitraversable (Shape t) where
    bitraverse _ f (Ready values) = Ready <$> traverse f values
    bitraverse g _ (Converted raw) = Converted <$> g raw

type Partially t raw value = Cofree (Shape t raw) value
