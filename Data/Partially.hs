module Data.Partially (Partially (..)) where

import Data.Foldable (Foldable (..))
import Data.Bifunctor (Bifunctor (..))
import Data.Bifoldable (Bifoldable (..))
import Data.Bitraversable (Bitraversable (..))
import Data.Shape (Shape (..))
import Control.Comonad.Cofree (Cofree (..))

data Partially t raw value = Partially
    { unPartially :: (Cofree (Shape t raw) value) }

instance Functor t => Functor (Partially t raw) where
    fmap f (Partially structure) = Partially $ f <$> structure

instance Functor t => Bifunctor (Partially t) where
    bimap g f (Partially (x :< Ready values)) = Partially $
        f x :< Ready (unPartially . bimap g f . Partially <$> values)
    bimap g f (Partially (x :< Converted raw)) = Partially $
        f x :< (Converted $ g raw)

instance Foldable t => Bifoldable (Partially t) where
    bifoldr g f acc (Partially (x :< Ready values)) = f x $
        foldr (\st a -> bifoldr g f a $ Partially st) acc values
    bifoldr g f acc (Partially (x :< Converted raw)) = f x $ g raw acc

instance Traversable t => Bitraversable (Partially t) where
    bitraverse g f (Partially (x :< Ready values)) = (<$>) Partially $ (:<) <$> f x <*>
        (Ready <$> traverse ((<$>) unPartially . bitraverse g f . Partially) values)
    bitraverse g f (Partially (x :< Converted raw)) = (<$>) Partially $
        (:<) <$> f x <*> (Converted <$> g raw)
