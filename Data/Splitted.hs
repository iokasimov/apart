module Data.Splitted (Splitted (..), recover, limit) where

import Control.Comonad.Cofree (Cofree (..))
import Data.Bifoldable (Bifoldable (..))
import Data.Bifunctor (Bifunctor (..))
import Data.Bitraversable (Bitraversable (..))
import Data.Foldable (Foldable (..))
import Data.Shape (Shape (..))
import Data.Kind

data Splitted t raw value = Splitted
    { unSplitted :: (Cofree (Shape t raw) value) }

instance Functor t => Functor (Splitted t raw) where
    fmap f (Splitted structure) = Splitted $ f <$> structure

instance Functor t => Bifunctor (Splitted t) where
    bimap g f (Splitted (x :< Ready values)) = Splitted $
        f x :< Ready (unSplitted . bimap g f . Splitted <$> values)
    bimap g f (Splitted (x :< Converted raw)) = Splitted $
        f x :< (Converted $ g raw)

instance Foldable t => Bifoldable (Splitted t) where
    bifoldr g f acc (Splitted (x :< Ready values)) = f x $
        foldr (\st a -> bifoldr g f a $ Splitted st) acc values
    bifoldr g f acc (Splitted (x :< Converted raw)) = f x $ g raw acc

instance Traversable t => Bitraversable (Splitted t) where
    bitraverse g f (Splitted (x :< Ready values)) = (<$>) Splitted $ (:<) <$> f x <*>
        (Ready <$> traverse ((<$>) unSplitted . bitraverse g f . Splitted) values)
    bitraverse g f (Splitted (x :< Converted raw)) = (<$>) Splitted $
        (:<) <$> f x <*> (Converted <$> g raw)

-- do nothing with Ready part, pull back Converted to Ready
recover :: (Traversable t, Applicative g) => (raw -> g (t (Cofree t value)))
	-> Splitted t raw value -> g (Cofree t value)
recover convert (Splitted (x :< Ready values)) = (:<) x <$>
	traverse (recover convert . Splitted) values
recover convert (Splitted (x :< Converted raw)) = (:<) x <$> convert raw

-- keep only a certain number of elements in memory, the rest - write to file
limit :: (Traversable t, Applicative g) => Int -> (t (Cofree t value) -> g raw)
	-> Cofree t value -> g (Splitted t raw value)
limit 0 convert (x :< rest) = (Splitted . (:<) x . Converted) <$> convert rest
limit n convert (x :< rest) = (<$>) (Splitted . (:<) x . Ready) $
	((<$>) . (<$>)) unSplitted $ traverse (limit (n - 1) convert) rest
