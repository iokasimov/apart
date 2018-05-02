 module Data.Apart (Segment (..), Scattered (..), Apart (..), recover, limit) where

import Control.Comonad.Cofree (Cofree (..))
import Data.Bifoldable (Bifoldable (..))
import Data.Bifunctor (Bifunctor (..))
import Data.Bitraversable (Bitraversable (..))
import Data.Foldable (Foldable (..))
import Data.Kind (Constraint, Type)
import Data.Shape (Shape (..))

data Apart t raw value = Apart
    { part :: (Cofree (Shape t raw) value) }

instance Functor t => Functor (Apart t raw) where
    fmap f (Apart structure) = Apart $ f <$> structure

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

type family Segment (structure :: Type) :: Type where
    Segment (Cofree t value) = t (Cofree t value)

type family Scattered (structure :: Type) (raw :: Type) :: Type where
    Scattered (Cofree t value) raw = Apart t raw value

-- do nothing with Ready part, pull back Converted to Ready
recover :: (Traversable t, Applicative g) => (raw -> g (Segment (Cofree t value)))
	-> Scattered (Cofree t value) raw -> g (Cofree t value)
recover convert (Apart (x :< Ready values)) = (:<) x <$>
	traverse (recover convert . Apart) values
recover convert (Apart (x :< Converted raw)) = (:<) x <$> convert raw

-- keep only a certain number of elements in memory, do something with the rest
limit :: (Traversable t, Applicative g) => Int -> (Segment (Cofree t value) -> g raw)
	-> Cofree t value -> g (Scattered (Cofree t value) raw)
limit 0 convert (x :< rest) = (Apart . (:<) x . Converted) <$> convert rest
limit n convert (x :< rest) = (<$>) (Apart . (:<) x . Ready) $
	((<$>) . (<$>)) part $ traverse (limit (n - 1) convert) rest
