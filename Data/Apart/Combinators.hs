module Data.Apart.Combinators (Restorer, Materializer, recover, limit, throughout, inmemory) where

import Control.Applicative (Alternative (..))
import Control.Comonad.Cofree (Cofree (..))
import Control.Monad (join)

import Data.Apart.Apart (Apart (..), Shape (..), Scattered (..), Segment (..))

-- | Pull back segment of values to memory.
type Restorer g t raw value = (Traversable t, Applicative g) =>
	raw -> g (Segment (Cofree t) value)

-- | Put in-memory values to somewhere else.
type Materializer g t raw value = (Traversable t, Applicative g) =>
	Segment (Cofree t) value -> g raw

-- | Do nothing with in-memory part, pull back all values of structure to memory.
recover :: (Traversable t, Applicative g) => Restorer g t raw value
	-> Scattered (Cofree t) value raw -> g (Cofree t value)
recover convert (Apart (x :< Ready values)) = (:<) x <$>
	traverse (recover convert . Apart) values
recover convert (Apart (x :< Converted raw)) = (:<) x <$> convert raw

-- | Keep only a certain number of elements in memory, do something with the rest.
limit :: (Traversable t, Applicative g) => Int -> Materializer g t raw value
	-> Cofree t value -> g (Scattered (Cofree t) value raw)
limit ((>=) 0 -> True) convert (x :< rest) = error "Limit value should be greater than 0"
limit 1 convert (x :< rest) = (Apart . (:<) x . Converted) <$> convert rest
limit n convert (x :< rest) = (<$>) (Apart . (:<) x . Ready) $
	((<$>) . (<$>)) part $ traverse (limit (n - 1) convert) rest

-- | Traverse over scattered structure, including with all restored segments.
throughout :: (Traversable t, Monad g) => (value -> g result) -> Restorer g t raw value
	-> (Scattered (Cofree t) value raw) -> g (Cofree t result)
throughout f g (Apart (x :< Ready vs)) = (:<) <$> f x <*> (traverse (throughout f g . Apart) vs)
throughout f g (Apart (x :< Converted r)) = join $ traverse f <$> ((:<) x <$> g r)

inmemory :: (Functor t, Alternative t) => Apart t raw value -> Cofree t value
inmemory (Apart (x :< Ready xs)) = (:<) x $ inmemory . Apart <$> xs
inmemory (Apart (x :< Converted _)) = x :< empty
