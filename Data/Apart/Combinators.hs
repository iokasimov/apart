module Data.Apart.Combinators (Restorer, Materializer, recover, limit, fluent) where

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
fluent :: (Traversable t, Monad g) => (value -> g res) -> Restorer g t raw value
	-> (Scattered (Cofree t) value raw) -> g (Cofree t res)
fluent for_value for_raw (Apart (x :< Ready values)) = (:<) <$> for_value x
	<*> (traverse (fluent for_value for_raw . Apart) values)
fluent for_value for_raw (Apart (x :< Converted raw)) = join $
	traverse for_value <$> ((:<) x <$> for_raw raw)
