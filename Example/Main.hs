import Control.Comonad.Cofree (Cofree (..))

import Data.Partially (Partially (..))
import Data.Shape (Shape (..))
import Data.Bitraversable (bitraverse)
import Data.Bifunctor (Bifunctor (..))
import Data.Bifoldable (Bifoldable (..))
import Data.Semigroup (Semigroup (..))

-- do nothing with Ready part, pull back Converted to Ready
recover :: (Traversable f, Applicative g) => (raw -> g (f (Cofree f a))) -> Partially f raw a -> g (Cofree f a)
recover convert (Partially (x :< Ready values)) = ((:<) x) <$> traverse (recover convert . Partially) values
recover convert (Partially (x :< Converted raw)) = ((:<) x) <$> convert raw

-- part of data structure in some file
partially :: Partially Maybe FilePath Int
partially = Partially $ 1 :< Ready (Just $ 2 :< Ready (Just $ 3 :< Converted "Example/piece.txt"))

read_from_file :: FilePath -> IO (Maybe (Cofree Maybe Int))
read_from_file fp = read @(Maybe (Cofree Maybe Int)) <$> readFile fp

main = recover read_from_file partially >>= traverse (print . (<>) "Ready: " . show)
