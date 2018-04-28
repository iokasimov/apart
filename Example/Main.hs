import Control.Comonad.Cofree (Cofree (..))
import Control.Monad (void)
import Data.Foldable (toList)
import Data.Bitraversable (Bitraversable (..))
import Data.Bifunctor (Bifunctor (..))
import Data.Bifoldable (Bifoldable (..))
import Data.Semigroup (Semigroup (..))

import Data.Shape (Shape (..))
import Data.Splitted (Splitted (..), recover, limit)
import Data.Structure.Stack (Stack)

-- part of data structure in some file
partially :: Splitted Maybe FilePath Int
partially = Splitted $ 1 :< Ready (Just $ 2 :< Ready (Just $ 3 :< Converted "Example/piece.txt"))

read_from_file :: FilePath -> IO (Maybe (Cofree Maybe Int))
read_from_file fp = read @(Maybe (Cofree Maybe Int)) <$> readFile fp

-- the whole structure in memory
normally :: Stack Int
normally = 1 :< Just (2 :< Just (3 :< Just (4 :< Just (5 :< Nothing))))

save_to_file :: FilePath -> Maybe (Cofree Maybe Int) -> IO FilePath
save_to_file fp structure = writeFile fp (show structure) *> pure fp

main = do
	print "Splitting data structure based on limit, the rest should be putted in file"
	limit 3 (save_to_file "Example/backup.txt") normally >>= print . toList . unSplitted
	print "Recovering data structure, the rest of structure should be in file"
	recover read_from_file partially >>= print . toList
