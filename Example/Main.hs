import Control.Comonad.Cofree (Cofree (..))
import Control.Monad (void)
import Data.Bifoldable (Bifoldable (..))
import Data.Bifunctor (Bifunctor (..))
import Data.Bitraversable (Bitraversable (..))
import Data.Foldable (toList)
import Data.Semigroup (Semigroup (..))

import Data.Apart (Apart (..), Scattered (..), Segment (..), limit, recover)
import Data.Shape (Shape (..))
import Data.Structure.Stack (Stack)

-- part of data structure in some file
partially :: Scattered (Stack Int) FilePath
partially = Apart $ 1 :< Ready (Just $ 2 :< Ready (Just $ 3 :< Converted "Example/piece.txt"))

read_from_file :: FilePath -> IO (Segment (Stack Int))
read_from_file fp = read @(Segment (Stack Int)) <$> readFile fp

-- the whole structure in memory
normally :: Stack Int
normally = 1 :< Just (2 :< Just (3 :< Just (4 :< Just (5 :< Nothing))))

save_to_file :: FilePath -> Segment (Stack Int) -> IO FilePath
save_to_file fp structure = writeFile fp (show structure) *> pure fp

main = do
	print "Splitting data structure based on limit, the rest should be putted in file"
	limit 3 (save_to_file "Example/backup.txt") normally >>= print . toList . part
	print "Recovering data structure, the rest of structure should be in file"
	recover read_from_file partially >>= print . toList
