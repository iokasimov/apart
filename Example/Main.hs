import Control.Comonad.Cofree (Cofree (..))
import Data.Foldable (toList)

import Data.Apart

-- part of data structure in some file
scattered :: Scattered (Stack Int) FilePath
scattered = Apart $ 1 :< Ready (Just $ 2 :< Ready (Just $ 3 :< Converted "Example/piece.txt"))

read_from_file :: FilePath -> IO (Segment (Stack Int))
read_from_file fp = read @(Segment (Stack Int)) <$> readFile fp

-- the whole structure in memory
inmemory :: Stack Int
inmemory = 1 :< Just (2 :< Just (3 :< Just (4 :< Just (5 :< Nothing))))

save_to_file :: FilePath -> Segment (Stack Int) -> IO FilePath
save_to_file fp structure = writeFile fp (show structure) *> pure fp

main = do
    print "Splitting data structure based on limit, the rest should be putted in file"
    limit 4 (save_to_file "Example/backup.txt") inmemory >>= print . toList . part
    print "Recovering data structure, the rest of structure should be in file"
    recover read_from_file scattered >>= print . toList
    print "Traverse over structure with action, recover segments on the way"
    fluent print read_from_file scattered
