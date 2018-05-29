module Data.Apart.Structures.Tree.Binary.Rotation
	(Rotate (..), Direction (..), Complexity (..), rtt) where

import Control.Comonad (Comonad (..))
import Control.Comonad.Cofree (Cofree (..))
import Control.Lens ((<&>))
import Data.Functor.Bind (Bind (..))
import Data.Semigroup (Semigroup (..))

import Data.Apart.Apart (Segment (..))
import Data.Apart.Structures.Tree.Binary (Binary, Crotch (..), ls, gt, height)

data Direction = L | R
data Complexity = I | II
data Rotate = Rotate Complexity Direction

rtt :: Rotate -> Binary a -> Segment Binary a
rtt (Rotate I L) t = (<&>) (extract <$> ls t) $ flip (:<)
	$ (Less $ (extract t) :< (ls t <> (gt t >>- ls))) <> (gt t >>- gt)
rtt (Rotate I R) t = (<&>) (extract <$> gt t) $ flip (:<) $
	(Greater $ (extract t) :< ((ls t >>- gt ) <> gt t)) <> (ls t >>- ls)
rtt (Rotate II L) t = gt t >>- rtt (Rotate I L) .
	(:<) (extract t) . (<>) (ls t) . rtt (Rotate I R)
rtt (Rotate II R) t = ls t >>- rtt (Rotate I R) .
	(:<) (extract t) . (<>) (gt t) . rtt (Rotate I L)
