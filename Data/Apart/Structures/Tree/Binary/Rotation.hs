module Data.Apart.Structures.Tree.Binary.Rotation
	(Rotate (..), rtt) where

import Control.Comonad (Comonad (..))
import Control.Comonad.Cofree (Cofree (..))
import Control.Lens ((<&>))
import Data.Functor.Bind (Bind (..))
import Data.Semigroup (Semigroup (..))

import Data.Apart.Apart (Segment (..))
import Data.Apart.Structures.Tree.Binary (Binary, Crotch (..), ls, gt, height)

data Rotate = L | R | LL | RR | LR | RL

rtt :: Rotate -> Binary a -> Segment Binary a
rtt L t = (<&>) (extract <$> ls t) $ flip (:<) $ (gt t >>- gt)
	<> (Less $ (extract t) :< (ls t <> (gt t >>- ls)))
rtt R t = (<&>) (extract <$> gt t) $ flip (:<) $ (ls t >>- ls)
	<> (Greater $ (extract t) :< ((ls t >>- gt ) <> gt t))
rtt LL t = gt t >>- rtt L . (:<) (extract t) . (<>) (gt t) . rtt L
rtt RR t = ls t >>- rtt R . (:<) (extract t) . (<>) (ls t) . rtt R
rtt RL t = gt t >>- rtt L . (:<) (extract t) . (<>) (ls t) . rtt R
rtt LR t = ls t >>- rtt R . (:<) (extract t) . (<>) (gt t) . rtt L
