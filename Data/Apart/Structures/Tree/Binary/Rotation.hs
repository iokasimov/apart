module Data.Apart.Structures.Tree.Binary.Rotation
	(Rotate (..), rtt) where

import Control.Comonad (Comonad (..))
import Control.Comonad.Cofree (Cofree (..))
import Control.Lens ((<&>))
import Data.Functor.Bind (Bind (..))
import Data.Semigroup (Semigroup (..))

import Data.Apart.Apart (Segment (..))
import Data.Apart.Structures.Tree.Binary (Binary, Branches (..), ls, gt, height)

data Rotate
	= L -- ^ Simple left (AVL), left zig (Splay)
	| R -- ^ Simple right (AVL), right zig (Splay)
	| LR -- ^ Double right (AVL), right zig-zag (Splay)
	| RL -- ^ Double left (AVL), left zig-zag (Splay)
	| LL -- ^ Left zig-zig (Splay)
	| RR -- ^ Right zig-zig (Splay)

rtt :: Rotate -> Binary a -> Segment Binary a
rtt L t = (<&>) (extract <$> ls t) $ flip (:<) $ (gt t >>- gt)
	<> (Less $ (extract t) :< (ls t <> (gt t >>- ls)))
rtt R t = (<&>) (extract <$> gt t) $ flip (:<) $ (ls t >>- ls)
	<> (Greater $ (extract t) :< ((ls t >>- gt ) <> gt t))
rtt RL t = gt t >>- rtt L . (:<) (extract t) . (<>) (ls t) . rtt R
rtt LR t = ls t >>- rtt R . (:<) (extract t) . (<>) (gt t) . rtt L
rtt LL t = gt t >>- rtt L . (:<) (extract t) . (<>) (gt t) . rtt L
rtt RR t = ls t >>- rtt R . (:<) (extract t) . (<>) (ls t) . rtt R
