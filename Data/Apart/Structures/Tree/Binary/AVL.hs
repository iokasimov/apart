module Data.Apart.Structures.Tree.Binary.AVL (AVL, insert, example, gl_LT_or_EQ_gg) where

import Control.Applicative (Alternative (..))
import Control.Arrow ((&&&))
import Control.Comonad (Comonad (..))
import Control.Comonad.Cofree (Cofree (..))
import Control.Lens ((^?), (<&>))
import Data.Functor.Contravariant (Predicate (..))
import Data.Functor.Contravariant.Divisible (Divisible (..))
import Data.Functor.Bind (Bind (..))
import Data.Semigroup (Semigroup (..))
import Data.Typeable

import Data.Apart.Apart (Segment (..))
import Data.Apart.Structures.Tree.Binary (Binary)
import Data.Apart.Structures.Tree.Binary.Internal (Crotch (..), ls, gt, height)
import qualified Data.Apart.Structures.Tree.Binary.Internal as Binary (insert)

type AVL = Binary

example :: Binary Int
example = 5 :< Crotch (4 :< End)
	(6 :< Crotch (3 :< End) (7 :< End))

insert :: Ord a => a -> AVL a -> Segment AVL a
insert x tree = balancing $ Binary.insert tree x

-- | There is a trick, after each insert we know the last rotate direction
balancing :: Binary a -> Segment AVL a
balancing t@(getPredicate simple_left_condition -> True) = rtt (Rotate I L) t
balancing t@(getPredicate simple_right_condition -> True) = rtt (Rotate I R) t
balancing t@(getPredicate double_left_condition -> True) = rtt (Rotate II L) t
balancing t@(getPredicate double_right_condition -> True) = rtt (Rotate II R) t

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

simple_left_condition :: Predicate (Binary a)
simple_left_condition = divide (id &&& id)
	gl_LT_or_EQ_gg g_height_diff_2_l

simple_right_condition :: Predicate (Binary a)
simple_right_condition = divide (id &&& id)
	lg_GT_or_EQ_ll l_height_diff_2_g

double_left_condition :: Predicate (Binary a)
double_left_condition = divide (id &&& id)
	gl_LT_or_EQ_gg gl_GT_gg

double_right_condition :: Predicate (Binary a)
double_right_condition = divide (id &&& id)
	lg_GT_or_EQ_ll lg_GT_ll

gl_LT_or_EQ_gg :: Predicate (Binary a)
gl_LT_or_EQ_gg = Predicate $ \t -> (<=)
	(foldr (id . const) 0 $ height <$> (gt t >>- ls))
	(foldr (id . const) 0 $ height <$> (gt t >>- gt))

lg_GT_or_EQ_ll :: Predicate (Binary a)
lg_GT_or_EQ_ll = Predicate $ \t -> (>=)
	(foldr (id . const) 0 $ height <$> (ls t >>- gt))
	(foldr (id . const) 0 $ height <$> (ls t >>- ls))

gl_GT_gg :: Predicate (Binary a)
gl_GT_gg = Predicate $ \t -> (>)
	(foldr (id . const) 0 $ height <$> (gt t >>- ls))
	(foldr (id . const) 0 $ height <$> (gt t >>- gt))

lg_GT_ll :: Predicate (Binary a)
lg_GT_ll = Predicate $ \t -> (>)
	(foldr (id . const) 0 $ height <$> (ls t >>- gt))
	(foldr (id . const) 0 $ height <$> (ls t >>- ls))

g_height_diff_2_l :: Predicate (Binary a)
g_height_diff_2_l = Predicate $ \t -> (== 2) $ (-)
	(foldr (id . const) 0 $ height <$> gt t)
	(foldr (id . const) 0 $ height <$> ls t)

l_height_diff_2_g :: Predicate (Binary a)
l_height_diff_2_g = Predicate $ \t -> (== 2) $ (-)
	(foldr (id . const) 0 $ height <$> ls t)
	(foldr (id . const) 0 $ height <$> gt t)
