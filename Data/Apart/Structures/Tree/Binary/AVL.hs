module Data.Apart.Structures.Tree.Binary.AVL (insert) where

import "base" Control.Arrow ((&&&))
import "contravariant" Data.Functor.Contravariant (Predicate (..))
import "contravariant" Data.Functor.Contravariant.Divisible (Divisible (..))
import "semigroupoids" Data.Functor.Bind (Bind (..))

import Data.Apart.Transformations (Segmented (..))

import Data.Apart.Structures.Tree.Binary (Binary, Branches (..), ls, gt, height)
import qualified Data.Apart.Structures.Tree.Binary as Binary (insert)
import Data.Apart.Structures.Tree.Binary.Rotation (Rotate (..), rtt)

-- | Trying rebalance tree after each insert.
insert :: Ord a => a -> Binary a -> Segmented Binary a
insert x tree = balancing $ Binary.insert tree x

balancing :: Binary a -> Segmented Binary a
balancing t@(getPredicate simple_left -> True) = rtt L t
balancing t@(getPredicate simple_right -> True) = rtt R t
balancing t@(getPredicate double_left -> True) = rtt RL t
balancing t@(getPredicate double_right -> True) = rtt LR t

subheight :: Segmented Binary a -> Int
subheight = foldr (\t _ -> height t) 0

simple_left :: Predicate (Binary a)
simple_left = divide (id &&& id)
	gl_LT_or_EQ_gg g_height_diff_2_l

simple_right :: Predicate (Binary a)
simple_right = divide (id &&& id)
	lg_GT_or_EQ_ll l_height_diff_2_g

double_left :: Predicate (Binary a)
double_left = divide (id &&& id)
	gl_LT_or_EQ_gg gl_GT_gg

double_right :: Predicate (Binary a)
double_right = divide (id &&& id)
	lg_GT_or_EQ_ll lg_GT_ll

gl_LT_or_EQ_gg :: Predicate (Binary a)
gl_LT_or_EQ_gg = Predicate $ \t -> (<=)
	(subheight $ gt t >>- ls)
	(subheight $ gt t >>- gt)

lg_GT_or_EQ_ll :: Predicate (Binary a)
lg_GT_or_EQ_ll = Predicate $ \t -> (>=)
	(subheight $ ls t >>- gt)
	(subheight $ ls t >>- ls)

gl_GT_gg :: Predicate (Binary a)
gl_GT_gg = Predicate $ \t -> (>)
	(subheight $ gt t >>- ls)
	(subheight $ gt t >>- gt)

lg_GT_ll :: Predicate (Binary a)
lg_GT_ll = Predicate $ \t -> (>)
	(subheight $ ls t >>- gt)
	(subheight $ ls t >>- ls)

g_height_diff_2_l :: Predicate (Binary a)
g_height_diff_2_l = Predicate $ \t -> (== 2) $ (-)
	(subheight $ gt t) (subheight $ ls t)

l_height_diff_2_g :: Predicate (Binary a)
l_height_diff_2_g = Predicate $ \t -> (== 2) $ (-)
	(subheight $ ls t) (subheight $ gt t)
