module Data.Apart.Structures.Tree.Binary.AVL (AVL, insert) where

import Control.Arrow ((&&&))
import Data.Functor.Contravariant (Predicate (..))
import Data.Functor.Contravariant.Divisible (Divisible (..))
import Data.Functor.Bind (Bind (..))

import Data.Apart.Apart (Segment (..))
import Data.Apart.Structures.Tree.Binary (Binary, Branches (..), ls, gt, height)
import qualified Data.Apart.Structures.Tree.Binary as Binary (insert)
import Data.Apart.Structures.Tree.Binary.Rotation (Rotate (..), rtt)

type AVL = Binary

insert :: Ord a => a -> AVL a -> Segment AVL a
insert x tree = balancing $ Binary.insert tree x

balancing :: Binary a -> Segment AVL a
balancing t@(getPredicate simple_left -> True) = rtt L t
balancing t@(getPredicate simple_right -> True) = rtt R t
balancing t@(getPredicate double_left -> True) = rtt RL t
balancing t@(getPredicate double_right -> True) = rtt LR t

subheight :: Segment Binary a -> Int
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
