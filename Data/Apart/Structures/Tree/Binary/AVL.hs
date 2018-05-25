module Data.Apart.Structures.Tree.Binary.AVL (AVL, insert) where

import Control.Applicative (Alternative (..))
import Control.Arrow ((&&&))
import Control.Comonad.Cofree (Cofree (..))
import Control.Lens ((^?))
import Data.Functor.Contravariant (Predicate (..))
import Data.Functor.Contravariant.Divisible (Divisible (..))

import Data.Apart.Structures.Tree.Binary (Binary)

import Data.Apart.Structures.Tree.Binary.Internal (Crotch (..), less, greater, height)
import qualified Data.Apart.Structures.Tree.Binary.Internal as Binary (insert)

type AVL = Binary

insert :: Ord a => a -> AVL a -> AVL a
insert x tree = balancing $ Binary.insert tree x

balancing :: Binary a -> AVL a
balancing tree@(getPredicate simple_left_condition -> True) = rotate (Rotation I L) tree
balancing tree@(getPredicate simple_right_condition -> True) = rotate (Rotation I R) tree
balancing tree@(getPredicate double_left_condition -> True) = rotate (Rotation II L) tree
balancing tree@(getPredicate double_right_condition -> True) = rotate (Rotation II R) tree
balancing tree = tree

simple_left_condition :: Predicate (Binary a)
simple_left_condition = divide (id &&& id)
	gl_LT_or_EQ_gg g_height_diff_2_l

simple_right_condition :: Predicate (Binary a)
simple_right_condition = divide (id &&& id)
	lg_LT_or_EQ_ll l_height_diff_2_g

double_left_condition :: Predicate (Binary a)
double_left_condition = divide (id &&& id)
	gl_LT_or_EQ_gg gl_GT_gg

double_right_condition :: Predicate (Binary a)
double_right_condition = divide (id &&& id)
	lg_LT_or_EQ_ll lg_GT_ll

gl_LT_or_EQ_gg :: Predicate (Binary a)
gl_LT_or_EQ_gg = Predicate $
	\t -> maybe False id $ (<=)
		<$> (height <$> t ^? greater . less <|> pure 0)
		<*> (height <$> t ^? greater . greater)

lg_LT_or_EQ_ll :: Predicate (Binary a)
lg_LT_or_EQ_ll = Predicate $
	\t -> maybe False id $ (<=)
		<$> (height <$> t ^? less . greater <|> pure 0)
		<*> (height <$> t ^? less . less)

gl_GT_gg :: Predicate (Binary a)
gl_GT_gg = Predicate $ \t -> maybe False id $ (>)
	<$> (height <$> t ^? greater . less)
	<*> (height <$> t ^? greater . greater)

lg_GT_ll :: Predicate (Binary a)
lg_GT_ll = Predicate $ \t -> maybe False id $ (>)
	<$> (height <$> t ^? less . greater)
	<*> (height <$> t ^? less . less)

g_height_diff_2_l :: Predicate (Binary a)
g_height_diff_2_l = Predicate $
	\t -> maybe False (==2) $ (-)
		<$> (height <$> t ^? greater)
		<*> (height <$> t ^? less)

l_height_diff_2_g :: Predicate (Binary a)
l_height_diff_2_g = Predicate $
	\t -> maybe False (==2) $ (-)
		<$> (height <$> t ^? less)
		<*> (height <$> t ^? greater)

data Direction = L | R
data Complexity = I | II
data Rotation = Rotation Complexity Direction

rotate :: Rotation -> Binary a -> Binary a
rotate (Rotation I L) tree@(a :< Crotch l (b :< Crotch c r)) = b :< Crotch (a :< Crotch l c) r
rotate (Rotation I L) tree@(a :< Crotch l (b :< Greater r)) = b :< Crotch (a :< Less l) r
rotate (Rotation I R) (a :< Crotch (b :< Crotch l c) r) = b :< Crotch l (a :< Crotch c r)
rotate (Rotation I R) (a :< Crotch (b :< Less l) r) = b :< Crotch l (a :< Greater r)

rotate (Rotation II L) (a :< Crotch l (b :< Crotch (c :< Crotch m n) r)) =
	c :< Crotch (a :< Crotch l m) (b :< Crotch n r)
rotate (Rotation II R) (a :< Crotch (b :< Crotch l (c :< Crotch m n)) r) =
	c :< Crotch (b :< Crotch l m) (a :< Crotch n r)
rotate (Rotation _ _) tree = tree
