module Data.Apart.Structures.Tree.Binary.Splay (Splay) where

import Control.Comonad (Comonad (..))
import Data.Functor.Bind (Bind (..))
import Data.Functor.Contravariant (Predicate (..))
import Data.Function ((&))

import Data.Apart.Apart (Segment (..))
import Data.Apart.Structures.Tree.Binary (Binary, ls, gt)
import Data.Apart.Structures.Tree.Binary.Rotation (Rotate (..), rtt)

type Splay = Binary

left_zig :: Eq a => Predicate (a, Binary a)
left_zig = Predicate $ \ (x, t) -> gt t
	& foldr (\g _ -> extract g == x) False

right_zig :: Eq a => Predicate (a, Binary a)
right_zig = Predicate $ \ (x, t) -> ls t
	& foldr (\l _ -> extract l == x) False

left_zig_zig :: Eq a => Predicate (a, Binary a)
left_zig_zig = Predicate $ \ (x, t) -> gt t >>- gt
	& foldr (\gg _ -> extract gg == x) False

right_zig_zig :: Eq a => Predicate (a, Binary a)
right_zig_zig = Predicate $ \ (x, t) -> ls t >>- ls
	& foldr (\ll _ -> extract ll == x) False

left_zig_zag :: Eq a => Predicate (a, Binary a)
left_zig_zag = Predicate $ \ (x, t) -> gt t >>- ls
	& foldr (\ll _ -> extract ll == x) False

right_zig_zag :: Eq a => Predicate (a, Binary a)
right_zig_zag = Predicate $ \ (x, t) -> ls t >>- gt
	& foldr (\ll _ -> extract ll == x) False

splay :: Eq a => a -> Binary a -> Segment Splay a
splay x t@(getPredicate left_zig . (x,) -> True) = rtt L t
splay x t@(getPredicate right_zig . (x,) -> True) = rtt R t
splay x t@(getPredicate left_zig_zig . (x,) -> True) = rtt LL t
splay x t@(getPredicate right_zig_zig . (x,) -> True) = rtt RR t
splay x t@(getPredicate left_zig_zag . (x,) -> True) = rtt RL t
splay x t@(getPredicate right_zig_zag . (x,) -> True) = rtt LR t
