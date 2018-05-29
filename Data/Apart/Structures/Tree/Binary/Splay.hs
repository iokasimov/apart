module Data.Apart.Structures.Tree.Binary.Splay (Splay) where

import Control.Comonad (Comonad (..))
import Data.Functor.Bind (Bind (..))
import Data.Functor.Contravariant (Predicate (..))

import Data.Apart.Structures.Tree.Binary (Binary, ls, gt)

type Splay = Binary

left_zig :: Eq a => Predicate (a, a, Binary a)
left_zig = Predicate $ \ (x, p, t) -> extract t == p &&
	(foldr (\g _ -> extract g == x) False $ gt t)

right_zig :: Eq a => Predicate (a, a, Binary a)
right_zig = Predicate $ \ (x, p, t) -> extract t == p &&
	(foldr (\l _ -> extract l == x) False $ ls t)

left_zig_zig :: Eq a => Predicate (a, a, Binary a)
left_zig_zig = Predicate $ \ (x, p, t) ->
	(foldr (\g _ -> extract g == p) False $ gt t) &&
	(foldr (\gg _ -> extract gg == x) False $ gt t >>- gt)

right_zig_zig :: Eq a => Predicate (a, a, Binary a)
right_zig_zig = Predicate $ \ (x, p, t) ->
	(foldr (\l _ -> extract l == p) False $ ls t) &&
	(foldr (\ll _ -> extract ll == x) False $ ls t >>- ls)

left_zig_zag :: Eq a => Predicate (a, a, Binary a)
left_zig_zag = Predicate $ \ (x, p, t) ->
	(foldr (\l _ -> extract l == p) False $ gt t) &&
	(foldr (\ll _ -> extract ll == x) False $ gt t >>- ls)

right_zig_zag :: Eq a => Predicate (a, a, Binary a)
right_zig_zag = Predicate $ \ (x, p, t) ->
	(foldr (\l _ -> extract l == p) False $ ls t) &&
	(foldr (\ll _ -> extract ll == x) False $ ls t >>- gt)
