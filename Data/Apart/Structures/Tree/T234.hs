module Data.Apart.Structures.Tree.T234 (T234) where

import Control.Comonad.Cofree (Cofree (..))

data N234 a b = L2 a a | L3 a a a a | L4 a a a a a a
	| B2 b b | B3 a b b b | B4 a a b b b b

type T234 a = Cofree (N234 a) a
