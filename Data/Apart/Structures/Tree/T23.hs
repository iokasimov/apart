module Data.Apart.Structures.Tree.T23 (T23, N23 (..)) where

import Control.Comonad.Cofree (Cofree (..))

data N23 a b = L2 a a | L3 a a a a | B2 b b | B3 a b b b

type T23 a = Cofree (N23 a) a
