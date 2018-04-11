module Data.Tree (Tree) where

import Control.Comonad.Cofree

type Tree f a = Cofree f a
