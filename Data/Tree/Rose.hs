module Data.Tree.Rose (Rose) where

import Control.Comonad.Cofree

type Rose a = Cofree [] a
