module Data.Structure.Tree.Rose (Rose) where

import Control.Comonad.Cofree

type Rose a = Cofree [] a
