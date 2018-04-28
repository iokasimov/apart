module Data.Structure.Stack (Stack) where

import Control.Comonad.Cofree

type Stack a = Cofree Maybe a
