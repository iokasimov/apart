module Data.Structure.Stack (Stack, push) where

import Control.Comonad.Cofree

type Stack a = Cofree Maybe a

push :: a -> Stack a -> Stack a
push x = (:<) x . Just
