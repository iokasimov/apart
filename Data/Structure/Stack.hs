module Data.Structure.Stack (Stack, push) where

import Control.Comonad.Cofree (Cofree (..), unwrap)

type Stack a = Cofree Maybe a

push :: a -> Stack a -> Stack a
push x = (:<) x . Just
