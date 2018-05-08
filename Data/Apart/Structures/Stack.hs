module Data.Apart.Structures.Stack (Stack, push) where

import Control.Comonad.Cofree (Cofree (..), unwrap)

type Stack = Cofree Maybe

push :: a -> Stack a -> Stack a
push x = (:<) x . Just
