module Data.Apart.Structures.Stream (Stream, same) where

import "base" Data.Functor.Identity (Identity (..))
import "free" Control.Comonad.Cofree (Cofree (..))

-- | Infinite sequence.
type Stream = Cofree Identity

same :: a -> Stream a
same x = x :< Identity (same x)

iter :: (a -> a) -> a -> Stream a
iter f x = x :< Identity (iter f $ f x)
