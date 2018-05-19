module Data.Apart.Structures.Stream (Stream, same) where

import Control.Comonad.Cofree (Cofree (..))
import Data.Functor.Identity (Identity (..))

type Stream = Cofree Identity

same :: a -> Stream a
same x = x :< Identity (same x)

iter :: (a -> a) -> a -> Stream a
iter f x = x :< Identity (iter f $ f x)
