module Data.Apart.Machinery.Moore (Moore, dumb) where

import Control.Comonad.Cofree (Cofree (..))

type Moore a b = Cofree ((->) b) a

dumb :: a -> Moore a a
dumb x = x :< (const $ dumb x)
