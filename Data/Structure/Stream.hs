module Data.Structure.Stream (Stream) where

import Control.Comonad.Cofree
import Data.Functor.Identity

type Stream a = Cofree Identity a
