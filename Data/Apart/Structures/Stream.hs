module Data.Apart.Structures.Stream (Stream) where

import Control.Comonad.Cofree (Cofree (..))
import Data.Functor.Identity (Identity (..))

type Stream = Cofree Identity
