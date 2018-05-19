module Data.Apart.Structures.Tree.Rose (Rose, construct) where

import Control.Applicative (Alternative (..))
import Control.Comonad.Cofree (Cofree (..), coiter)

type Rose t = Cofree t

construct :: (Functor t, Alternative t) => a -> t a -> Rose t a
construct x structure = (:<) x $ (<$>) (coiter $ const empty) structure
