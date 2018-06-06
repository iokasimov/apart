module Data.Apart.Structures.Tree.Rose (Rose, singleton, construct) where

import Control.Applicative (Alternative (..))
import Control.Comonad.Cofree (Cofree (..), coiter)

type Rose t = Cofree t

singleton :: Alternative t => a -> Rose t a
singleton x = x :< empty

construct :: (Functor t, Alternative t) => a -> t a -> Rose t a
construct x structure = (:<) x $ (<$>) (coiter $ const empty) structure
