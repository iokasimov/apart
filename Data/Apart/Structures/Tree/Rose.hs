module Data.Apart.Structures.Tree.Rose (Rose) where

import Control.Comonad.Cofree (Cofree (..))

type Rose a = Cofree [] a
