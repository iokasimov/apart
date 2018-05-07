module Data.Apart.Structures.Tree.AVL (AVL) where

import Control.Comonad.Cofree (Cofree (..))

import Data.Apart.Structures.Tree.Binary (Binary)
import qualified Data.Apart.Structures.Tree.Binary as Binary (insert)

type AVL a = Binary a
