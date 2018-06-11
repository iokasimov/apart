module Data.Apart.Structures.Tree.Binary.Redblack (Redblack, Color (..)) where

import Data.Apart.Structures.Tree.Binary (Binary)

data Color = Red | Black
type Redblack a = Binary (a, Color)
