module Data.Apart.Structures.Tree.Binary.Redblack (Redblack, Color (..), paint) where

import "free" Control.Comonad.Cofree (Cofree (..))

import Data.Apart.Structures.Tree.Binary (Binary, Branches (..))

data Color = Red | Black deriving Show

type Redblack a = Binary (a, Color)

paint :: Binary a -> Redblack a
paint tree = step Black tree where

	step :: Color -> Binary a -> Redblack a
	step color (x :< End) = (x, Black) :< End
	step color (x :< rest) = (x, color) :< (step (invert color) <$> rest)

	invert :: Color -> Color
	invert Black = Red
	invert Red = Black
