module Data.Apart.Abilities.Segmented (Segmented (..)) where

import Control.Comonad.Cofree (Cofree)
import Data.Kind (Type)

type family Segmented (structure :: Type -> Type) (value :: Type) :: Type where
	Segmented (Cofree t) value = t (Cofree t value)
