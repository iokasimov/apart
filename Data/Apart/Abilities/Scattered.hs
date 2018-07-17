module Data.Apart.Abilities.Scattered (Scattered (..)) where

import Control.Comonad.Cofree (Cofree)
import Data.Kind (Type)

import Data.Apart.Apart (Apart)

type family Scattered (structure :: Type -> Type) (value :: Type) (raw :: Type) :: Type where
	Scattered (Cofree t) value raw = Apart t raw value
