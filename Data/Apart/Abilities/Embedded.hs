module Data.Apart.Abilities.Embedded (Embedded (..)) where

import Control.Comonad.Cofree (Cofree)
import Data.Functor.Compose (Compose)
import Data.Kind (Type)

type (:.:) = Compose

type family Embedded (structure :: Type -> Type) (extension :: Type -> Type) (value :: Type) :: Type where
	Embedded (Cofree t) extension value = Cofree (extension :.: t) value
