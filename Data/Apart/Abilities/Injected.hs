module Data.Apart.Abilities.Injected (Injected (..)) where

import Control.Comonad.Cofree (Cofree)
import Data.Functor.Compose (Compose)
import Data.Kind (Type)

type (:.:) = Compose

type family Injected (structure :: Type -> Type) (extension :: Type -> Type) (value :: Type) :: Type where
	Injected (Cofree t) extension value = Cofree (t :.: extension) value
