module Data.Apart.Transformations (Attached (..), Embedded (..), Injected (..), Segmented (..), Scattered (..)) where

import Control.Comonad.Cofree (Cofree)
import Data.Functor.Compose (Compose)
import Data.Kind (Type)

import Data.Apart (Apart)

type (:.:) = Compose

type family Attached (structure :: Type -> Type) (extension :: Type -> Type) (value :: Type) :: Type where
    Attached (Cofree t) extension value = Cofree t (extension value)

type family Embedded (structure :: Type -> Type) (extension :: Type -> Type) (value :: Type) :: Type where
	Embedded (Cofree t) extension value = Cofree (extension :.: t) value

type family Injected (structure :: Type -> Type) (extension :: Type -> Type) (value :: Type) :: Type where
	Injected (Cofree t) extension value = Cofree (t :.: extension) value

type family Segmented (structure :: Type -> Type) (value :: Type) :: Type where
	Segmented (Cofree t) value = t (Cofree t value)

type family Scattered (structure :: Type -> Type) (value :: Type) (raw :: Type) :: Type where
	Scattered (Cofree t) value raw = Apart t raw value
