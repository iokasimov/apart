module Test.Apart.Structures.Tree.Binary.AVL
	(balance_factor_is_well) where

import Hedgehog (Property (..), Gen (..), forAll, (===), property, assert, failure)
import Hedgehog.Gen (enumBounded, list)
import Hedgehog.Range (linear)
import Data.Functor.Bind (Bind (..))

import Data.Apart.Structures.Tree.Binary (Binary, Crotch (..), singleton, factor)
import Data.Apart.Structures.Tree.Binary.AVL (AVL, insert)

balance_factor_is_well :: Property
balance_factor_is_well = property $ do
	xs <- forAll $ list (linear 0 100) (enumBounded :: Gen Int)
	let inserted = foldr (\el t -> t >>- insert el) End xs
	assert $ foldr (\t _ -> -1 <= factor t && factor t <= 1) True inserted
