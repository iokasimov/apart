module Test.Apart.Structures.Tree.Binary.AVL
	(balance_factor_is_well) where

import "hedgehog" Hedgehog (Property (..), Gen (..), forAll, (===), property, assert, failure)
import "hedgehog" Hedgehog.Gen (enumBounded, list)
import "hedgehog" Hedgehog.Range (linear)
import "semigroupoids" Data.Functor.Bind (Bind (..))

import Data.Apart.Structures.Tree.Binary (Binary, Branches (..), singleton, factor)
import Data.Apart.Structures.Tree.Binary.AVL (insert)

balance_factor_is_well :: Property
balance_factor_is_well = property $ do
	xs <- forAll $ list (linear 0 100) (enumBounded :: Gen Int)
	let inserted = foldr (\el t -> t >>- insert el) End xs
	assert $ foldr (\t _ -> -1 <= factor t && factor t <= 1) True inserted
