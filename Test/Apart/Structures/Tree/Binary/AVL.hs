module Test.Apart.Structures.Tree.Binary.AVL
	(balance_factor_less_than_1) where

import Hedgehog (Property (..), Gen (..), forAll, (===), property, assert, failure)
import Hedgehog.Gen (enumBounded, list)
import Hedgehog.Range (linear)

import Data.Apart.Structures.Tree.Binary (Binary, singleton, factor)
import Data.Apart.Structures.Tree.Binary.AVL (AVL, insert)

gen_singleton_binary_tree :: Gen (Binary Int)
gen_singleton_binary_tree = singleton <$> enumBounded

balance_factor_less_than_1 :: Property
balance_factor_less_than_1 = property $ do
	xs <- forAll $ list (linear 0 100) (enumBounded :: Gen Int)
	binary <- forAll $ gen_singleton_binary_tree
	let inserted = foldr insert binary xs
	assert $ -1 <= factor inserted && factor inserted <= 1
