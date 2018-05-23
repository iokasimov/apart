module Test.Apart.Structures.Tree.Binary
	( any_value_in_left_subtree_less_than_root_value
	, any_value_in_right_subtree_greater_than_root_value
	) where

import Control.Comonad (extract)
import Control.Comonad.Cofree (Cofree (..), unwrap)
import Control.Lens ((^?))
import Data.Function ((&))
import Hedgehog (Property (..), Gen (..), forAll, (===), property)
import Hedgehog.Gen (enumBounded, list)
import Hedgehog.Range (linear)

import Data.Apart.Structures.Tree.Binary
	(Binary, less, greater, singleton, insert)

gen_singleton_binary_tree :: Gen (Binary Int)
gen_singleton_binary_tree = singleton <$> enumBounded

any_value_in_left_subtree_less_than_root_value :: Property
any_value_in_left_subtree_less_than_root_value = property $ do
	xs <- forAll $ list (linear 0 100) (enumBounded :: Gen Int)
	binary <- forAll $ gen_singleton_binary_tree
	let inserted = foldr (flip insert) binary xs
	let all_less_than_focus = inserted ^? less &
		maybe True (all ((>=) (extract inserted)))
	all_less_than_focus === True

any_value_in_right_subtree_greater_than_root_value :: Property
any_value_in_right_subtree_greater_than_root_value = property $ do
	xs <- forAll $ list (linear 0 100) (enumBounded :: Gen Int)
	binary <- forAll $ gen_singleton_binary_tree
	let inserted = foldr (flip insert) binary xs
	let all_greater_than_focus = inserted ^? greater &
		maybe True (all ((<=) (extract inserted)))
	all_greater_than_focus === True
