module Test.Apart.Structures.Tree.Prefix
	(after_successful_insert_length_should_be_incremented) where

import Hedgehog (Property (..), Gen (..), forAll, property, assert)
import Hedgehog.Gen (enumBounded, list, integral)
import Hedgehog.Range (linear)

import Data.Apart.Structures.Stack (Stack, foldaway)
import Data.Apart.Structures.Tree.Prefix (Prefix, singleton, insert)

gen_singleton_prefix_tree :: Gen (Prefix Char [] Int)
gen_singleton_prefix_tree = singleton <$> enumBounded <*> enumBounded

after_successful_insert_length_should_be_incremented :: Property
after_successful_insert_length_should_be_incremented = property $ do
	prefix <- forAll $ gen_singleton_prefix_tree
	path <- forAll $ list (linear 0 10) (enumBounded :: Gen Char)
	value <- forAll (integral (linear 0 1000000) :: Gen Int)
	assert $ maybe True ((>=) (length prefix + 1) . length) $
		(\ss -> insert ss value prefix) <$> foldaway path
