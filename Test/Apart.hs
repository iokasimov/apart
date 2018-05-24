module Main where

import System.IO (BufferMode(..), hSetBuffering, stdout, stderr)
import Hedgehog (Group (..), checkParallel)

import Test.Apart.Structures.Stack
import Test.Apart.Structures.Tree.Binary
import Test.Apart.Structures.Tree.Binary.AVL

main = do
	hSetBuffering stdout LineBuffering
	hSetBuffering stderr LineBuffering

	checkParallel $ Group "Stack structure" [
		( "Same length with origin of foldaway"
		, same_length_with_origin_of_foldaway )
		]

	checkParallel $ Group "Binary tree structure" [
		( "Any value in left subtree less than root value"
		, any_value_in_left_subtree_less_than_root_value ),
		( "Any value in right subtree greater than root value"
		, any_value_in_right_subtree_greater_than_root_value )
		]

	checkParallel $ Group "AVL tree structure" [
		( "Balance factor is well"
		, balance_factor_is_well )
		]
