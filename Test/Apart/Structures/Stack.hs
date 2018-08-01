module Test.Apart.Structures.Stack
	(same_length_with_origin_of_foldaway) where

import "base" Data.Function ((&))
import "hedgehog" Hedgehog (Property (..), Gen (..), forAll, (===), property)
import "hedgehog" Hedgehog.Gen (enumBounded, list)
import "hedgehog" Hedgehog.Range (linear)

import Data.Apart.Structures.Stack (Stack, insert, foldaway)

same_length_with_origin_of_foldaway :: Property
same_length_with_origin_of_foldaway = property $ do
	xs <- forAll $ list (linear 0 1000) (enumBounded :: Gen ())
	(foldaway xs & maybe 0 length) === length xs
