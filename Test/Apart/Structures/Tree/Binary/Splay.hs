module Test.Apart.Structures.Tree.Binary.Splay
	(found_element_should_be_lifted_to_root) where

import "comonad" Control.Comonad (Comonad (..))
import "hedgehog" Hedgehog (Property (..), Gen (..), forAll, property, assert)
import "hedgehog" Hedgehog.Gen (enumBounded, list)
import "hedgehog" Hedgehog.Range (linear)
import "semigroupoids" Data.Functor.Bind (Bind (..))

import Data.Apart.Structures.Tree.Binary (Binary, Branches (..))
import Data.Apart.Structures.Tree.Binary.Splay (insert, search)

found_element_should_be_lifted_to_root :: Property
found_element_should_be_lifted_to_root = property $ do
	xs <- forAll $ list (linear 0 100) (enumBounded :: Gen Int)
	let inserted = foldr (\el t -> t >>- insert el) End xs
	ys <- forAll $ list (linear 0 100) (enumBounded :: Gen Int)
	assert $ all (all id) $ (\y -> inserted >>- (<$>) ((== y) . extract) . search y) <$> ys
