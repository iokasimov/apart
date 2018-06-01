module Test.Apart.Structures.Tree.Binary.AVL
	(found_element_should_be_lifted_to_root) where

import Hedgehog (Property (..), Gen (..), forAll, (===), property, assert, failure)
import Hedgehog.Gen (enumBounded, list)
import Hedgehog.Range (linear)

import Data.Apart.Structures.Tree.Binary (Binary, Crotch (..))
import Data.Apart.Structures.Tree.Binary.Splay (Splay, search)

found_element_should_be_lifted_to_root :: Property
found_element_should_be_lifted_to_root = undefined
