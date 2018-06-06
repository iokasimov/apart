module Test.Apart.Structures.Tree.Prefix where

import Data.Apart.Structures.Tree.Prefix (Prefix, singleton)

gen_singleton_prefix_tree :: Gen (Prefix Char Int [])
gen_singleton_prefix_tree = singleton <$> enumBounded <$> enumBounded
