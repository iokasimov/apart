module Main where

import System.IO (BufferMode(..), hSetBuffering, stdout, stderr)
import Hedgehog (Group (..), checkParallel)

import Test.Apart.Structures.Stack (same_length_with_origin_of_foldaway)

main = do
	hSetBuffering stdout LineBuffering
	hSetBuffering stderr LineBuffering

	checkParallel $ Group "Stack structure"
		[("same_length_with_origin_of_foldaway", same_length_with_origin_of_foldaway)]
