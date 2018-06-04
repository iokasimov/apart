-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Apart
-- Copyright   :  (C) 2018 Murat Kasimov
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Murat Kasimov <iokasimov.m@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Get all your structure and rip it apart.
--
-- The main idea: if you can describe your data structure via Cofree, with apart you can serialize, persistent or hash a segment of your structure!
--
-- A simple introduction to this library can be found here: https://iokasimov.github.io/posts/2018/05/cofree-will-tear-us-apart
----------------------------------------------------------------------------

module Data.Apart
	( module Data.Apart.Apart
	, module Data.Apart.Combinators
	) where

import Data.Apart.Apart
import Data.Apart.Combinators
