module Data.Apart.Structures.Tree.Prefix (Prefix, Labeled (..), seek, insert) where

import Control.Applicative (Alternative (..))
import Control.Comonad (Comonad (..))
import Control.Comonad.Cofree (Cofree (..), unwrap)
import Control.Lens (Lens', (^.), (%~))
import Data.Maybe (isJust)
import Data.Function ((&))
import Data.Foldable (find)
import Data.Monoid (Monoid (..), (<>))

import Data.Apart.Structures.Stack (Stack)

type Prefix s t = Cofree (Labeled s t)

data Labeled s t a = Hop s (t a) deriving Show

symbol :: Lens' (Prefix s t a) s
symbol f (x :< Hop s ns) = (\new -> x :< Hop new ns) <$> f s

nodes :: Lens' (Prefix s t a) (t (Prefix s t a))
nodes f (x :< Hop s ns) = (\new -> x :< Hop s new) <$> f ns

instance Functor t => Functor (Labeled s t) where
	fmap f (Hop s as) = Hop s $ f <$> as

instance Foldable t => Foldable (Labeled s t) where
	foldr f acc (Hop s as) = foldr f acc as

instance Traversable t => Traversable (Labeled s t) where
	traverse f (Hop s as) = Hop s <$> traverse f as

seek :: (Functor t, Foldable t, Eq s) => Stack s -> Prefix s t v -> Maybe v
seek (s :< Just ss) prefix@((==) s . flip (^.) symbol -> True) =
	(<$>) extract $ find (isJust . seek ss) $ unwrap prefix
seek (s :< Nothing) prefix@((==) s . flip (^.) symbol -> True) = Just $ extract prefix
seek (s :< _) prefix@((==) s . flip (^.) symbol -> False) = Nothing

-- | You can insert value with @path + 1 symbol@ of existing @path@ in tree.
insert :: (Foldable t, Alternative t, Eq s) => Stack s -> v -> Prefix s t v -> Prefix s t v
insert (s :< _) x prefix@((==) s . flip (^.) symbol -> False) = prefix
insert (s :< Nothing) x prefix@((==) s . flip (^.) symbol -> True) = x :< unwrap prefix
insert (s :< Just ss@(s' :< Just _)) x prefix@((==) s . flip (^.) symbol -> True) =
	prefix & nodes %~ (<$>) (insert ss x)
insert (s :< Just ss@(s' :< Nothing)) x prefix@((==) s . flip (^.) symbol -> True) =
	prefix & nodes %~ (<|>) (pure $ x :< Hop s' empty)
