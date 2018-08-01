module Data.Apart.Structures.Tree.Prefix
	(Prefix, Labeled (..), singleton, seek, insert, crumbs) where

import "base" Control.Applicative (Alternative (..))
import "base" Control.Arrow ((&&&))
import "base" Data.Foldable (find)
import "base" Data.Function ((&))
import "base" Data.Maybe (isJust)
import "base" Data.Monoid (Monoid (..), (<>))
import "comonad" Control.Comonad (Comonad (..))
import "contravariant" Data.Functor.Contravariant (Predicate (..))
import "contravariant" Data.Functor.Contravariant.Divisible (Divisible (..))
import "free" Control.Comonad.Cofree (Cofree (..), unwrap)
import "lens" Control.Lens (Lens', (^.), (%~))

import Data.Apart.Structures.Stack (Stack)

type Prefix s t = Cofree (Labeled s t)

data Labeled s t a = Hop s (t a) deriving Show

nodes :: Lens' (Prefix s t a) (t (Prefix s t a))
nodes f (x :< Hop s ns) = (\new -> x :< Hop s new) <$> f ns

instance Functor t => Functor (Labeled s t) where
	fmap f (Hop s as) = Hop s $ f <$> as

instance Foldable t => Foldable (Labeled s t) where
	foldr f acc (Hop s as) = foldr f acc as

instance Traversable t => Traversable (Labeled s t) where
	traverse f (Hop s as) = Hop s <$> traverse f as

singleton :: Alternative t => s -> a -> Prefix s t a
singleton s v = v :< Hop s empty

-- | Prefix tree haven't nodes
deadend :: Foldable t => Predicate (Prefix s t a)
deadend = Predicate $ \(_ :< Hop _ ns) -> length ns == 0

-- | Key and current key of root matched
progress :: (Eq s, Foldable t) => Predicate (s, Prefix s t a)
progress = Predicate $ \(s, _ :< Hop s' ns) -> s == s'

-- | Keys matched and this is the end
exactly :: (Eq s, Foldable t) => Predicate (s, Prefix s t a)
exactly = divide (snd &&& id) deadend progress

seek :: (Functor t, Foldable t, Eq s)
	=> Stack s -> Prefix s t v -> Maybe v
seek (s :< Just ss) prefix@(getPredicate progress . (s,) -> True) =
	(<$>) extract $ find (isJust . seek ss) $ unwrap prefix
seek (s :< Nothing) prefix@(getPredicate progress . (s,) -> True) = Just $ extract prefix
seek (s :< _) prefix@(getPredicate progress . (s,) -> False) = Nothing

-- | You can insert value with @path + 1 symbol@ of existing @path@ in tree.
insert :: (Foldable t, Alternative t, Eq s)
	=> Stack s -> v -> Prefix s t v -> Prefix s t v
insert (s :< _) x prefix@(getPredicate progress . (s,) -> False) = prefix
insert (s :< Nothing) x prefix@(getPredicate progress . (s,) -> True) = x :< unwrap prefix
insert (s :< Just ss@(s' :< Just _)) x prefix@(getPredicate progress . (,) s -> True) =
	prefix & nodes %~ (<$>) (insert ss x)
insert (s :< Just ss@(s' :< Nothing)) x prefix@(getPredicate progress . (,) s -> True) =
	prefix & nodes %~ (<|>) (pure $ x :< Hop s' empty)
insert _ _ prefix = prefix

-- | Unlike @insert@, you can specify longest path, but a gap will be filled Monoid's empty values
crumbs :: (Foldable t, Alternative t, Eq s, Monoid v)
	=> Stack s -> v -> Prefix s t v -> Prefix s t v
crumbs (s :< _) x prefix@(getPredicate progress . (s,) -> False) = prefix
crumbs (s :< Just ss) x prefix@(getPredicate exactly . (,) s -> True) =
	(extract prefix) :< Hop s (pure $ crumbs ss x $ mempty :< Hop (extract ss) empty)
crumbs (s :< Nothing) x prefix@(getPredicate exactly . (,) s -> True) = x :< Hop s empty
crumbs (s :< Just ss@(s' :< Just _)) x prefix@(getPredicate progress . (,) s -> True) =
	prefix & nodes %~ (<$>) (crumbs ss x)
crumbs (s :< Just (s' :< Nothing)) x prefix@(getPredicate progress . (,) s -> True) =
	prefix & nodes %~ (<|>) (pure $ x :< Hop s' empty)
crumbs _ _ prefix = prefix
