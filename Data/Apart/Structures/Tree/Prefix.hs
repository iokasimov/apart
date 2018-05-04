module Data.Apart.Structures.Tree.Prefix (Prefix, seek) where

import Control.Comonad (Comonad (..))
import Control.Comonad.Cofree (Cofree (..), unwrap)
import Data.Maybe (isJust)
import Data.Function ((&))
import Data.Foldable (find)

import Data.Apart.Structures.Stack (Stack)

type Prefix s a = Cofree (Labeled s) a

data Labeled s a = forall t . Traversable t => Hop s (t a)

instance Functor (Labeled s) where
    fmap f (Hop s as) = Hop s $ f <$> as

instance Foldable (Labeled s) where
    foldr f acc (Hop s as) = foldr f acc as

instance Traversable (Labeled s) where
    traverse f (Hop s as) = Hop s <$> traverse f as

seek :: Eq s => Stack s -> Prefix s v -> Maybe v
seek keys prefix = (<$>) extract $ here prefix (extract keys) *> unwrap keys &
    maybe (Just prefix) (\path -> find (isJust . seek path) $ unwrap prefix) where

    here :: Eq s => Prefix s v -> s -> Maybe (Prefix s v)
    here prefix@(v :< Hop s rest) k@((==) s -> True) = Just prefix
    here _ _ = Nothing
