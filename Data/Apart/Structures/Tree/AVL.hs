module Data.Apart.Structures.Tree.AVL (AVL) where

import Control.Comonad.Cofree (Cofree (..))

type AVL a = Cofree Balanced a

-- balance factor
data Balanced a = End | Leftlonger a a | Rightlonger a a | Equalheight a a

instance Functor Balanced where
    fmap f End = End
    fmap f (Leftlonger l r) = Leftlonger (f l) (f r)
    fmap f (Rightlonger l r) = Rightlonger (f l) (f r)
    fmap f (Equalheight l r) = Equalheight (f l) (f r)

-- pre-order traversal only
instance Foldable Balanced where
    foldr f acc End = acc
    foldr f acc (Leftlonger l r) = f l $ f r acc
    foldr f acc (Rightlonger l r) = f l $ f r acc
    foldr f acc (Equalheight l r) = f l $ f r acc

instance Traversable Balanced where
    traverse f End = pure End
    traverse f (Leftlonger l r) = Leftlonger <$> f l <*> f r
    traverse f (Rightlonger l r) = Rightlonger <$> f l <*> f r
    traverse f (Equalheight l r) = Equalheight <$> f l <*> f r
