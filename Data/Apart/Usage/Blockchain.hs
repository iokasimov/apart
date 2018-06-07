module Data.Apart.Usage.Blockchain (Transaction (..), Block, genesis) where

import Control.Comonad.Cofree (Cofree (..))

import Data.Apart (Segment (..))
import Data.Apart.Structures.Stack (Stack)

type Account = Int
type Tokens = Int

-- | Simplified transaction type, no certificates/keys: from, amount, to.
data Transaction = Transaction Account Tokens Account

-- | Block is just a bunch of transactions
type Block = Stack Transaction

-- | @'Transaction' 0 1000 1 :< Nothing@
--
-- Let's suppose that genesis is a regular transaction, but from magic account 0
genesis :: Block
genesis = Transaction 0 1000 1 :< Nothing
