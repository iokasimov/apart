module Data.Apart.Usage.Blockchain
	(Transaction (..), Block, Blockchain, genesis, utxo) where

import Control.Comonad.Cofree (Cofree (..))

import Data.Apart (Segment (..), Scattered (..))
import Data.Apart.Structures.Stack (Stack)

type Account = Int
type Tokens = Int

-- | Simplified transaction type, no certificates/keys: from, amount, to.
data Transaction = Transaction Account Tokens Account

-- | Block is just a bunch of transactions, no pointers, keys here
type Block = Stack Transaction

-- | Let's suppose that genesis is a regular transaction,
-- but proceeded from magic account 0 to real accounts
--
-- @'Transaction' 0 1000 1 :< Nothing@
genesis :: Block
genesis = Transaction 0 1000 1 :< Nothing

data Balance = Balance Int Int

-- | Our blockchain type is distributed - we really don't want to keep all chain in memory,
-- instead, we'll store balance table of all accounts
type Blockchain = Scattered Stack Block [Balance]

utxo :: Account -> Blockchain -> Tokens
utxo account blockchain = undefined
