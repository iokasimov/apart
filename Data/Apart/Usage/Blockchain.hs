module Data.Apart.Usage.Blockchain
	(Transaction (..), Block, Blockchain, genesis, block, mainchain, verify) where

import Control.Comonad.Cofree (Cofree (..))
import Data.Bitraversable (Bitraversable (..))
import Data.Functor.Compose (Compose (..))

import Data.Apart (Apart (..), Shape (..))
import Data.Apart.Abilities.Scattered (Scattered (..))
import Data.Apart.Structures.Stack (Stack)

type Account = Int
type Tokens = Int
data Connection
data Table

-- | Simplified transaction type, no certificates/keys.
data Transaction = Transaction
	{ from :: Account, amount :: Tokens, to :: Account }

-- | Block is just a bunch of transactions, no pointers/keys here.
type Block = Stack Transaction

-- | Our blockchain type is distributed - we really don't want to keep all chain in memory,
-- instead, we'll store balance table of all accounts in some database.
type Blockchain = Scattered Stack Block (Connection, Table)

-- | Let's suppose that genesis is a regular transaction,
-- but proceeded from magic account 0 to real accounts.
--
-- @'Transaction' 0 1000 1 ':<' 'Nothing'@
genesis :: Block
genesis = Transaction 0 1000 1 :< Nothing

-- | First real block, 1 sends 50 tokens to 4.
--
-- @'Transaction' 1 50 4 ':<' 'Nothing'@
block :: Block
block = Transaction 1 50 4 :< Nothing

-- | We're facing with increasing chain in memory, so, let's calculate UTXO
-- for each account and put this information somewhere else.
--
-- @'Apart' $ 'block' ':<' 'Converted' ...@
mainchain :: Blockchain
mainchain = Apart $ block :< Converted (undefined, undefined)

-- | Verifying block is just a calculation UTXO for each account that trying to send money,
-- so, we already have balance table and current block, the task becomes simple -
-- just match transactions and balances.
verify :: Stack Transaction -> Blockchain -> Compose IO Maybe Block
verify txs chain = undefined
