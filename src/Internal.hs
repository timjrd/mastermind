module Internal where

import Control.Monad.Reader
import Control.Monad.Random

newtype Combination = Combination Int
  deriving (Eq)

data Hint = Hint Int Int
  deriving (Eq)

data Param = Param { colors :: Int
                   , holes  :: Int
                   , hint   :: Combination -> Combination -> Env Hint }

-- data Param' = Param' ... cardinality ...

type Env = ReaderT Param (Rand StdGen)
