module Internal where

import Control.Monad.Reader
import Control.Monad.Random

newtype Combination = Combination Int
  deriving (Eq, Show)

data Hint = Hint Int Int
  deriving (Eq, Show)

data Param = Param { colors      :: Int
                   , holes       :: Int
                   , hintFn      :: Combination -> Combination -> Env Hint
                   
                   -- the following are derived from the above
                   , cardinality :: Int
                   , powers      :: [Int] }

type Env = ReaderT Param (Rand StdGen)
