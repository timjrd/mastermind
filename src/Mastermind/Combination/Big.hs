module Mastermind.Combination.Big
  ( Big
  , big ) where

import Control.Monad.Random

import Mastermind.Env
import Mastermind.Combination
import Mastermind.Combination.Compact.Internal

newtype Big = Big [Int]
  deriving (Eq, Ord)

instance Combination Big where
  random = do
    r <- take ?holes <$> getRandomRs (0, fromIntegral ?colors - 1)
    return $ Big r

  fromList = Big
  toList (Big x) = x

big = undefined :: Big

