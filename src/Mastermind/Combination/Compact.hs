module Mastermind.Combination.Compact
  ( module Mastermind.Combination.Compact
  , Compact ) where

import Control.Monad.Random

import Mastermind.Env
import Mastermind.Combination
import Mastermind.Combination.Compact.Internal

instance Combination Compact where
  random = do
    r <- getRandomR (0, fromIntegral ?cardinality - 1)
    return $ Compact r

  fromList xs =
    let muls = map (uncurry (*)) $ zip (map fromIntegral ?powers) xs
    in Compact $ foldl (+) 0 muls

  toList (Compact x) =
    let f power = (x `div` power) `mod` ?colors
    in map f $ map fromIntegral ?powers

compact = undefined :: Compact
