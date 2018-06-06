module Mastermind.Util where

import Control.Monad.Random.Class

import Control.Monad.Random
import System.Random.Shuffle

powersOf :: Num n => n -> [n]
powersOf a = f 1
  where f x = x : f (x*a)

shuffleDistinct :: (Eq a, MonadRandom m) => [a] -> m [a]
shuffleDistinct xs = do
  shuffled <- shuffleM xs
  if or $ map (uncurry (==)) $ zip xs shuffled
    then shuffleDistinct xs
    else return shuffled
