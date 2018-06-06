module Mastermind.Candidate
  ( candidates
  , candidatesOverlap ) where

import Mastermind.Util
import Mastermind.Env
import Mastermind.Combination (Combination,random)
import Mastermind.Combination.Set

candidate :: _ => Set s c => s -> Env c
candidate excluded = do
  r <- random
  if r `member` excluded
    then candidate excluded
    else return r

candidates :: _ => Combination c => [c] -> Env [c]
candidates excluded = f (length excluded) (fromList excluded)
  where
    f n excluded = do
      if fromIntegral n >= ?cardinality
        then return []
        else do
          r  <- candidate excluded
          rs <- f (n+1) (insert r excluded)        
          return $ r:rs
          

candidatesOverlap :: _ => Combination c => [c] -> Env [c]
candidatesOverlap excluded = f (fromList excluded)
  where
    f excluded = do
      r  <- candidate excluded
      rs <- f excluded
      return $ r:rs
