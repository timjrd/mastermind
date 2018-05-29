module Candidate ( candidates
                 , candidatesOverlap ) where

import Util
import Env

import Combination
import CombinationSet

candidate :: CombinationSet -> Env Combination
candidate excluded = do
  r <- random
  if r `member` excluded
    then candidate excluded
    else return r

candidates :: [Combination] -> Env [Combination]
candidates = f . fromList
  where f excluded = do
          r  <- candidate excluded
          rs <- f (insert r excluded)
          return $ r:rs

candidatesOverlap :: [Combination] -> Env [Combination]
candidatesOverlap = f . fromList
  where f excluded = do
          r  <- candidate excluded
          rs <- f excluded
          return $ r:rs
