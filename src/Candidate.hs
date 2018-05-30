module Candidate ( candidates
                 , candidatesOverlap ) where

import Util
import Env

import Combination
import CombinationSet as S

candidate :: CombinationSet -> Env Combination
candidate excluded = do
  r <- random
  if r `member` excluded
    then candidate excluded
    else return r

candidates :: [Combination] -> Env [Combination]
candidates excluded = f (length excluded) (S.fromList excluded)
  where
    f n excluded = do
      card <- asks cardinality
      r    <- candidate excluded
      rs   <- f (n+1) (insert r excluded)
      if n >= card
        then return []
        else return $ r:rs
          

candidatesOverlap :: [Combination] -> Env [Combination]
candidatesOverlap = f . S.fromList
  where
    f excluded = do
      r  <- candidate excluded
      rs <- f excluded
      return $ r:rs
