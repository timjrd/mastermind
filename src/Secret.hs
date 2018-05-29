module Secret ( secrets
              , secretsOverlap ) where

import Control.Monad

import Env

import Combination
import CombinationSet
import Hint

secret :: CombinationSet -> [(Combination,Hint)] -> Env Combination
secret excluded constraints = do
  r  <- random
  ht <- hint <$> ask
  let f (x,y) = (y==) <$> ht r x
  valid <- forM constraints f
  if and valid && r `notMember` excluded
    then return r
    else secret excluded constraints

secrets :: [(Combination,Hint)] -> Env [Combination]
secrets = f empty
  where f excluded constraints = do
          r  <- secret excluded constraints
          rs <- f (insert r excluded) constraints
          return $ r:rs

secretsOverlap :: [(Combination,Hint)] -> Env [Combination]
secretsOverlap constraints = do
  r  <- secret empty constraints
  rs <- secretsOverlap constraints
  return $ r:rs
  
