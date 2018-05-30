module Secret ( secrets
              , secretsOverlap ) where

import Control.Monad

import Env

import Combination
import CombinationSet
import Hint

isValid :: [(Combination,Hint)] -> Combination -> Env Bool
isValid constraints secret = and <$> forM constraints f
  where
    f (x,y) = (y==) <$> hint secret x

secrets :: [(Combination,Hint)] -> Env [Combination]
secrets = f 0 empty
  where
    f n excluded constraints = do
      card <- asks cardinality
      r    <- random
      let (n', excluded', r') =
            if r `member` excluded
            then (n  , excluded         , [] )
            else (n+1, insert r excluded, [r])
      rs <- f n' excluded' constraints
      if n >= card
        then return []
        else return $ r' ++ rs

secretsOverlap :: [(Combination,Hint)] -> Env [Combination]
secretsOverlap constraints = do
  r     <- random
  valid <- isValid constraints r
  rs    <- secretsOverlap constraints
  if valid
    then return $ r:rs
    else return rs
  
  
  
