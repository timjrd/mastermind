module Secret ( secrets
              , secretsOverlap ) where

import Control.Monad
import Data.Maybe

import Env

import Combination
import CombinationSet
import Hint

valid :: [(Combination,Hint)] -> Combination -> Env (Maybe Combination)
valid constraints secret = do
  v <- and <$> mapM f constraints
  return $ if v then Just secret else Nothing
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
            then (n  , excluded         , Nothing)
            else (n+1, insert r excluded, Just r )
      r'' <- join <$> mapM (valid constraints) r'
      rs  <- f n' excluded' constraints
      if n >= card
        then return []
        else return $ maybeToList r'' ++ rs

secretsOverlap :: [(Combination,Hint)] -> Env [Combination]
secretsOverlap constraints = do
  r  <- random
  r' <- valid constraints r
  rs <- secretsOverlap constraints
  return $ maybeToList r' ++ rs
  
  
  
