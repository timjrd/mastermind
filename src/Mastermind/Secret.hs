module Mastermind.Secret
  ( genericSecrets
  , genericSecretsOverlap ) where

import Control.Monad
import Data.Maybe

import System.Random.Shuffle

import Mastermind.Env
import Mastermind.Combination
import Mastermind.Combination.Set (Set, empty, member, insert)
import Mastermind.Hint

stdSecret :: ( Combination c
             , Integral i
             , ?powers :: [i]
             , _ )
          => c -> Hint -> Env c
stdSecret x y = do
  let x' = toList x
  shuffled <- shuffleM (zip [0..] x')
  let (goods,r)    = splitAt (good y) shuffled
      (bads,wrong) = splitAt (bad  y) r
  return $ fromList $ snd $ unzip shuffled

valid :: _ => Combination c
      => [(c,Hint)] -> c -> Maybe c
valid constraints secret =
  if isValid
    then Just secret
    else Nothing
  where
    isValid = and $ map f constraints
    f (x,y) = y == ?hint secret x

genericSecrets :: _ => Combination c => [(c,Hint)] -> Env [c]
genericSecrets constraints = f 0 empty constraints
  where
    f n excluded constraints = do
      if n >= ?cardinality
        then return []
        else do
          r <- random
          let (n', excluded', r') =
                if r `member` excluded
                then (n  , excluded         , Nothing)
                else (n+1, insert r excluded, Just r )
          let r'' = join $ mapM (valid constraints) r'
          rs <- f n' excluded' constraints
          return $ maybeToList r'' ++ rs

genericSecretsOverlap :: _ => Combination c => [(c,Hint)] -> Env [c]
genericSecretsOverlap constraints = do
  r  <- random
  let r' = valid constraints r  
  rs <- genericSecretsOverlap constraints
  return $ maybeToList r' ++ rs
  
  
  
