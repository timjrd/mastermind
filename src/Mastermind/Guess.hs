module Mastermind.Guess where

import qualified Data.Set as S
import Data.List (maximumBy)
import Data.Ord (comparing)

import Mastermind.Util
import Mastermind.Env
import Mastermind.Hint
import Mastermind.Combination
import Mastermind.Candidate
import Mastermind.Secret

maxSecrets    = 1000
maxCandidates = 2000

guess :: _ => Combination c => [(c,Hint)] -> Env c
guess constraints = do
  someSecrets <- take maxSecrets <$> secrets constraints
  
  case someSecrets of
    [secret] -> return secret
    _ -> do
      sc <- candidates $ map fst constraints
      
      let someCandidates = take maxCandidates $ someSecrets ++ sc
          xs = zip someCandidates $ repeat someSecrets
          hints = map (\(c,s) -> (c, h c s)) xs
      
      return $ fst $ maximumBy (comparing $ score . snd) hints
  
  where
    h _         []          = []
    h candidate (secret:xs) = ?hint secret candidate : h candidate xs

fastGuess :: _ => Combination c => [(c,Hint)] -> Env c
fastGuess constraints = if null constraints
  then random
  else head <$> secrets constraints
  
score :: [Hint] -> Int
score = S.size . S.fromList
