module Mastermind.Secret
  ( genericSecrets
  , genericSecretsOverlap
  , secrets
  , stdSecret
  , valid ) where

import Data.Function ((&))
import Control.Monad
import Data.Maybe
import Data.List (sortOn, partition)
import System.Random.Shuffle
import qualified Data.IntSet as S

import Mastermind.Util
import Mastermind.Env
import Mastermind.Combination
import Mastermind.Combination.Set (Set, empty, member, insert)
import Mastermind.Hint

secrets :: _ => Combination c => [(c,Hint)] -> Env [c]
secrets constraints = do
  (x,n0) <- fromJust <$> secret constraints empty maxBound
  xs <- f (x `insert` empty) (n0*16)
  return $ x:xs
  where
    f excluded n = do
      mx <- secret constraints excluded (n*8)
      case mx of
        Nothing       -> return []
        (Just (x,n')) -> do
          xs <- f (x `insert` excluded) (max n n')
          return $ x:xs

secret :: _ => Set s c => [(c,Hint)] -> s -> Int -> Env (Maybe (c,Int))
secret constraints excluded trials = g 1
  where
    g n =
      if n > trials
      then return Nothing
      else do
        xs <- mapM (uncurry ?secret) constraints
        let result = map (valid constraints) xs
                     & filter isJust
                     & map fromJust
                     & listToMaybe
        case result of
          Nothing  -> g $ n+1
          (Just x) -> if x `member` excluded
                      then g $ n+1
                      else return $ Just (x,n)


stdSecret :: ( Combination c
             , Integral i
             , ?powers :: [i]
             , _ )
          => c -> Hint -> Env c
stdSecret x y = do
  let x' = toList x
  
  shuffled <- shuffleM (zip [0..] x')
  
  let (goods,badsWrongs) = splitAt (good y) shuffled
      (bads,wrongs)      = splitAt (bad  y) badsWrongs
      (badsWrongs1,badsWrongs2) = unzip badsWrongs
      bads2    = map snd bads
      wrongs2  = map snd wrongs
      wrongSet = S.fromList wrongs2
      
  let notWrong = do
        r <- randomColor
        if r `S.member` wrongSet
          then notWrong
          else return r

  let mNotBadsWrongs2 = do
        notWrongs2 <- mapM (\_ -> notWrong) badsWrongs2
        mv <- shuffleDistinct bads2 notWrongs2
        case mv of
          Nothing -> mNotBadsWrongs2
          Just v  -> return v

  notBadsWrongs2 <- mNotBadsWrongs2
  
  let notBadsWrongs = zip badsWrongs1 notBadsWrongs2
      result = map snd $ sortOn fst $ goods ++ notBadsWrongs

  return $ fromList result
  

shuffleDistinct :: (Eq a, Show a) => [a] -> [a] -> Env (Maybe [a])
shuffleDistinct refs xs = do
  let refs' = map Just refs ++ repeat Nothing
      xs'   = map Just xs
  shuffled <- shuffleM xs'
  if or $ map (uncurry (==)) $ zip refs' shuffled
    then return Nothing
    else return $ Just $ map fromJust shuffled

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

valid :: _ => Combination c
      => [(c,Hint)] -> c -> Maybe c
valid constraints secret =
  if isValid
    then Just secret
    else Nothing
  where
    isValid = and $ map f constraints
    f (x,y) = y == ?hint secret x
  
