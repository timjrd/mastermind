module Mastermind.Secret
  ( secrets
  , genericSecrets
  , genericSecretsOverlap ) where

import Data.Function ((&))
import Control.Monad
import Data.Maybe
import Data.List (sortOn, partition)
import Data.Tree
import System.Random.Shuffle
import Control.Monad.Random (interleave)
import qualified Data.IntSet as S

import Mastermind.Util
import Mastermind.Env
import Mastermind.Combination
import Mastermind.Combination.Set (Set, empty, member, insert)
import Mastermind.Hint
import Mastermind.Permutation
import Mastermind.Candidate

type Color = Int

data Constraints = Constraints
                   [Maybe Color] -- positive color  at each index
                   [S.IntSet]    -- negative colors at each index
                   S.IntSet      -- required colors
  deriving Show

secrets :: ( Combination c
           , Integral i
           , ?powers :: [i]
           , _ )
        => [(c,Hint)] -> Env [c]
secrets constraints =
  -- if null constraints
  -- then candidates []
  -- else
    hintPermutations constraints
    >>= mapM (toSecrets colors)
    <&> concat
  where colors = unzip constraints & fst & map toList

toSecrets :: _ => Combination c => [[Color]] -> [HintPermutation] -> Env [c]
toSecrets colors tags =
  zip colors tags
  & map (uncurry zip)
  & toConstraints
  & tree
  <&> enumerateTree ?holes
  <&> map fromList

toConstraints :: _ => [[(Color,HintTag)]] -> Constraints
toConstraints ps =
  foldl appendConstraints emptyConstraints $ map f ps
  where
    f p = Constraints a (map (uncurry S.union) $ zip b $ repeat b') c
      where (a,b,c,b') = foldr g ([], [], S.empty, S.empty) p

    g (color,tag) (a,b,c,b') = case tag of
      G -> (Just color : a,           S.empty : b, S.insert color c, b')
      B -> (   Nothing : a, S.singleton color : b, S.insert color c, b')
      W -> (   Nothing : a,           S.empty : b,                c, S.insert color b')

appendConstraints (Constraints a b c) (Constraints a' b' c') =
  Constraints
  (zip a a' & map (uncurry eitherJust))
  (zip b b' & map (uncurry S.union))
  (S.union c c')
  where
    eitherJust Nothing Nothing = Nothing      
    eitherJust Nothing x       = x
    eitherJust x       Nothing = x
    eitherJust x       _       = x

emptyConstraints :: _ => Constraints
emptyConstraints = Constraints
  (replicate ?holes Nothing)
  (replicate ?holes S.empty)
  S.empty

tree :: _ => Constraints -> Env (Forest Color)
tree ct@(Constraints _ _ c) = tree' ?holes (S.size c) ct

tree' :: _ => Int -> Int -> Constraints -> Env (Forest Color)
tree' _ _    (Constraints []     []     _) = return []
tree' n left (Constraints (a:as) (b:bs) c) = interleave $
  catMaybes <$> mapM node colors >>= shuffleM
  where
    colors = case a of
      Just color -> [color]
      Nothing    -> S.toList $ ?allColors S.\\ b
    
    node color = if not valid
      then return Nothing
      else Just . Node color <$> children
      where
        children = tree' n' left' $ Constraints as bs c'
        n' = n-1
        (left',c') = if S.member color c
                     then (left-1, S.delete color c)
                     else (left, c)
        valid = left' <= n'
          
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
  
