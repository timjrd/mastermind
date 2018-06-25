module Mastermind.Permutation where

import Debug.Trace

import Data.Function ((&))
import Control.Category ((>>>))
import Data.Maybe
import Data.List hiding (insert)
import Data.List.Split
import Data.Map (Map, empty, insert, (!?))
import Control.Monad.Random hiding (next)
import System.Random.Shuffle

import Mastermind.Util
import Mastermind.Env
import Mastermind.Combination
import Mastermind.Hint

type Index = Int

type Color = Int

data Tag = G | B | W
  deriving (Eq,Show)

type Permutation = [Tag]

data Node = Node Tag [Node]

instance Show Node where
  show (Node tag _) = show tag

permutations :: ( Integral i
                , Combination c
                , (?powers :: [i])
                , _ )
             => [(c,Hint)] -> Env [[Permutation]]
permutations constraints = do
  shuffled <- shuffleM $ zip [0..] constraints
  let (is,xs) = unzip shuffled
      (cs,hs) = unzip xs
      colors  = concatMap toList cs
  trees <- mapM (tree . mkTags) hs
  return $
    concatTrees trees
    & prune colors
    & enumerate (length constraints)
    & map ( zip is
            >>> sortOn fst
            >>> map snd )

tree :: [Tag] -> Env [Node]
tree tags = interleave $
  mapM f (nub tags) >>= shuffleM
  where
    f tag = do
      children <- tree $ delete tag tags
      return $ Node tag children

concatTrees :: [[Node]] -> [Node]
concatTrees = foldr concat2Trees []

concat2Trees :: [Node] -> [Node] -> [Node]
concat2Trees a b = map f a
  where
    f (Node tag []      ) = Node tag b
    f (Node tag children) = Node tag $ concat2Trees children b  

prune :: _ => [Color] -> [Node] -> [Node]
prune colors nodes = prune' 0 colors empty nodes

prune' :: _ => Int -> [Color] -> Map (Index,Color) Tag -> [Node] -> [Node]
prune' _ [] _ _ = []
prune' index (color:colors) mp nodes = catMaybes $ map f nodes
  where
    f (Node tag children) =
      let
        isValid   = case mp !? (index,color) of
          (Just tag') -> tag' == tag
          Nothing     -> True        
        index'    = (index+1) `mod` ?holes
        mp'       = insert (index,color) tag mp
        children' = prune' index' colors mp' children        
      in
        if isValid
        then Just $ Node tag children'
        else Nothing

enumerate :: _ => Int -> [Node] -> [[Permutation]]
enumerate nbConstraints nodes =
  map (chunksOf ?holes) $ concatMap (f $ nbConstraints * ?holes) nodes
  where
    f 1 (Node tag []) = [[tag]]
    f _ (Node _   []) = []
    f n (Node tag children) = map (tag:) $
      concatMap (f $ n-1) children

-- enumerate :: _ => Int -> [Node] -> [[Permutation]]
-- enumerate _ [] = []
-- enumerate nbConstraints x =
--   let (p,x') = next nbConstraints x
--   in p : enumerate nbConstraints x'

-- next :: _ => Int -> [Node] -> ([Permutation], [Node])
-- next _ [] = ([],[])
-- next nbConstraints x =
--   let (p,x') = next' x
--   in if length p /= ?holes * nbConstraints
--      then next nbConstraints x'
--      else (chunksOf ?holes p, x')

-- next' :: [Node] -> (Permutation, [Node])
-- next' []                             = ([],[])
-- next' ((Node tag children):siblings) = (tags',siblings')
--   where
--     (tags,children') = next' children
--     tags'            = tag:tags
--     siblings'        = if   null children'
--                        then siblings
--                        else Node tag children' : siblings

mkTags :: _ => Hint -> [Tag]
mkTags x = replicate (good x) G
           ++ replicate (bad x) B
           ++ replicate (?holes - good x - bad x) W
