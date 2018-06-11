module Mastermind.Permutation where

import Data.Maybe
import Data.List
import Control.Monad.Random
import System.Random.Shuffle

import Mastermind.Env
import Mastermind.Combination
import Mastermind.Hint

type Color = Int

data Tag = G | B | M | W
  deriving (Eq)

type Permutation = [Tag]

data Node = Node Tag [Node]

permutations :: _ => Combination c => [(c,Hint)] -> Env [Permutation]
permutations constraints = do
  trees <- mapM (uncurry permutationTree) constraints
  -- TODO
  return []

permutationTree :: _ => Combination c => c -> Hint -> Env [Node]
permutationTree combination hint =
  pruneNodes (toList combination) [] [] []
  <$> nodes (mkTags hint)

nodes :: [Tag] -> Env [Node]
nodes tags = interleave $ do
  nodes <- mapM f (nub tags)
  shuffleM $ concat nodes
  where
    f tag = do
      children <- nodes $ delete tag tags
      let node  = Node tag children
          mNode = if tag == B
                  then [Node M children]
                  else []
      return $ [node] ++ mNode

pruneNodes :: [Color] -> [Color] -> [Color] -> [Color] -> [Node] -> [Node]
pruneNodes colors gs bs ms nodes =
  catMaybes $ map (pruneNode colors gs bs ms) nodes

pruneNode :: [Color] -> [Color] -> [Color] -> [Color] -> Node -> Maybe Node
pruneNode (color:colors) gs bs ms (Node tag children) =
  let
    (gs', bs', ms') = case tag of
      G -> (color:gs,       bs,       ms)
      B -> (      gs, color:bs,       ms)
      M -> (      gs,       bs, color:ms)
      W -> (      gs,       bs,       ms)
      
    validNode = case tag of
      G -> color `notElem` bs
      B -> color `notElem` bs && color `notElem` gs
      M -> True
      W -> True
      
    isNode = not $ null colors
    
    validLeaf = (not $ null $ intersect gs' ms')
                || (not $ null $ intersect bs' ms')
                
    valid = validNode && (isNode || validLeaf)
  in
    if not valid
    then Nothing
    else Just $ Node tag $ pruneNodes colors gs' bs' ms' children

mkTags :: _ => Hint -> [Tag]
mkTags x = replicate (good x) G
           ++ replicate (bad x) B
           ++ replicate (?holes - good x - bad x) W
