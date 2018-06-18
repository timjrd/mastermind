module Mastermind.Permutation where

import Data.Maybe
import Data.List
import Control.Monad.Random
import System.Random.Shuffle

import Mastermind.Util
import Mastermind.Env
import Mastermind.Combination
import Mastermind.Hint

type Color = Int

data Tag = G | B | M | W
  deriving (Eq,Show)

type Permutation = [Tag]

data Node = Node Tag [Node]

type PermutationNodes = (Permutation, [Node])

instance Show Node where
  show (Node tag _) = show tag

permutations :: ( Integral i
                , Combination c
                , (?powers :: [i])
                , _ )
             => [(c,Hint)] -> Env [[Permutation]]
permutations constraints = do
  trees <- mapM (uncurry permutationTree) constraints
  let (cs,_) = unzip constraints
      all = allPermutations trees
      fil = filter (areConsistent cs) all 
  return fil

areConsistent :: _ => Combination c => [c] -> [Permutation] -> Bool
areConsistent cs ps = and $ map f1 prod1
  where
    cs'   = map toList cs
    is    = repeat [0..]
    all   = zip3 is cs' ps
    all'  = map (uncurry3 zip3) all
    prod1 = [(a,b) | a <- all', b <- all']
    f1 (a,b) = and $ map f2 [(x,y) | x <- a, y <- b]
    f2 ((i1,c1,t1), (i2,c2,t2)) = case (t1,t2) of
      (G,G) -> i1 /= i2 || c1 == c2
      (G,B) -> i1 /= i2 || c1 /= c2
      (G,M) -> i1 /= i2 || c1 /= c2
      (G,W) -> i1 /= i2 || c1 /= c2
      (B,G) -> i1 /= i2 || c1 /= c2
      (B,B) -> True
      (B,M) -> True
      (B,W) -> i1 /= i2 || c1 /= c2
      (M,G) -> i1 /= i2 || c1 /= c2
      (M,B) -> True
      (M,M) -> True
      (M,W) -> i1 /= i2 || c1 /= c2
      (W,G) -> i1 /= i2 || c1 /= c2
      (W,B) -> i1 /= i2 || c1 /= c2
      (W,M) -> i1 /= i2 || c1 /= c2
      (W,W) -> True
    
allPermutations :: _ => [[Node]] -> [[Permutation]]
allPermutations trees = f os
  where
    os     = map nextPermutation trees
    f xs   = let (ps,xs',end) = nextPermutations os xs
             in if end
                then [ps]
                else ps : f xs'

nextPermutations :: _ => [PermutationNodes] -> [PermutationNodes]
                 -> ([Permutation], [PermutationNodes], Bool)
nextPermutations []     []         = ([],[],True)
nextPermutations ((p0,x0):os) ((p,x):xs) =
  if next
  then if null x || null p'
       then (p0:ps, (p0,x0):os, True)
       else (p':ps, (p',x'):xs', False)
  else (p:ps, (p,x):xs', False)
  where
    (ps,xs',next) = nextPermutations os xs
    (p',x')       = nextPermutation x

nextPermutation :: _ => [Node] -> (Permutation, [Node])
nextPermutation [] = ([],[])
nextPermutation x  =
  let v@(p,x') = nextPermutation' x
  in if length p < ?holes
  then nextPermutation x'
  else v

nextPermutation' :: [Node] -> (Permutation, [Node])
nextPermutation' []                             = ([],[])
nextPermutation' ((Node tag children):siblings) = (tags',siblings')
  where
    (tags,children') = nextPermutation' children
    tags'            = tag:tags
    siblings'        = if   null children'
                       then siblings
                       else Node tag children' : siblings

-- -- FLAWED
-- allPermutations :: _ => Int -> [[Node]] -> [[Permutation]]
-- allPermutations n trees =
--   if n > ?holes
--   then [[ [] ]]
--   else if abort
--   then []
--   else res
--   where
--     res  = map concat $ transpose $
--            map f $ enumerate trees
--     f xs = let (tags,subtrees) = unzip $ map unnode xs
--                ps = allPermutations (n+1) $ subtrees
--            in  map prepend $ zip tags ps
--     prepend (tag, ys) = map (tag:) ys
--     unnode (Node tag nodes) = (tag,nodes)
--     abort = or (map null trees)

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
    
    validLeaf = and $ map v ms'
    v m = m `elem` gs' || m `elem` bs'
                
    valid = validNode && (isNode || validLeaf)
  in
    if not valid
    then Nothing
    else Just $ Node tag $ pruneNodes colors gs' bs' ms' children

mkTags :: _ => Hint -> [Tag]
mkTags x = replicate (good x) G
           ++ replicate (bad x) B
           ++ replicate (?holes - good x - bad x) W
