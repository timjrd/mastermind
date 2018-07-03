module Mastermind.Permutation
  ( hintPermutations
  , HintTag(G,B,W)
  , HintPermutation ) where

import Debug.Trace

import Data.Function ((&))
import Control.Category ((>>>))
import Data.Maybe
import Data.Tree
import Data.List hiding (insert)
import Data.List.Split
import qualified Data.IntMap as M
import Control.Monad.Random (interleave)
import System.Random.Shuffle

import Mastermind.Util
import Mastermind.Env
import Mastermind.Combination
import Mastermind.Hint

type Index = Int
type Color = Int

data HintTag = G | B | W
  deriving (Eq,Show)

-- Color -> Index -> Tag
type TagMap = M.IntMap (M.IntMap HintTag)

type HintPermutation = [HintTag]

hintPermutations :: ( Integral i
                    , Combination c
                    , (?powers :: [i])
                    , _ )
                 => [(c,Hint)] -> Env [[HintPermutation]]
hintPermutations constraints = do
  shuffled <- shuffleM $ zip [0..] constraints
  let (is,xs) = unzip shuffled
      (cs,hs) = unzip xs
      colors  = concatMap toList cs
  mapM (tree . mkTags) hs
    <&> concatTrees
    <&> prune colors
    -- <&> addM & join
    -- <&> pruneM colors
    <&> enumerate (length constraints)
    <&> map ( zip is
              >>> sortOn fst
              >>> map snd )

tree :: [HintTag] -> Env (Forest HintTag)
tree tags = interleave $
  mapM f (nub tags) >>= shuffleM
  where
    f tag = do
      children <- tree $ delete tag tags
      return $ Node tag children

concatTrees :: [Forest HintTag] -> Forest HintTag
concatTrees = foldr concat2Trees []

concat2Trees :: Forest HintTag -> Forest HintTag -> Forest HintTag
concat2Trees a b = map f a
  where
    f (Node tag []      ) = Node tag b
    f (Node tag children) = Node tag $ concat2Trees children b  

prune :: _ => [Color] -> Forest HintTag -> Forest HintTag
prune colors nodes = prune' 0 colors M.empty nodes

prune' :: _ => Int -> [Color] -> TagMap -> Forest HintTag -> Forest HintTag
prune' _ [] _ _ = []
prune' index (color:colors) mp nodes = catMaybes $ map f nodes
  where
    f (Node tag children) =
      if not valid
      then Nothing
      else Just $ Node tag children'
      where
        index'    = (index+1) `mod` ?holes
        mp'       = insertTag mp color index tag
        children' = prune' index' colors mp' children        

        valid = case tag of
          G -> sameColorIs (\x -> x==G||x==B) && sameColorIndexIs (==G)
          B -> sameColorIs (\x -> x==G||x==B) && sameColorIndexIs (==B)
          W -> sameColorIs (==W)
        
        sameColorIs p = and $ map p $ mp `ofColor` color
        sameColorIndexIs p = maybe True p $ ofColorIndex mp color index                    

ofColor :: TagMap -> Color -> [HintTag]
ofColor mp color = maybe [] M.elems $ M.lookup color mp

ofColorIndex :: TagMap -> Color -> Index -> Maybe HintTag
ofColorIndex mp color index = M.lookup color mp >>= M.lookup index

insertTag :: TagMap -> Color -> Index -> HintTag -> TagMap
insertTag mp color index tag = M.insertWith M.union color (M.singleton index tag) mp

-- addM :: [Node] -> Env [Node]
-- addM nodes = interleave $ do
--   withM <- mapM f nodes
--   shuffleM $ concat withM
--   where
--     f (Node tag children) = do
--       children' <- addM children
--       let node  = Node tag children'
--           mNode = if tag == B
--                   then [Node M children']
--                   else []
--       return $ node:mNode

-- pruneM :: _ => [Color] -> [Node] -> [Node]
-- pruneM colors nodes = pruneM' ?holes colors [] [] [] nodes

-- pruneM' :: _ => Int -> [Color] -> [Color] -> [Color] -> [Color] -> [Node] -> [Node]
-- pruneM' 0 colors _  _  _  nodes = pruneM colors nodes
-- pruneM' n colors gs bs ms nodes = catMaybes $ map (pruneM'' n colors gs bs ms) nodes

-- pruneM'' :: _ => Int -> [Color] -> [Color] -> [Color] -> [Color] -> Node -> Maybe Node
-- pruneM'' n (color:colors) gs bs ms (Node tag children) =
--   if not valid
--   then Nothing
--   else Just $ Node tag $ pruneM' (n-1) colors gs' bs' ms' children
--   where
--     (gs', bs', ms') = case tag of
--       G -> (color:gs,       bs,       ms)
--       B -> (      gs, color:bs,       ms)
--       M -> (      gs,       bs, color:ms)
--       W -> (      gs,       bs,       ms)
      
--     validNode = case tag of
--       G -> color `notElem` bs
--       B -> color `notElem` bs && color `notElem` gs
--       M -> True
--       W -> True
      
--     isNode = n > 1
    
--     validLeaf = and $ map v ms'
--     v m = m `elem` gs' || m `elem` bs'
                
--     valid = validNode && (isNode || validLeaf)

enumerate :: _ => Int -> Forest HintTag -> [[HintPermutation]]
enumerate nbConstraints nodes =
  map (chunksOf ?holes) $ enumerateTree (nbConstraints * ?holes) nodes

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

mkTags :: _ => Hint -> [HintTag]
mkTags x = replicate (good x) G
           ++ replicate (bad x) B
           ++ replicate (?holes - good x - bad x) W
