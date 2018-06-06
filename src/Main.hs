import System.Random (StdGen, getStdGen)

import Mastermind.Util
import Mastermind.Env

import Mastermind.Combination
import qualified Mastermind.Combination.Compact as C
import qualified Mastermind.Combination.Set
import qualified Mastermind.Combination.Set.Compact as S
import Mastermind.Candidate
import Mastermind.Secret
import Mastermind.Hint

f :: ( Combination c
     , Integral i
     , ?combination :: c
     , ?powers      :: [i]
     , _ )
  => Env [[Int]]
f = do
  let secret = fromList [1,2,3,4]
      r      = fromList [1,2,4,3]
      h      = ?hint secret r
  xs <- genericSecrets [(r,h)] 
  return $ map toList xs

run :: StdGen -> [[Int]]
run g =
  let
    ?colors = 8
    ?holes  = 4
    ?combination = C.compact
    ?set         = S.compact
  in let
    ?cardinality = cardinality :: Int
    ?powers      = powers :: [Int]
  in let
    ?hint = stdHint
  in runEnv g f
  
main :: IO ()
main = do
  g <- getStdGen
  print $ run g
