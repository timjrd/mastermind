import System.Random (StdGen, getStdGen)
import Data.Maybe
import Control.Monad
import qualified Data.IntSet as IS

import Mastermind.Util
import Mastermind.Env
import Mastermind.Combination
import qualified Mastermind.Combination.Compact as C
import qualified Mastermind.Combination.Set
import qualified Mastermind.Combination.Set.Compact as S
import Mastermind.Candidate
import Mastermind.Secret
import Mastermind.Hint
import Mastermind.Permutation

import Debug.Trace

debugC :: _ => Combination c => String -> c -> c
debugC msg x = trace (msg ++ ": " ++ show (toList x)) x

f :: ( Combination c
     , Integral i
     , ?combination :: c
     , ?powers      :: [i]
     , _ )
  => Env _
f = do
  secret <- random
  --let secret = fromList [0,1,1,1]

  cs <- replicateM 15 $ do
    r <- random
    --let r = fromList [1,1,1,0]
    let h = ?hint secret r
    return (r,h)

  --map (map $ concatMap show) <$> hintPermutations cs
  map toList <$> secrets cs

run :: StdGen -> _
run g =
  let
    ?colors = 9
    ?holes  = 9
    ?combination = C.compact
    ?set         = S.compact
  in let
    ?cardinality = cardinality :: Int
    ?powers      = powers :: [Int]
    ?allColors   = IS.fromList [0..(?colors-1)]
  in let
    ?hint = stdHint
  in runEnv g f
  
main :: IO ()
main = do
  g <- getStdGen
  mapM_ print $ run g
