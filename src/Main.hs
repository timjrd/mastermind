import System.Random (StdGen, getStdGen)
import Data.Maybe

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
  => Env _
f = do
  let secret = fromList $ 1:7:2: take (?holes-3) [0..]
      r      = fromList $ 0:1:3: take (?holes-3) [0..]
      h      = ?hint secret r
  xs <- secrets [(r,h)]
  return $ map toList xs

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
  in let
    ?hint   = stdHint
    ?secret = stdSecret
  in runEnv g f
  
main :: IO ()
main = do
  g <- getStdGen
  print $ run g
