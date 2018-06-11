import System.Random (StdGen, getStdGen)
import Data.Maybe
import Control.Monad

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

f :: ( Combination c
     , Integral i
     , ?combination :: c
     , ?powers      :: [i]
     , _ )
  => Env _
f = do
  secret <- random

  cs <- replicateM 3 $ do
    r <- random
    let h = ?hint secret r
    return (r,h)

  xs <- take 100 <$> secrets cs
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
