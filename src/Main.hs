import System.Random (StdGen, getStdGen)
import Data.Maybe
import Data.List
import Control.Monad
import qualified Data.IntSet as IS
import qualified Data.Set as Set

import Mastermind.Util
import Mastermind.Env
import Mastermind.Combination
import qualified Mastermind.Combination.Compact as CC
import qualified Mastermind.Combination.Set as S
import qualified Mastermind.Combination.Set.Compact as CS
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
  --let secret = fromList [0,1]

  cs <- replicateM 3 $ do
    r <- random
    --let r = fromList [1,0]
    let h = ?hint
            (fromList $ debug "secret" $ toList secret)
            r
    return (r,h)

  fast <- Set.fromList <$> map toList <$> secrets (
    (flip traceShow) cs $ map (toList . fst) cs )
  slow <- Set.fromList <$> map toList <$> genericSecrets cs
  
  return $ [ ("good"           , Set.toList $ Set.intersection fast slow)
           , ("not found"      , Set.toList $ slow Set.\\ fast)
           , ("false positives", Set.toList $ fast Set.\\ slow) ]

run :: StdGen -> _
run g =
  let
    ?colors = 8
    ?holes  = 4
    ?combination = CC.compact
    ?set         = CS.compact
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
  forM_ (run g) $ \(title,cs) -> do
    putStrLn title
    mapM_ print cs
    putStrLn ""
