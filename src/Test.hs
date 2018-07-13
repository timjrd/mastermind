module Test (runTests, tests) where

import System.Random (newStdGen)
import Data.Function ((&))
import Data.Maybe
import Data.List
import Control.Monad
import System.IO
import qualified Data.Set as S

import Mastermind.Util
import Mastermind.Env
import Mastermind.Combination
import Mastermind.Candidate
import Mastermind.Secret
import Mastermind.Hint
import Mastermind.Permutation

type Test = (String, IO (Maybe String))

testSecrets :: _ => Int -> IO (Maybe String)
testSecrets 0 = return Nothing
testSecrets n = do
  g <- newStdGen
  case runEnv g testSecrets' of
    Nothing  -> testSecrets (n-1)
    Just msg -> return $ Just msg

testSecrets' :: ( Combination c
                , Integral i
                , ?combination :: c
                , ?powers      :: [i]
                , _ )
             => Env (Maybe String)
testSecrets' = do
  secret <- random

  cs <- replicateM 10 $ do
    r <- random
    let h = ?hint secret r
    return (r,h)

  fast' <- secrets cs
  slow' <- genericSecrets cs

  let fast = S.fromList $ map toList fast'
      slow = S.fromList $ map toList slow'
  
  return $ if fast == slow -- && length fast' == S.size fast
    then Nothing
    else Just $ unlines $
         [ "secret         : " ++ (show $ toList secret)
         , "guesses        : " ++ (show $ map (toList . fst) cs)
         , "good           : " ++ (show $ S.toList $ S.intersection fast slow)
         , "not found      : " ++ (show $ S.toList $ slow S.\\ fast)
         , "false positives: " ++ (show $ S.toList $ fast S.\\ slow) ]
         
tests :: _ => [Test]
tests = [ ("secrets", testSecrets 200) ]

runTests :: [Test] -> IO ()
runTests xs = forM_ xs $ \(name,f) -> do
  putStr $ "[  ..  ] " ++ name ++ " "
  hFlush stdout
  result <- f
  case result of
    Nothing  -> putStrLn $ "\r[  ok  ] " ++ name
    Just msg -> putStrLn $ "\r[FAILED] " ++ name ++ "\n"
                ++ (lines msg & map ("  "++) & unlines)
