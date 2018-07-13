module Autoplay where

import Data.Function ((&))
import Control.Monad (forM_)
import System.Random (getStdGen)

import Mastermind.Env
import Mastermind.Hint
import Mastermind.Combination
import Mastermind.Guess

autoplay :: ( Integral i
            , ?powers :: [i]
            , _ )
         => IO ()
autoplay = do
  g <- getStdGen
  
  let game = runEnv g $ do
        secret <- random
        trace  <- play secret []
        return (secret,trace)
    
  putStrLn $ "secret " ++ (game & fst & toList & show)
  putStrLn "trace"
  forM_ (zip [1..] $ snd game) $ \(i,(c,h)) -> do
    putStrLn $ "  " ++ show i ++ ". " ++ show (toList c) ++ " " ++ show h

play :: _ => Combination c => c -> [(c,Hint)] -> Env [(c,Hint)]
play secret constraints = do
  g <- guess constraints
  let h = ?hint secret g
  cs <- if good h == ?holes
        then return []
        else play secret $ (g,h) : constraints
  return $ (g,h) : cs
