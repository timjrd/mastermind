module Autoplay where

import Data.Function ((&))
import Control.Monad (forM, forM_, replicateM)
import System.Random (newStdGen)

import Mastermind.Env
import Mastermind.Hint
import Mastermind.Combination
import Mastermind.Guess

autoplayN :: ( Integral i
            , ?powers :: [i]
            , _ )
         => Int -> IO ()
autoplayN n = do
  res <- forM [1..n] $ \i -> do
    putStr $ "[" ++ show i ++ "/" ++ show n ++ "] "
    autoplay

  let avg = fromIntegral (sum res) / fromIntegral n
  putStrLn $ "Average score for " ++ show n ++ " games: " ++ show avg
  
autoplay :: ( Integral i
            , ?powers :: [i]
            , _ )
         => IO Int
autoplay = do
  g <- newStdGen
  
  let game = runEnv g $ do
        secret <- random
        trace  <- play secret []
        return (secret,trace)

      score = length $ snd game

  putStrLn $ "New game with secret " ++ (game & fst & toList & show) ++ ":"
  
  forM_ (zip [1..] $ snd game) $ \(i,(c,h)) -> do
    putStrLn $ "  " ++ show i ++ ". " ++ show h ++ " <- " ++ show (toList c)

  putStrLn $ "Secret found by " ++ show score ++ " guesses.\n"

  return score

play :: _ => Combination c => c -> [(c,Hint)] -> Env [(c,Hint)]
play secret constraints = do
  g <- guess constraints
  let h = ?hint secret g
  cs <- if good h == ?holes
        then return []
        else play secret $ (g,h) : constraints
  return $ (g,h) : cs
