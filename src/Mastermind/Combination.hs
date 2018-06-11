module Mastermind.Combination where

import Control.Monad.Random

import Mastermind.Env

class Combination c where
  random   :: ( Integral i
              , ?combination :: c
              , ?cardinality :: i )
           => Env c
  
  fromList :: ( Integral i
              , ?combination :: c
              , ?colors      :: Int
              , ?holes       :: Int
              , ?powers      :: [i] )
           => [Int] -> c
           
  toList   :: ( Integral i
              , ?combination :: c
              , ?colors      :: Int
              , ?holes       :: Int
              , ?powers      :: [i] )
           => c -> [Int]

randomColor :: _ => Env Int
randomColor = getRandomR (0, ?colors - 1)
  
