module Mastermind.Combination
  ( Combination
  , random
  , fromList
  , toList ) where

import Mastermind.Env (Env)

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
