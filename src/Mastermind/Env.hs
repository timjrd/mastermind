module Mastermind.Env where

import Control.Monad.Random

import Util

type Env a = Rand StdGen a

cardinality :: _ => Integral i => i
cardinality = fromIntegral ?colors ^ fromIntegral ?holes

powers :: _ => Integral i => [i]
powers = take ?holes $ powersOf $ fromIntegral ?colors

runEnv :: StdGen -> Env a -> a
runEnv g x = evalRand x g
