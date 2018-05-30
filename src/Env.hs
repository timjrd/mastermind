module Env ( Env
           , runEnv
           , colors
           , holes
           , cardinality
           , powers
           , hint
           , asks
           , getRandomR ) where

import Control.Monad.Reader
import Control.Monad.Random

import Internal

import Util

hint a b = do
  f <- asks hintFn
  f a b

runEnv :: Param -> StdGen -> Env a -> a
runEnv param generator x =
  evalRand (runReaderT x (deriveParam param)) generator

deriveParam :: Param -> Param
deriveParam x = x
  { cardinality = colors x ^ holes x
  , powers      = take (holes x) $ powersOf (colors x) }
