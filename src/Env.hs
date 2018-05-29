module Env ( Env
           , Param(Param)
           , colors
           , holes
           , hint
           , runEnv
           , ask
           , getRandomR ) where

import Control.Monad.Reader
import Control.Monad.Random

import Internal

runEnv :: Param -> StdGen -> Env a -> a
runEnv param generator x =
  evalRand (runReaderT x param) generator
