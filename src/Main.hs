import Control.Monad
import System.Random

import Util
import Env

import Combination
import CombinationSet
import Candidate
import Secret
import Hint

param = Param { colors = 8
              , holes  = 4
              , hint   = stdHint }

main = do
  g  <- getStdGen
  print $ runEnv param g $ do
    xs <- candidates []
    forM (take 10 xs) toList
