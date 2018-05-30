import Control.Monad
import System.Random (getStdGen)

import Util
import Env
import Param

import Combination
import qualified CombinationSet as S
import Candidate
import Secret
import Hint

param = stdParam { colors = 10
                 , holes  = 4 }

main = do
  g  <- getStdGen
  print $ runEnv param g $ do
    secret <- random
    r      <- random
    h      <- hint secret r
    xs     <- secrets [(r,h)]
    forM xs toList
