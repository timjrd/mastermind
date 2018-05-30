import System.Random (getStdGen)

import Util
import Env
import Param

import Combination
import qualified CombinationSet as S
import Candidate
import Secret
import Hint

param = stdParam { colors = 8
                 , holes  = 4 }

main = do
  g  <- getStdGen
  print $ runEnv param g $ do
    secret <- random
    r1     <- random
    r2     <- random
    h1     <- hint secret r1
    h2     <- hint secret r2
    xs     <- secrets [(r1,h1), (r2,h2)] 
    mapM toList xs
