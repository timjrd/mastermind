import System.Environment (getArgs)

import Mastermind.Env
import Mastermind.Hint
import qualified Mastermind.Combination.Compact     as C
import qualified Mastermind.Combination.Set.Compact as S
import qualified Mastermind.Combination.Big         as C
import qualified Mastermind.Combination.Set.Big     as S
import qualified Data.IntSet as IS

import Autoplay
import Options

_colors = Option "colors" (6::Int)  "number of distinct colors"
_holes  = Option "holes"  (4::Int)  "number of holes / code length"
_times  = Option "times"  (10::Int) "number of games"

defaults = []
  ... _colors
  ... _holes
  ... _times

main :: IO ()
main = withOptions defaults $ \opts ->
  let
    ?colors = opts !!! _colors
    ?holes  = opts !!! _holes
    ?combination = C.big -- C.compact
    ?set         = S.big -- S.compact
  in let
    ?cardinality = cardinality :: Integer
    ?powers      = powers      :: [Integer]
    ?allColors   = IS.fromList [0..(?colors-1)]
  in let
    ?hint = stdHint
  in do
    autoplayN $ opts !!! _times
