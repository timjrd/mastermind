import Test
import Autoplay

import Mastermind.Env
import Mastermind.Hint
import qualified Mastermind.Combination.Compact as CC
import qualified Mastermind.Combination.Set.Compact as CS
import qualified Data.IntSet as IS

main :: IO ()
main =
  let
    ?colors = 9
    ?holes  = 9
    ?combination = CC.compact
    ?set         = CS.compact
  in let
    ?cardinality = cardinality :: Int
    ?powers      = powers :: [Int]
    ?allColors   = IS.fromList [0..(?colors-1)]
  in let
    ?hint = stdHint
  in
    autoplay
