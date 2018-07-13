module Mastermind.Hint
  ( Hint
  , good
  , bad
  , stdHint ) where

import qualified Data.IntSet as S

import Mastermind.Env
import Mastermind.Combination

data Hint = Hint { good :: Int
                 , bad  :: Int }
  deriving (Eq, Ord)

instance Show Hint where
  show (Hint g b) = show (g,b)

stdHint :: ( Integral i
           , Combination c
           , ?powers :: [i]
           , _ )
        => c -> c -> Hint
stdHint secret x =
  let secret'   = toList secret
      x'        = toList x
      secretSet = S.fromList secret'
      zipped    = zip secret' x'
      goods     = map g zipped
      bads      = map b zipped
      g (s,a)   = fromEnum $ a == s
      b (s,a)   = fromEnum $ a /= s && S.member a secretSet
  in Hint (foldl (+) 0 goods) (foldl (+) 0 bads)
