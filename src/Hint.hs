module Hint ( Hint
            , stdHint ) where

import qualified Data.IntSet as S

import Internal (Hint(Hint))

import Env

import Combination

stdHint :: Combination -> Combination -> Env Hint
stdHint secret x = do
  secret' <- toList secret
  x'      <- toList x
  let secretSet  = S.fromList secret'
      zipped     = zip secret' x'
      goods      = map good zipped
      bads       = map bad  zipped
      good (s,a) = fromEnum $ a == s
      bad  (s,a) = fromEnum $ a /= s && S.member a secretSet
  return $ Hint (foldl (+) 0 goods) (foldl (+) 0 bads)
