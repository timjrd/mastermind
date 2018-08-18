module Mastermind.Combination.Set.Big
  ( Big
  , big ) where

import qualified Data.Set as S

import Mastermind.Combination.Set
import qualified Mastermind.Combination.Big as C

newtype Big = Big (S.Set C.Big)
  deriving (Eq)

instance Set Big C.Big where
  empty    = Big $ S.empty
  fromList = Big . S.fromList
  member x (Big s) = S.member x s
  insert x (Big s) = Big $ S.insert x s

big = undefined :: Big
