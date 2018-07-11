module Mastermind.Combination.Set.Compact
  ( Compact
  , compact ) where

import qualified Data.IntSet as S

import Mastermind.Combination.Set
import qualified Mastermind.Combination.Compact as C
import qualified Mastermind.Combination.Compact.Internal as CI

newtype Compact = Compact S.IntSet
  deriving Eq

instance Set Compact C.Compact where
  empty = Compact S.empty

  fromList xs =
    Compact $ S.fromList $ map (\(CI.Compact x) -> x) xs

  member (CI.Compact x) (Compact set) =
    S.member x set

  insert (CI.Compact x) (Compact set) =
    Compact $ S.insert x set

compact = undefined :: Compact
