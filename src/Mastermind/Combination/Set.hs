module Mastermind.Combination.Set
  ( Set
  , empty
  , fromList
  , member
  , insert
  , notMember ) where

import Mastermind.Combination (Combination)

class Combination c => Set s c where
  empty     :: (?combination :: c, ?set :: s) => s
  fromList  :: (?combination :: c, ?set :: s) => [c] -> s
  member    :: (?combination :: c, ?set :: s) => c -> s -> Bool
  insert    :: (?combination :: c, ?set :: s) => c -> s -> s

  notMember :: (?combination :: c, ?set :: s) => c -> s -> Bool
  notMember c s = not (member c s)
