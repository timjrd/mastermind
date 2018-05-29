module CombinationSet ( CombinationSet
                      , empty
                      , fromList
                      , member
                      , notMember
                      , insert ) where

import qualified Data.IntSet as IntSet

import Internal (Combination(Combination))

newtype CombinationSet = CombinationSet IntSet.IntSet

empty :: CombinationSet
empty = CombinationSet IntSet.empty

fromList :: [Combination] -> CombinationSet
fromList xs =
  CombinationSet $ IntSet.fromList $ map (\(Combination x) -> x) xs

member :: Combination -> CombinationSet -> Bool
member (Combination x) (CombinationSet set) =
  IntSet.member x set

notMember :: Combination -> CombinationSet -> Bool
notMember (Combination x) (CombinationSet set) =
  IntSet.notMember x set

insert :: Combination -> CombinationSet -> CombinationSet
insert (Combination x) (CombinationSet set) =
  CombinationSet $ IntSet.insert x set
