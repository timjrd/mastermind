module Combination ( Combination
                   , random
                   , fromList
                   , toList ) where

import Internal (Combination(Combination))

import Util
import Env

random :: Env Combination
random = do
  card <- asks cardinality
  r    <- getRandomR (0, card-1)
  return $ Combination r

fromList :: [Int] -> Env Combination
fromList xs = do
  pows <- asks powers
  let muls = map (uncurry (*)) (zip pows xs)
  return $ Combination $ foldl (+) 0 muls

toList :: Combination -> Env [Int]
toList (Combination x) = do
  cols <- asks colors
  pows <- asks powers
  let f power = ((x `div` power) `mod` cols)
  return $ map f pows
