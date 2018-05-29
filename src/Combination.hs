module Combination ( Combination
                   , random
                   , toList ) where

import Internal (Combination(Combination))

import Util
import Env

random :: Env Combination
random = do
  c <- colors <$> ask
  h <- holes  <$> ask
  r <- getRandomR (0, c^h - 1)
  return $ Combination r

toList :: Combination -> Env [Int]
toList (Combination x) = do
  c <- colors <$> ask
  h <- holes  <$> ask
  let powers  = take h $ powersOf c
      f power = ((x `div` power) `mod` c)
  return $ map f powers

-- toListNaturalOrder x = do
--   list <- toList x
--   return $ reverse list
