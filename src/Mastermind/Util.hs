module Mastermind.Util where

import Data.Tree

import Debug.Trace

powersOf :: Num n => n -> [n]
powersOf a = f 1
  where f x = x : f (x*a)

both :: (a -> b) -> (a,a) -> (b,b)
both f (x,y) = (f x, f y)

infixl 1 <&>
(<&>) :: Functor f => f a -> (a -> b) -> f b
a <&> f = fmap f a

zip4 :: [a] -> [b] -> [c] -> [d] -> [(a,b,c,d)]
zip4 []     _      _      _      = []
zip4 _      []     _      _      = []
zip4 _      _      []     _      = []
zip4 _      _      _      []     = []
zip4 (a:as) (b:bs) (c:cs) (d:ds) = (a,b,c,d) : zip4 as bs cs ds

uncurry4 :: (a -> b -> c -> d -> e) -> (a,b,c,d) -> e
uncurry4 f (a,b,c,d) = f a b c d

uncurry3 :: (a -> b -> c -> d) -> (a,b,c) -> d
uncurry3 f (a,b,c) = f a b c

boolToMaybe :: Bool -> Maybe ()
boolToMaybe True  = Just ()
boolToMaybe False = Nothing

-- enumerate :: [[a]] -> [[a]]
-- enumerate []     = [[]]
-- enumerate (x:xs) = concat $ map f x
--   where f y = map (y:) (enumerate xs)

enumerateTree :: Int -> Forest a -> [[a]]
enumerateTree n nodes =
  concatMap (f n) nodes
  where
    f 1 (Node label []) = [[label]]
    f _ (Node _     []) = []
    f n (Node label children) = map (label:) $
      concatMap (f $ n-1) children

debug msg x = trace (msg ++ ": " ++ show x) x
