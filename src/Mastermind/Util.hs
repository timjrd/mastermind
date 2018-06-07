module Mastermind.Util where

powersOf :: Num n => n -> [n]
powersOf a = f 1
  where f x = x : f (x*a)

both :: (a -> b) -> (a,a) -> (b,b)
both f (x,y) = (f x, f y)
