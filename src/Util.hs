module Util where

import Data.List

powersOf a = f 1
  where f x = x : f (x*a)
