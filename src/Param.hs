module Param ( Param
             , colors
             , holes
             , hintFn
             , stdParam ) where

import Internal

import Hint

stdParam = Param { colors      = 8
                 , holes       = 4
                 , hintFn      = stdHint
                 
                 -- the following are derived from the above
                 , cardinality = undefined
                 , powers      = undefined }
