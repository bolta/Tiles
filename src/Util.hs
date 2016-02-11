module Util where

import Debug.Trace (trace)

tr :: (Show a) => a -> a
tr value = trace (show value) value

