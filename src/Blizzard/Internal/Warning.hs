module Blizzard.Internal.Warning
    ( warning
    ) where

import Debug.Trace (trace)


warning :: a -> String -> a
warning = flip trace
