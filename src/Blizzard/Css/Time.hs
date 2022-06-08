{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Blizzard.Css.Time
    ( Time
    , ms
    , s
    ) where


import Control.Arrow ((>>>))
import Data.Text (pack)

import Blizzard.Css.Common (Auto, Inherit, None, Normal)
import Blizzard.Css.Property (Val, Value, value)


newtype Time = Time Value
    deriving (Auto, Inherit, None, Normal, Val)


instance Num Time where
    fromInteger = s . fromInteger
    (+)         = error "plus not implemented for Time"
    (*)         = error "times not implemented for Time"
    abs         = error "abs not implemented for Time"
    signum      = error "signum not implemented for Time"
    negate      = error "negate not implemented for Time"


instance Fractional Time where
    fromRational = s . fromRational
    recip        = error "recip not implemented for Time"


ms :: Double -> Time
ms
     =  show
    >>> pack
    >>> (<> "ms")
    >>> value
    >>> Time


s :: Double -> Time
s
     =  show
    >>> pack
    >>> (<> "s")
    >>> value
    >>> Time
