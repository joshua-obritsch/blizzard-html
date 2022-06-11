{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Blizzard.Css.Time
    ( Time
    , ms
    , sec
    ) where


import Data.Text (pack)

import Blizzard.Css.Common
    ( Auto
    , Inherit
    , Initial
    , None
    , Normal
    , Unset
    )
import Blizzard.Css.Property (Val(..), Value)


newtype Time = Time Value
    deriving
        ( Auto
        , Inherit
        , Initial
        , None
        , Normal
        , Unset
        , Val
        )


instance Num Time where
    fromInteger = sec . fromInteger
    (+)         = error "plus not implemented for Time"
    (*)         = error "times not implemented for Time"
    abs         = error "abs not implemented for Time"
    signum      = error "signum not implemented for Time"
    negate      = error "negate not implemented for Time"


instance Fractional Time where
    fromRational = sec . fromRational
    recip        = error "recip not implemented for Time"


ms :: Double -> Time
ms
    = Time
    . value
    . (<> "ms")
    . pack
    . show


sec :: Double -> Time
sec
    = Time
    . value
    . (<> "s")
    . pack
    . show
