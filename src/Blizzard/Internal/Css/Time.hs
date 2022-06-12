{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Blizzard.Internal.Css.Time
    ( Time
    , ms
    , sec
    ) where


import Data.Text (pack)

import Blizzard.Css.Common
    ( Inherit
    , Initial
    , Revert
    , RevertLayer
    , Unset
    )
import Blizzard.Css.Property (Val(..), Value)


newtype Time = Time Value
    deriving
        ( Inherit
        , Initial
        , Revert
        , RevertLayer
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


ms :: (Num a, Show a) => a -> Time
ms
    = Time
    . value
    . (<> "ms")
    . pack
    . show


sec :: (Num a, Show a) => a -> Time
sec
    = Time
    . value
    . (<> "s")
    . pack
    . show
