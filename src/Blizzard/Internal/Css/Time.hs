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
