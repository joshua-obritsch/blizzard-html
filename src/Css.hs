{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Css
    ( Css
    , accentColor
    , color
    , module Css.Colors
    , module Css.Values
    ) where


import Css.Colors
import Css.Values
import Data.Text.Lazy.Builder (Builder)
import Internal (Buildable(..))


data Css
    = Property Builder Builder


instance Buildable Css where
    build (Property key value) = mconcat [ key, value, ";" ]


instance Buildable [Css] where
    build = mconcat . map build


accentColor :: Builder -> Css
accentColor = Property "accent-color:"


color :: Builder -> Css
color = Property "color:"
