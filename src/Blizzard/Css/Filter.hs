module Blizzard.Css.Filter
    ( Filter
    , filter
    , filters
    , url
    , blur
    , brightness
    , contrast
    , dropShadow
    , grayscale
    , hueRotate
    , invert
    , opacity
    , saturate
    , sepia
    ) where


import Prelude hiding (filter)

import Blizzard.Internal (Attribute(..))
import Clay.Filter
    ( Filter
    , url
    , blur
    , brightness
    , contrast
    , dropShadow
    , grayscale
    , hueRotate
    , invert
    , opacity
    , saturate
    , sepia
    )

import qualified Clay.Filter as F


filter :: Filter -> Attribute
filter a = AttrCss $ F.filter a


filters :: [Filter] -> Attribute
filters a = AttrCss $ F.filters a
