{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

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

import Data.Text (Text)

import Blizzard.Internal (Attribute(..))
import Blizzard.Css.Color (Color)
import Blizzard.Css.Common (Inherit, None)
import Blizzard.Css.Property ((!), Val, Value, noCommas, value)
import Blizzard.Css.Size (Angle, Length, Percentage, Size)
import Blizzard.Css.Stylesheet (prop)


newtype Filter = Filter Value
    deriving (Inherit, None, Val)


filter :: Filter -> Attribute
filter = prop "filter"


filters :: [Filter] -> Attribute
filters = prop "filter" . noCommas


url :: Text -> Filter
url a = Filter $ "url(" <> value a <> ")"


blur :: Size Length -> Filter
blur a = Filter $ "blur(" <> value a <> ")"


brightness :: Double -> Filter
brightness a = Filter $ "brightness(" <> value a <> ")"


contrast :: Size Percentage -> Filter
contrast a = Filter $ "contrast(" <> value a <> ")"


dropShadow :: Size Length -> Size Length -> Size Length -> Color -> Filter
dropShadow a b c d = Filter $ "drop-shadow(" <> value (a ! b ! c ! d) <> ")"


grayscale :: Size Percentage -> Filter
grayscale a = Filter $ "grayscale(" <> value a <> ")"


hueRotate :: Angle a -> Filter
hueRotate a = Filter $ "hue-rotate(" <> value a <> ")"


invert :: Size Percentage -> Filter
invert a = Filter $ "invert(" <> value a <> ")"


opacity :: Size Percentage -> Filter
opacity a = Filter $ "opacity(" <> value a <> ")"


saturate :: Size Percentage -> Filter
saturate a = Filter $ "saturate(" <> value a <> ")"


sepia :: Size Percentage -> Filter
sepia a = Filter $ "sepia(" <> value a <> ")"
