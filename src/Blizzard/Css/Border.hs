{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Blizzard.Css.Border where


import Data.Text (Text)

import Blizzard.Internal (Attribute(..), Css(..))


data Stroke
    = ClassStroke Text
    | StyleStroke Text


class Auto    a where auto    :: a
class Inherit a where inherit :: a
class None    a where none    :: a


instance Auto    Stroke where auto    = StyleStroke "auto"
instance Inherit Stroke where inherit = StyleStroke "inherit"
instance None    Stroke where none    = StyleStroke "none"


dashed = ClassStroke "dashed"
dotted = ClassStroke "dotted"
double = ClassStroke "double"
groove = StyleStroke "groove"
inset  = StyleStroke "inset"
outset = StyleStroke "outset"
ridge  = StyleStroke "ridge"
solid  = ClassStroke "solid"
wavy   = ClassStroke "wavy"


borderStyle, borderBottomStyle, borderLeftStyle, borderRightStyle, borderTopStyle :: Stroke -> Attribute

borderStyle       = toAttribute
borderBottomStyle = toStyledAttribute "border-bottom-style:"
borderLeftStyle   = toStyledAttribute "border-left-style:"
borderRightStyle  = toStyledAttribute "border-right-style:"
borderTopStyle    = toStyledAttribute "border-top-style:"


class ToAttribute a where
    toAttribute       ::         a -> Attribute
    toStyledAttribute :: Text -> a -> Attribute

instance ToAttribute Stroke where
    toAttribute = \case
        ClassStroke stroke -> AttrCss $ Class ("border-" <> stroke)
        StyleStroke stroke -> AttrCss $ Style ("border-style:" <> stroke <> ";")

    toStyledAttribute prefix = \case
        ClassStroke stroke -> AttrCss $ Style (prefix <> stroke <> ";")
        StyleStroke stroke -> AttrCss $ Style (prefix <> stroke <> ";")
