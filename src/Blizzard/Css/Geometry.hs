{-# LANGUAGE OverloadedStrings #-}

module Blizzard.Css.Geometry
    ( size, top, left, bottom, right
    , width, height, minWidth, minHeight, maxWidth, maxHeight
    , padding
    , paddingTop, paddingLeft, paddingRight, paddingBottom
    , margin
    , marginTop, marginLeft, marginRight, marginBottom
    ) where


import Blizzard.Internal (Attribute(..))
import Blizzard.Css.Property ((!))
import Blizzard.Css.Size (Size)
import Blizzard.Css.Stylesheet (prop)


size, bottom, left, right, top :: Size a -> Attribute

size   = prop "size"
bottom = prop "bottom"
left   = prop "left"
right  = prop "right"
top    = prop "top"


width, height, minWidth, minHeight, maxWidth, maxHeight :: Size a -> Attribute

width     = prop "width"
height    = prop "height"
minWidth  = prop "min-width"
minHeight = prop "min-height"
maxWidth  = prop "max-width"
maxHeight = prop "max-height"


padding :: Size a -> Size a -> Size a -> Size a -> Attribute
padding a b c d = prop "padding" (a ! b ! c ! d)


paddingBottom, paddingLeft, paddingRight, paddingTop :: Size a -> Attribute

paddingBottom = prop "padding-bottom"
paddingLeft   = prop "padding-left"
paddingRight  = prop "padding-right"
paddingTop    = prop "padding-top"


margin :: Size a -> Size a -> Size a -> Size a -> Attribute
margin a b c d = prop "margin" (a ! b ! c ! d)


marginBottom, marginLeft, marginRight, marginTop :: Size a -> Attribute

marginBottom = prop "margin-bottom"
marginLeft   = prop "margin-left"
marginRight  = prop "margin-right"
marginTop    = prop "margin-top"
