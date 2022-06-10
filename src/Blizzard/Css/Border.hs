{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Blizzard.Css.Border
    ( Stroke
    , solid, dotted, dashed, double, wavy, groove, ridge, inset, outset
    , border, borderTop, borderLeft, borderBottom, borderRight
    , borderColor4, borderColor, borderLeftColor, borderRightColor, borderTopColor, borderBottomColor
    , borderStyle4, borderStyle, borderLeftStyle, borderRightStyle, borderTopStyle, borderBottomStyle
    , borderWidth4, borderWidth, borderLeftWidth, borderRightWidth, borderTopWidth, borderBottomWidth
    , outline, outlineTop, outlineLeft, outlineBottom, outlineRight
    , outlineColor4, outlineColor, outlineLeftColor, outlineRightColor, outlineTopColor, outlineBottomColor
    , outlineStyle4, outlineStyle, outlineLeftStyle, outlineRightStyle, outlineTopStyle, outlineBottomStyle
    , outlineWidth4, outlineWidth, outlineLeftWidth, outlineRightWidth, outlineTopWidth, outlineBottomWidth
    , outlineOffset
    , borderRadius
    , borderTopLeftRadius, borderTopRightRadius
    , borderBottomLeftRadius, borderBottomRightRadius
    , borderCollapse
    , borderSpacing, borderSpacing2
    ) where


import Blizzard.Internal (Attribute(..))
import Blizzard.Css.Color (Color)
import Blizzard.Css.Common (Auto, Inherit, None, Other)
import Blizzard.Css.Display (Visibility)
import Blizzard.Css.Size (Length, Size)
import Blizzard.Css.Property ((!), Val, Value)
import Blizzard.Css.Stylesheet (prop)


newtype Stroke = Stroke Value
    deriving (Auto, Inherit, None, Other, Val)


dashed, dotted, double, groove, inset, outset, ridge, solid, wavy :: Stroke

dashed = Stroke "dashed"
dotted = Stroke "dotted"
double = Stroke "double"
groove = Stroke "groove"
inset  = Stroke "inset"
outset = Stroke "outset"
ridge  = Stroke "ridge"
solid  = Stroke "solid"
wavy   = Stroke "wavy"


border, borderBottom, borderLeft, borderRight, borderTop :: Size Length -> Stroke -> Color -> Attribute

border       a b c = prop "border"        (a ! b ! c)
borderBottom a b c = prop "border-bottom" (a ! b ! c)
borderLeft   a b c = prop "border-left"   (a ! b ! c)
borderRight  a b c = prop "border-right"  (a ! b ! c)
borderTop    a b c = prop "border-top"    (a ! b ! c)


borderColor4 :: Color -> Color -> Color -> Color -> Attribute
borderColor4 a b c d = prop "border-color" (a ! b ! c ! d)


borderColor, borderBottomColor, borderLeftColor, borderRightColor, borderTopColor :: Color -> Attribute

borderColor       = prop "border-color"
borderBottomColor = prop "border-bottom-color"
borderLeftColor   = prop "border-left-color"
borderRightColor  = prop "border-right-color"
borderTopColor    = prop "border-top-color"


borderStyle4 :: Stroke -> Stroke -> Stroke -> Stroke -> Attribute
borderStyle4 a b c d = prop "border-style" (a ! b ! c ! d)


borderStyle, borderBottomStyle, borderLeftStyle, borderRightStyle, borderTopStyle :: Stroke -> Attribute

borderStyle       = prop "border-style"
borderBottomStyle = prop "border-bottom-style"
borderLeftStyle   = prop "border-right-style"
borderRightStyle  = prop "border-right-style"
borderTopStyle    = prop "border-top-style"


borderWidth4 :: Size Length -> Size Length -> Size Length -> Size Length -> Attribute
borderWidth4 a b c d = prop "border-width" (a ! b ! c ! d)


borderWidth, borderBottomWidth, borderLeftWidth, borderRightWidth, borderTopWidth :: Size Length -> Attribute

borderWidth       = prop "border-width"
borderBottomWidth = prop "border-bottom-width"
borderLeftWidth   = prop "border-left-width"
borderRightWidth  = prop "border-right-width"
borderTopWidth    = prop "border-top-width"


outline, outlineBottom, outlineLeft, outlineRight, outlineTop :: Stroke -> Size Length -> Color -> Attribute

outline       a b c = prop "outline"        (a ! b ! c)
outlineBottom a b c = prop "outline-bottom" (a ! b ! c)
outlineLeft   a b c = prop "outline-left"   (a ! b ! c)
outlineRight  a b c = prop "outline-right"  (a ! b ! c)
outlineTop    a b c = prop "outline-top"    (a ! b ! c)


outlineColor4 :: Color -> Color -> Color -> Color -> Attribute
outlineColor4 a b c d = prop "outline-color" (a ! b ! c ! d)


outlineColor, outlineBottomColor, outlineLeftColor, outlineRightColor, outlineTopColor :: Color -> Attribute

outlineColor       = prop "outline-color"
outlineBottomColor = prop "outline-bottom-color"
outlineLeftColor   = prop "outline-left-color"
outlineRightColor  = prop "outline-right-color"
outlineTopColor    = prop "outline-top-color"


outlineStyle4 :: Stroke -> Stroke -> Stroke -> Stroke -> Attribute
outlineStyle4 a b c d = prop "outline-style" (a ! b ! c ! d)


outlineStyle, outlineBottomStyle, outlineLeftStyle, outlineRightStyle, outlineTopStyle :: Stroke -> Attribute

outlineStyle       = prop "outline-style"
outlineBottomStyle = prop "outline-bottom-style"
outlineLeftStyle   = prop "outline-left-style"
outlineRightStyle  = prop "outline-right-style"
outlineTopStyle    = prop "outline-top-style"


outlineWidth4 :: Size Length -> Size Length -> Size Length -> Size Length -> Attribute
outlineWidth4 a b c d = prop "outline-width" (a ! b ! c ! d)


outlineWidth, outlineBottomWidth, outlineLeftWidth, outlineRightWidth, outlineTopWidth :: Size Length -> Attribute

outlineWidth       = prop "outline-width"
outlineBottomWidth = prop "outline-bottom-width"
outlineLeftWidth   = prop "outline-left-width"
outlineRightWidth  = prop "outline-right-width"
outlineTopWidth    = prop "outline-top-width"


outlineOffset :: Size Length -> Attribute
outlineOffset = prop "outline-offset"


borderRadius :: Size a -> Size a -> Size a -> Size a -> Attribute
borderRadius a b c d = prop "border-radius" (a ! b ! c ! d)


borderBottomLeftRadius, borderBottomRightRadius, borderTopLeftRadius, borderTopRightRadius :: Size a -> Size a -> Attribute

borderBottomLeftRadius  a b = prop "border-bottom-left-radius"  (a ! b)
borderBottomRightRadius a b = prop "border-bottom-right-radius" (a ! b)
borderTopLeftRadius     a b = prop "border-top-left-radius"     (a ! b)
borderTopRightRadius    a b = prop "border-top-right-radius"    (a ! b)


borderCollapse :: Visibility -> Attribute
borderCollapse = prop "border-collapse"


borderSpacing :: Size a -> Attribute
borderSpacing = prop "border-spacing"


borderSpacing2 :: Size a -> Size a -> Attribute
borderSpacing2 a b = prop "border-spacing" (a ! b)
