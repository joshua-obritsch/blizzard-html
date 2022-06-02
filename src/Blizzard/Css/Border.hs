module Blizzard.Css.Border
    ( solid, dotted, dashed, double, wavy, groove, ridge, inset, outset
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
    , borderTopLeftRadius, borderTopRightRadius, borderBottomLeftRadius, borderBottomRightRadius
    , borderCollapse
    , borderSpacing, borderSpacing2
    ) where


import Blizzard.Internal (Attribute(..))
import Clay.Border (Stroke)
import Clay.Color (Color)
import Clay.Display (Visibility)
import Clay.Size (LengthUnit, Size)
import Clay.Stylesheet (Css)

import qualified Clay.Border as B


solid, dotted, dashed, double, wavy, groove, ridge, inset, outset :: Stroke

solid  = B.solid
dotted = B.dotted
dashed = B.dashed
double = B.double
wavy   = B.wavy
groove = B.groove
ridge  = B.ridge
inset  = B.inset
outset = B.outset


border, borderTop, borderLeft, borderBottom, borderRight :: Size LengthUnit -> Stroke -> Color -> Attribute

border       a b c = AttrCss $ B.border       a b c
borderTop    a b c = AttrCss $ B.borderTop    a b c
borderLeft   a b c = AttrCss $ B.borderLeft   a b c
borderBottom a b c = AttrCss $ B.borderBottom a b c
borderRight  a b c = AttrCss $ B.borderRight  a b c


borderColor4 :: Color -> Color -> Color -> Color -> Attribute
borderColor4 a b c d = AttrCss $ B.borderColor4 a b c d


borderColor, borderLeftColor, borderRightColor, borderTopColor, borderBottomColor :: Color -> Attribute

borderColor       a = AttrCss $ B.borderColor       a
borderLeftColor   a = AttrCss $ B.borderLeftColor   a
borderRightColor  a = AttrCss $ B.borderRightColor  a
borderTopColor    a = AttrCss $ B.borderTopColor    a
borderBottomColor a = AttrCss $ B.borderBottomColor a


borderStyle4 :: Stroke -> Stroke -> Stroke -> Stroke -> Attribute
borderStyle4 a b c d = AttrCss $ B.borderStyle4 a b c d


borderStyle, borderLeftStyle, borderRightStyle, borderTopStyle, borderBottomStyle :: Stroke -> Attribute

borderStyle       a = AttrCss $ B.borderStyle       a
borderLeftStyle   a = AttrCss $ B.borderLeftStyle   a
borderRightStyle  a = AttrCss $ B.borderRightStyle  a
borderTopStyle    a = AttrCss $ B.borderTopStyle    a
borderBottomStyle a = AttrCss $ B.borderBottomStyle a


borderWidth4 :: Size LengthUnit -> Size LengthUnit -> Size LengthUnit -> Size LengthUnit -> Attribute
borderWidth4 a b c d = AttrCss $ B.borderWidth4 a b c d


borderWidth, borderLeftWidth, borderRightWidth, borderTopWidth, borderBottomWidth :: Size LengthUnit -> Attribute

borderWidth       a = AttrCss $ B.borderWidth       a
borderLeftWidth   a = AttrCss $ B.borderLeftWidth   a
borderRightWidth  a = AttrCss $ B.borderRightWidth  a
borderTopWidth    a = AttrCss $ B.borderTopWidth    a
borderBottomWidth a = AttrCss $ B.borderBottomWidth a


outline, outlineTop, outlineLeft, outlineBottom, outlineRight :: Stroke -> Size LengthUnit -> Color -> Attribute

outline        a b c = AttrCss $ B.outline       a b c
outlineTop     a b c = AttrCss $ B.outlineTop    a b c
outlineLeft    a b c = AttrCss $ B.outlineLeft   a b c
outlineBottom  a b c = AttrCss $ B.outlineBottom a b c
outlineRight   a b c = AttrCss $ B.outlineRight  a b c


outlineColor4 :: Color -> Color -> Color -> Color -> Attribute
outlineColor4 a b c d = AttrCss $ B.outlineColor4 a b c d


outlineColor, outlineLeftColor, outlineRightColor, outlineTopColor, outlineBottomColor :: Color -> Attribute

outlineColor       a = AttrCss $ B.outlineColor       a
outlineLeftColor   a = AttrCss $ B.outlineLeftColor   a
outlineRightColor  a = AttrCss $ B.outlineRightColor  a
outlineTopColor    a = AttrCss $ B.outlineTopColor    a
outlineBottomColor a = AttrCss $ B.outlineBottomColor a


outlineStyle4 :: Stroke -> Stroke -> Stroke -> Stroke -> Attribute
outlineStyle4 a b c d = AttrCss $ B.outlineStyle4 a b c d


outlineStyle, outlineLeftStyle, outlineRightStyle, outlineTopStyle, outlineBottomStyle :: Stroke -> Attribute

outlineStyle       a = AttrCss $ B.outlineStyle       a
outlineLeftStyle   a = AttrCss $ B.outlineLeftStyle   a
outlineRightStyle  a = AttrCss $ B.outlineRightStyle  a
outlineTopStyle    a = AttrCss $ B.outlineTopStyle    a
outlineBottomStyle a = AttrCss $ B.outlineBottomStyle a


outlineWidth4 :: Size LengthUnit -> Size LengthUnit -> Size LengthUnit -> Size LengthUnit -> Attribute
outlineWidth4 a b c d = AttrCss $ B.outlineWidth4 a b c d


outlineWidth, outlineLeftWidth, outlineRightWidth, outlineTopWidth, outlineBottomWidth :: Size LengthUnit -> Attribute

outlineWidth       a = AttrCss $ B.outlineWidth       a
outlineLeftWidth   a = AttrCss $ B.outlineLeftWidth   a
outlineRightWidth  a = AttrCss $ B.outlineRightWidth  a
outlineTopWidth    a = AttrCss $ B.outlineTopWidth    a
outlineBottomWidth a = AttrCss $ B.outlineBottomWidth a


outlineOffset :: Size LengthUnit -> Attribute
outlineOffset a = AttrCss $ B.outlineOffset a


borderRadius :: Size a -> Size a -> Size a -> Size a -> Attribute
borderRadius a b c d = AttrCss $ B.borderRadius a b c d


borderTopLeftRadius, borderTopRightRadius, borderBottomLeftRadius, borderBottomRightRadius :: Size a -> Size a -> Attribute

borderTopLeftRadius     a b = AttrCss $ B.borderTopLeftRadius     a b
borderTopRightRadius    a b = AttrCss $ B.borderTopRightRadius    a b
borderBottomLeftRadius  a b = AttrCss $ B.borderBottomLeftRadius  a b
borderBottomRightRadius a b = AttrCss $ B.borderBottomRightRadius a b


borderCollapse :: Visibility -> Attribute
borderCollapse a = AttrCss $ B.borderCollapse a


borderSpacing :: Size a -> Attribute
borderSpacing a = AttrCss $ B.borderSpacing a


borderSpacing2 :: Size a -> Size a -> Attribute
borderSpacing2 a b = AttrCss $ B.borderSpacing2 a b
