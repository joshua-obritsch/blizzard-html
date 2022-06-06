module Blizzard.Css.Geometry
    ( size, top, left, bottom, right
    , width, height, minWidth, minHeight, maxWidth, maxHeight
    , padding
    , paddingTop, paddingLeft, paddingRight, paddingBottom
    , margin
    , marginTop, marginLeft, marginRight, marginBottom
    ) where


import Blizzard.Internal (Attribute(..))
import Clay.Size (Size)

import qualified Clay.Geometry as G


size, top, left, bottom, right :: Size a -> Attribute

size   a = AttrCss $ G.size   a
top    a = AttrCss $ G.top    a
left   a = AttrCss $ G.left   a
bottom a = AttrCss $ G.bottom a
right  a = AttrCss $ G.right  a


width, height, minWidth, minHeight, maxWidth, maxHeight :: Size a -> Attribute

width     a = AttrCss $ G.width     a
height    a = AttrCss $ G.height    a
minWidth  a = AttrCss $ G.minWidth  a
minHeight a = AttrCss $ G.minHeight a
maxWidth  a = AttrCss $ G.maxWidth  a
maxHeight a = AttrCss $ G.maxHeight a


padding :: Size a -> Size a -> Size a -> Size a -> Attribute
padding a b c d = AttrCss $ G.padding a b c d


paddingTop, paddingLeft, paddingRight, paddingBottom :: Size a -> Attribute

paddingTop    a = AttrCss $ G.paddingTop    a
paddingLeft   a = AttrCss $ G.paddingLeft   a
paddingRight  a = AttrCss $ G.paddingRight  a
paddingBottom a = AttrCss $ G.paddingBottom a


margin :: Size a -> Size a -> Size a -> Size a -> Attribute
margin a b c d = AttrCss $ G.margin a b c d


marginTop, marginLeft, marginRight, marginBottom :: Size a -> Attribute

marginTop    a = AttrCss $ G.marginTop    a
marginLeft   a = AttrCss $ G.marginLeft   a
marginRight  a = AttrCss $ G.marginRight  a
marginBottom a = AttrCss $ G.marginBottom a
