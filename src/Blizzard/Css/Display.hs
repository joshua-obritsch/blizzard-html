{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Blizzard.Css.Display
    ( FloatStyle
    , float
    , floatLeft
    , floatRight
    , Clear
    , clear
    , both
    , clearLeft
    , clearRight
    , Position
    , position
    , static, absolute, fixed, relative, sticky
    , Display
    , display
    , inline, block, listItem, runIn, inlineBlock, table, displayTable, inlineTable, tableRowGroup
    , tableHeaderGroup, tableFooterGroup, tableRow, tableColumnGroup, tableColumn
    , tableCell, tableCaption, displayNone, displayInherit, flex
    , inlineFlex, grid, inlineGrid
    , Overflow
    , overflow, overflowX, overflowY
    , scroll
    , Visibility
    , visibility
    , collapse, separate
    , Clip
    , clip
    , rect
    , opacity
    , zIndex
    , PointerEvents
    , pointerEvents
    , visiblePainted, visibleFill, visibleStroke, painted
    , fillEvents, strokeEvents, allEvents
    , VerticalAlign(..)
    , middle, vAlignSub, vAlignSuper, textTop, textBottom, vAlignTop, vAlignBottom, vAlignBaseline
    , Cursor(..)
    , cursorUrl
    , cursorDefault
    , contextMenu, help, pointer, cursorProgress, wait
    , cell, crosshair, cursorText, vText
    , alias, cursorCopy, move, noDrop, notAllowed, grab, grabbing
    , allScroll, colResize, rowResize, nResize, eResize, sResize, wResize
    , neResize, nwResize, swResize, ewResize, nsResize, neswResize, nwseResize
    , zoomIn, zoomOut
    ) where


import Blizzard.Internal (Attribute(..))
import Clay.Common (Auto, Baseline, Inherit, None, baseline)
import Clay.Display
    ( FloatStyle
    , floatLeft
    , floatRight
    , Clear
    , both
    , clearLeft
    , clearRight
    , Position
    , static, absolute, fixed, relative, sticky
    , Display
    , inline, block, listItem, runIn, inlineBlock, table, displayTable, inlineTable, tableRowGroup
    , tableHeaderGroup, tableFooterGroup, tableRow, tableColumnGroup, tableColumn
    , tableCell, tableCaption, displayNone, displayInherit, flex
    , inlineFlex, grid, inlineGrid
    , Overflow
    , scroll
    , Visibility
    , collapse, separate
    , Clip
    , rect
    , PointerEvents
    , visiblePainted, visibleFill, visibleStroke, painted
    , fillEvents, strokeEvents, allEvents
    )
import Clay.Property (Val, Value, value)
import Clay.Stylesheet (key)
import Data.Text (Text)

import qualified Clay.Display as D


float :: FloatStyle -> Attribute
float a = AttrCss $ D.float a


clear :: Clear -> Attribute
clear a = AttrCss $ D.clear a


position :: Position -> Attribute
position a = AttrCss $ D.position a


display :: Display -> Attribute
display a = AttrCss $ D.display a


overflow, overflowX, overflowY :: Overflow -> Attribute

overflow  a = AttrCss $ D.overflow  a
overflowX a = AttrCss $ D.overflowX a
overflowY a = AttrCss $ D.overflowY a


visibility :: Visibility -> Attribute
visibility a = AttrCss $ D.visibility a


clip :: Clip -> Attribute
clip a = AttrCss $ D.clip a


opacity :: Double -> Attribute
opacity a = AttrCss $ D.opacity a


zIndex :: Integer -> Attribute
zIndex a = AttrCss $ D.zIndex a


pointerEvents :: PointerEvents -> Attribute
pointerEvents a = AttrCss $ D.pointerEvents a


class (Val a) => VerticalAlign a where
    verticalAlign :: a -> Attribute
    verticalAlign a = AttrCss $ key "vertical-align" a


newtype VerticalAlignValue = VerticalAlignValue Value
    deriving (Val, Baseline)


middle, vAlignSub, vAlignSuper, textTop, textBottom, vAlignTop, vAlignBottom, vAlignBaseline :: VerticalAlignValue

middle = VerticalAlignValue "middle"
vAlignSub = VerticalAlignValue "sub"
vAlignBaseline = baseline
vAlignSuper = VerticalAlignValue "super"
textTop = VerticalAlignValue "text-top"
textBottom = VerticalAlignValue "text-bottom"
vAlignTop = VerticalAlignValue "top"
vAlignBottom = VerticalAlignValue "bottom"


class (Val a) => Cursor a where
    cursor :: a -> Attribute
    cursor a = AttrCss $ key "cursor" a


newtype CursorValue a = CursorValue Value
    deriving (Auto, Inherit, None, Val)


instance Cursor (CursorValue a)


cursorUrl :: Text -> CursorValue Value
cursorUrl u = CursorValue $ value ("url(\"" <> u <> "\")")


cursorDefault
    , contextMenu, help, pointer, cursorProgress, wait
    , cell, crosshair, cursorText, vText
    , alias, cursorCopy, move, noDrop, notAllowed, grab, grabbing
    , allScroll, colResize, rowResize, nResize, eResize, sResize, wResize
    , neResize, nwResize, seResize, swResize, ewResize, nsResize, neswResize, nwseResize
    , zoomIn, zoomOut :: CursorValue Value

cursorDefault = CursorValue "default"

contextMenu = CursorValue "context-menu"
help = CursorValue "help"
pointer = CursorValue "pointer"
cursorProgress = CursorValue "progress"
wait = CursorValue "wait"

cell = CursorValue "cell"
crosshair = CursorValue "crosshair"
cursorText = CursorValue "text"
vText = CursorValue "vertical-text"

alias = CursorValue "alias"
cursorCopy = CursorValue "copy"
move = CursorValue "move"
noDrop = CursorValue "no-drop"
notAllowed = CursorValue "not-allowed"
grab = CursorValue "grab"
grabbing = CursorValue "grabbing"

allScroll = CursorValue "all-scroll"
colResize = CursorValue "col-resize"
rowResize = CursorValue "row-resize"
nResize = CursorValue "n-resize"
eResize = CursorValue "e-resize"
sResize = CursorValue "s-resize"
wResize = CursorValue "w-resize"

neResize = CursorValue "ne-resize"
nwResize = CursorValue "nw-resize"
seResize = CursorValue "se-resize"
swResize = CursorValue "sw-resize"
ewResize = CursorValue "ew-resize"
nsResize = CursorValue "ns-resize"
neswResize = CursorValue "nesw-resize"
nwseResize = CursorValue "nwse-resize"

zoomIn = CursorValue "zoom-in"
zoomOut = CursorValue "zoom-out"
