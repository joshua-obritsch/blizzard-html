{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Blizzard.Css.Display
    ( FloatStyle
    , float
    , floatLeft
    , floatRight
    , Clear
    , clear
    , clearLeft
    , clearRight
    , Position
    , position
    , static, absolute, fixed, relative, sticky
    , Display
    , display
    , inline, block, listItem, runIn, inlineBlock, displayTable, inlineTable, tableRowGroup
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


import Data.String (fromString)
import Data.Text (Text)

import Blizzard.Internal (Attribute(..))
import Blizzard.Css.Common
    ( Auto
    , Baseline
    , Both
    , Hidden
    , Inherit
    , None
    , Other
    , Unset
    , Visible
    , baseline
    )
import Blizzard.Css.Property (Val, Value, value)
import Blizzard.Css.Size (Size)
import Blizzard.Css.Stylesheet (prop)


newtype FloatStyle = FloatStyle Value
    deriving (Inherit, None, Val)


float :: FloatStyle -> Attribute
float = prop "float"


floatLeft, floatRight :: FloatStyle

floatLeft  = FloatStyle "left"
floatRight = FloatStyle "right"


newtype Clear = Clear Value
    deriving (Both, Inherit, None, Other, Val)


clear :: Clear -> Attribute
clear = prop "clear"


clearLeft, clearRight :: Clear

clearLeft  = Clear "left"
clearRight = Clear "right"


newtype Position = Position Value
    deriving (Inherit, Other, Val)


position :: Position -> Attribute
position = prop "position"


absolute, fixed, relative, static, sticky :: Position

absolute = Position "absolute"
fixed    = Position "fixed"
relative = Position "relative"
static   = Position "static"
sticky   = Position "sticky"


newtype Display = Display Value
    deriving (Inherit, None, Other, Val)


display :: Display -> Attribute
display = prop "display"


block, displayInherit, displayNone, displayTable, flex, grid
    , inline, inlineBlock, inlineFlex, inlineGrid, inlineTable
    , listItem, runIn
    , tableCaption, tableCell, tableColumn, tableColumnGroup, tableFooterGroup, tableHeaderGroup, tableRow, tableRowGroup :: Display

block            = Display "block"
displayInherit   = Display "inherit"
displayNone      = Display "none"
displayTable     = Display "table"
flex             = Display "flex"
grid             = Display "grid"
inline           = Display "inline"
inlineBlock      = Display "inline-block"
inlineFlex       = Display "inline-flex"
inlineGrid       = Display "inline-grid"
inlineTable      = Display "inline-table"
listItem         = Display "list-item"
runIn            = Display "run-in"
tableCaption     = Display "table-caption"
tableCell        = Display "table-cell"
tableColumn      = Display "table-column"
tableColumnGroup = Display "table-column-group"
tableFooterGroup = Display "table-footer-group"
tableHeaderGroup = Display "table-header-group"
tableRow         = Display "table-row"
tableRowGroup    = Display "table-row-group"


newtype Overflow = Overflow Value
    deriving (Auto, Hidden, Inherit, Other, Val, Visible)


overflow, overflowX, overflowY :: Overflow -> Attribute

overflow  = prop "overflow"
overflowX = prop "overflow-x"
overflowY = prop "overflow-y"


scroll :: Overflow
scroll = Overflow "scroll"


newtype Visibility = Visibility Value
    deriving (Hidden, Inherit, Other, Unset, Val, Visible)


visibility :: Visibility -> Attribute
visibility = prop "visibility"


collapse, separate :: Visibility

collapse = Visibility "collapse"
separate = Visibility "separate"


newtype Clip = Clip Value
    deriving (Auto, Inherit, Other, Val)


clip :: Clip -> Attribute
clip = prop "clip"


rect :: Size a -> Size a -> Size a -> Size a -> Clip
rect a b c d = Clip $ mconcat ["rect(", value a, ",", value b, ",", value c, ",", value d, ")"]


opacity :: Double -> Attribute
opacity = prop "opacity"


zIndex :: Integer -> Attribute
zIndex = prop "z-index" . fromString' . show
  where
    fromString' :: String -> Value
    fromString' = fromString

newtype PointerEvents = PointerEvents Value
    deriving (Auto, Inherit, None, Other, Val, Visible)


pointerEvents :: PointerEvents -> Attribute
pointerEvents = prop "pointer-events"


allEvents, fillEvents, painted, strokeEvents, visibleFill, visiblePainted, visibleStroke :: PointerEvents

allEvents      = PointerEvents "all"
fillEvents     = PointerEvents "fill"
painted        = PointerEvents "painted"
strokeEvents   = PointerEvents "stroke"
visibleFill    = PointerEvents "visibleFill"
visiblePainted = PointerEvents "visiblePainted"
visibleStroke  = PointerEvents "visibleStroke"


class (Val a) => VerticalAlign a where
    verticalAlign :: a -> Attribute
    verticalAlign = prop "vertical-align"


instance VerticalAlign VerticalAlignValue
instance VerticalAlign (Size a)


newtype VerticalAlignValue = VerticalAlignValue Value
    deriving (Baseline, Val)


middle, textBottom, textTop, vAlignBaseline, vAlignBottom, vAlignSub, vAlignSuper, vAlignTop :: VerticalAlignValue

middle = VerticalAlignValue "middle"
textBottom = VerticalAlignValue "text-bottom"
textTop = VerticalAlignValue "text-top"
vAlignBaseline = baseline
vAlignBottom = VerticalAlignValue "bottom"
vAlignSub = VerticalAlignValue "sub"
vAlignSuper = VerticalAlignValue "super"
vAlignTop = VerticalAlignValue "top"


class (Val a) => Cursor a where
    cursor :: a -> Attribute
    cursor = prop "cursor"


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
