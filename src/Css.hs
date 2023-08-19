{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Css
    ( Css
    , auto
    , inherit
    , initial
    , accentColor
    , alignContent
    ) where


import Data.Text.Lazy.Builder (Builder)
import Internal (Buildable(..))


data Css
    = Property Builder Builder


instance Buildable Css where
    build (Property key value) = mconcat [ key, value, ";" ]


instance Buildable [Css] where
    build = mconcat . map build


auto :: Builder
auto = "auto"


inherit :: Builder
inherit = "inherit"


initial :: Builder
initial = "initial"


-- | Set the accent color for an element using the "accent-color" CSS property.
--
-- ==== __Accepted values__
-- +-----------+---------------------------------------------------+
-- | 'auto'    | Use the browser's default accent color.           |
-- +-----------+---------------------------------------------------+
-- | 'inherit' | Inherit the accent color from the parent element. |
-- +-----------+---------------------------------------------------+
-- | 'initial' | Set the accent color to its initial value.        |
-- +-----------+---------------------------------------------------+
-- | \<color\> | A specific color value (e.g., hex, RGB, named).   |
-- +-----------+---------------------------------------------------+
accentColor :: Builder -> Css
accentColor = Property "accent-color:"


-- | Set how the browser distributes space between and around content items along the cross-axis of a flexbox container using the "align-content" CSS property.
--
-- ==== __Accepted values__
-- +----------------+----------------------------------------------------------------------------------------+
-- | 'center'       | Pack items toward the center of the container.                                         |
-- +----------------+----------------------------------------------------------------------------------------+
-- | 'flexEnd'      | Pack items toward the end of the container.                                            |
-- +----------------+----------------------------------------------------------------------------------------+
-- | 'flexStart'    | Pack items toward the start of the container.                                          |
-- +----------------+----------------------------------------------------------------------------------------+
-- | 'spaceAround'  | Distribute items evenly with equal space around them.                                  |
-- +----------------+----------------------------------------------------------------------------------------+
-- | 'spaceBetween' | Distribute items evenly with the first item at the start and the last item at the end. |
-- +----------------+----------------------------------------------------------------------------------------+
-- | 'stretch'      | Stretch items to fill the container (default).                                         |
-- +----------------+----------------------------------------------------------------------------------------+
-- | 'inherit'      | Inherit the accent color from the parent element.                                      |
-- +----------------+----------------------------------------------------------------------------------------+
-- | 'initial'      | Set the accent color to its initial value.                                             |
-- +----------------+----------------------------------------------------------------------------------------+
alignContent :: Builder -> Css
alignContent = Property "align-content:"


-- | ==== __Accepted values__
--
-- @
-- normal | stretch | center | flexStart | flexEnd | start | end | baseline | initial | inherit
-- @
alignItems :: Builder -> Css
alignItems = Property "align-items:"


-- | ==== __Accepted values__
--
-- @
-- auto | stretch | center | flexStart | flexEnd | baseline | initial | inherit
-- @
alignSelf :: Builder -> Css
alignSelf = Property "align-self:"


-- | ==== __Accepted values__
--
-- @
-- name duration timing-function delay iteration-count direction fill-mode play-state | initial | inherit
-- @
animation :: Builder -> Css
animation = Property "animation:"


-- | ==== __Accepted values__
--
-- @
-- time | initial | inherit
-- @
animationDelay :: Builder -> Css
animationDelay = Property "animation-delay:"
