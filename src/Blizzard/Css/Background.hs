{-# LANGUAGE OverloadedStrings #-}

module Blizzard.Css.Background
    (
    -- background
      Background(..)

    -- background-attachment
    , BackgroundAttachment
    , backgroundAttachment
    , backgroundAttachments
    , attachFixed, attachScroll

    -- background-color
    , backgroundColor

    -- background-position
    , BackgroundPosition
    , backgroundPosition
    , backgroundPositions
    , placed
    , positioned

    -- background-size
    , BackgroundSize
    , backgroundSize
    , backgroundSizes
    , contain, cover
    , by

    -- background-repeat
    , BackgroundRepeat
    , backgroundRepeat
    , backgroundRepeats
    , repeat, space, round, noRepeat
    , xyRepeat
    , repeatX, repeatY

    -- background-origin
    , BackgroundOrigin
    , backgroundOrigin
    , backgroundOrigins
    , origin

    -- background-clip
    , BackgroundClip
    , backgroundClip
    , backgroundClips
    , boxClip

    -- background-image
    , BackgroundImage
    , backgroundImage
    , backgroundImages
    , url

    , Side
    , sideTop
    , sideLeft
    , sideRight
    , sideBottom
    , sideCenter
    , sideMiddle
    , Direction
    , straight
    , angular
    , Location
    , Loc
    , Val
    , location
    ) where


import Prelude hiding (repeat, round)

import Blizzard.Internal (Attribute(..))
import Clay.Background
    ( BackgroundPosition
    , placed
    , positioned
    , BackgroundSize
    , contain, cover
    , by
    , BackgroundRepeat
    , repeat, space, round, noRepeat
    , xyRepeat
    , repeatX, repeatY
    , BackgroundOrigin
    , origin
    , BackgroundClip
    , boxClip
    , BackgroundAttachment
    , attachFixed, attachScroll
    , BackgroundImage
    , url
    , Side
    , sideTop
    , sideLeft
    , sideRight
    , sideBottom
    , sideCenter
    , sideMiddle
    , Direction
    , straight
    , angular
    , Location
    , Loc
    , Val
    , location
    )
import Clay.Color (Color)
import Clay.Stylesheet (key)

import qualified Clay.Background as B


class Val a => Background a where
    background :: a -> Attribute
    background a = AttrCss $ key "background" a


instance Background a => Background [a]
instance (Background a, Background b) => Background (a, b)

instance Background Color
instance Background BackgroundPosition
instance Background BackgroundSize
instance Background BackgroundRepeat
instance Background BackgroundOrigin
instance Background BackgroundClip
instance Background BackgroundAttachment
instance Background BackgroundImage


backgroundColor :: Color -> Attribute
backgroundColor a = AttrCss $ B.backgroundColor a


backgroundPosition :: BackgroundPosition -> Attribute
backgroundPosition a = AttrCss $ B.backgroundPosition a


backgroundPositions :: [BackgroundPosition] -> Attribute
backgroundPositions a = AttrCss $ B.backgroundPositions a


backgroundSize :: BackgroundSize -> Attribute
backgroundSize a = AttrCss $ B.backgroundSize a


backgroundSizes :: [BackgroundSize] -> Attribute
backgroundSizes a = AttrCss $ B.backgroundSizes a


backgroundRepeat :: BackgroundRepeat -> Attribute
backgroundRepeat a = AttrCss $ B.backgroundRepeat a


backgroundRepeats :: [BackgroundRepeat] -> Attribute
backgroundRepeats a = AttrCss $ B.backgroundRepeats a


backgroundOrigin :: BackgroundOrigin -> Attribute
backgroundOrigin a = AttrCss $ B.backgroundOrigin a


backgroundOrigins :: [BackgroundOrigin] -> Attribute
backgroundOrigins a = AttrCss $ B.backgroundOrigins a


backgroundClip :: BackgroundClip -> Attribute
backgroundClip a = AttrCss $ B.backgroundClip a


backgroundClips :: [BackgroundClip] -> Attribute
backgroundClips a = AttrCss $ B.backgroundClips a


backgroundAttachment :: BackgroundAttachment -> Attribute
backgroundAttachment a = AttrCss $ B.backgroundAttachment a


backgroundAttachments :: [BackgroundAttachment] -> Attribute
backgroundAttachments a = AttrCss $ B.backgroundAttachments a


backgroundImage :: BackgroundImage -> Attribute
backgroundImage a = AttrCss $ B.backgroundImage a


backgroundImages :: [BackgroundImage] -> Attribute
backgroundImages a = AttrCss $ B.backgroundImages a
