{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
    , repeatX, repeatY
    , repeatXy

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

import Data.Text (Text)

import Blizzard.Internal (Attribute(..))
import Blizzard.Css.Box (BoxType)
import Blizzard.Css.Color (Color)
import Blizzard.Css.Common (Inherit, None, Other)
import Blizzard.Css.Property (Val, Value, value)
import Blizzard.Css.Size (Angle, Size)
import Blizzard.Css.Stylesheet (prop)

import qualified Clay.Background as B


class Val a => Background a where
    background :: a -> Attribute
    background = prop "background"


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
backgroundColor = prop "background-color"


newtype BackgroundPosition = BackgroundPosition Value
    deriving (Inherit, Val)


backgroundPosition :: BackgroundPosition -> Attribute
backgroundPosition = prop "background-position"


backgroundPositions :: [BackgroundPosition] -> Attribute
backgroundPositions = prop "background-position"


placed :: Side -> Side -> BackgroundPosition
placed a b = BackgroundPosition $ value (a, b)


positioned :: Size a -> Size a -> BackgroundPosition
positioned a b = BackgroundPosition $ value (a, b)


newtype BackgroundSize = BackgroundSize Value
    deriving (Inherit, Val)


backgroundSize :: BackgroundSize -> Attribute
backgroundSize = prop "background-size"


backgroundSizes :: [BackgroundSize] -> Attribute
backgroundSizes = prop "background-size"


contain, cover :: BackgroundSize

contain = BackgroundSize "contain"
cover   = BackgroundSize "cover"


by :: Size a -> Size b -> BackgroundSize
by a b = BackgroundSize $ value (a, b)


newtype BackgroundRepeat = BackgroundRepeat Value
    deriving (Inherit, None, Other, Val)


backgroundRepeat :: BackgroundRepeat -> Attribute
backgroundRepeat = prop "background-repeat"


backgroundRepeats :: [BackgroundRepeat] -> Attribute
backgroundRepeats = prop "background-repeat"


noRepeat, repeat, round, space :: BackgroundRepeat

noRepeat = BackgroundRepeat "no-repeat"
repeat   = BackgroundRepeat "repeat"
round    = BackgroundRepeat "round"
space    = BackgroundRepeat "space"


repeatX, repeatY :: BackgroundRepeat

repeatX = repeatXy repeat   noRepeat
repeatY = repeatXy noRepeat repeat


repeatXy :: BackgroundRepeat -> BackgroundRepeat -> BackgroundRepeat
repeatXy a b = BackgroundRepeat $ value (a, b)


newtype BackgroundOrigin = BackgroundOrigin Value
    deriving (Inherit, Other, Val)


backgroundOrigin :: BackgroundOrigin -> Attribute
backgroundOrigin = prop "background-origin"


backgroundOrigins :: [BackgroundOrigin] -> Attribute
backgroundOrigins = prop "background-origin"


origin :: BoxType -> BackgroundOrigin
origin = BackgroundOrigin . value


newtype BackgroundClip = BackgroundClip Value
    deriving (Inherit, Other, Val)


backgroundClip :: BackgroundClip -> Attribute
backgroundClip = prop "background-clip"


backgroundClips :: [BackgroundClip] -> Attribute
backgroundClips = prop "background-clip"


boxClip :: BoxType -> BackgroundClip
boxClip = BackgroundClip . value


newtype BackgroundAttachment = BackgroundAttachment Value
    deriving (Inherit, Other, Val)


backgroundAttachment :: BackgroundAttachment -> Attribute
backgroundAttachment = prop "background-attachment"


backgroundAttachments :: [BackgroundAttachment] -> Attribute
backgroundAttachments = prop "background-attachment"


attachFixed, attachScroll :: BackgroundAttachment

attachFixed  = BackgroundAttachment "fixed"
attachScroll = BackgroundAttachment "scroll"


newtype BackgroundImage = BackgroundImage Value
    deriving (Inherit, None, Other, Val)


backgroundImage :: BackgroundImage -> Attribute
backgroundImage = prop "background-image"


backgroundImages :: [BackgroundImage] -> Attribute
backgroundImages = prop "background-image"


url :: Text -> BackgroundImage
url = BackgroundImage . value . (<> "\")") . ("url(\"" <>)


newtype Side = Side Value
    deriving (Inherit, Other, Val)


sideBottom, sideCenter, sideLeft, sideMiddle, sideRight, sideTop :: Side

sideBottom = Side "bottom"
sideCenter = Side "center"
sideLeft   = Side "left"
sideMiddle = Side "middle"
sideRight  = Side "right"
sideTop    = Side "top"


newtype Direction = Direction Value
    deriving (Other, Val)


angular :: Angle a -> Direction
angular = Direction . value


straight :: Side -> Direction
straight = Direction . value


newtype Location = Location Value
    deriving (Other, Val)


class Val a => Loc a where
    location :: a -> Location
    location = Location . value


instance Loc Side
instance Loc (Size a)
instance (Loc a, Loc b) => Loc (a, b)
