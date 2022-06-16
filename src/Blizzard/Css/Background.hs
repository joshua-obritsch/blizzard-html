{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Blizzard.Css.Background
    ( -- * background
      Background(..)
    , All2(..)
    , BackgroundPosition'(..)

      -- * background-attachment
    , BackgroundAttachment

      -- __Constants__
    , local

      -- __Functions__
    , backgroundAttachment
    , backgroundAttachments

      -- * background-color

      -- __Functions__
    , backgroundColor

      -- * background-position
    , BackgroundPosition(..)

      -- __Constants__
    --, placed
    --, positioned

      -- __Functions__
    --, backgroundPosition
    --, backgroundPositions

      -- * background-size
    , BackgroundSize
    , backgroundSize
    , backgroundSizes
    , contain, cover
    , by

      -- * background-repeat
    , BackgroundRepeat
    , backgroundRepeat
    , backgroundRepeats
    , repeat, space, round, noRepeat
    , repeatX, repeatY
    , repeatXy

      -- * background-origin
    , BackgroundOrigin
    , backgroundOrigin
    , backgroundOrigins
    , origin

      -- * background-clip
    , BackgroundClip
    , backgroundClip
    , backgroundClips
    , boxClip

      -- * background-image
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

import Blizzard.Css.Box (BoxType)
import Blizzard.Css.Color (Color)
import Blizzard.Css.Common
    ( Fixed
    , Inherit(..)
    , Initial
    , None
    , Other
    , Revert
    , RevertLayer
    , Scroll(..)
    , Unset
    )
import Blizzard.Css.Property (Val, Value, value)
import Blizzard.Css.Size (Angle, Size)
import Blizzard.Css.Stylesheet (prop)
import Blizzard.Internal (Attribute(..))
import Blizzard.Internal.Warning (warning)


class Val a => Background a where
    background :: a -> Attribute
    background = prop "background"


instance Background a => Background [a]
instance (Background a, Background b) => Background (a, b)

instance Background Color
--instance Background BackgroundPosition
instance Background BackgroundSize
instance Background BackgroundRepeat
instance Background BackgroundOrigin
instance Background BackgroundClip
instance Background BackgroundAttachment
instance Background BackgroundImage


-- | Encompasses any value accepted by the CSS property __background-attachment__.
--
-- Accepted values:
--
-- +--------+--------+
-- | Input  | Output |
-- +========+========+
-- | fixed  | fixed  |
-- +--------+--------+
-- | local  | local  |
-- +--------+--------+
-- | scroll | scroll |
-- +--------+--------+
newtype BackgroundAttachment = BackgroundAttachment Value
    deriving
        ( Fixed
        , Inherit
        , Initial
        , Revert
        , RevertLayer
        , Scroll
        , Unset
        , Val
        )


local :: BackgroundAttachment
local = BackgroundAttachment "local"


-- | Corresponds to the CSS property __background-attachment__.
--
-- __Examples:__
--
-- >>> backgroundAttachment fixed
-- "background-attachment:fixed"
backgroundAttachment :: BackgroundAttachment -> Attribute
backgroundAttachment = prop "background-attachment"


-- | Corresponds to the CSS property __background-attachment__.
--
-- __Examples:__
--
-- >>> backgroundAttachments [scroll, fixed]
-- "background-attachment:scroll,fixed"
backgroundAttachments :: [BackgroundAttachment] -> Attribute
backgroundAttachments [] = prop "background-attachment" backgroundAttachmentsWarning
backgroundAttachments xs = prop "background-attachment" xs


-- Prints a warning message and defaults when 'backgroundAttachments' is called with an empty list.
backgroundAttachmentsWarning :: [BackgroundAttachment]
backgroundAttachmentsWarning = warning [scroll]
    "Warning: 'backgroundAttachments' called with empty list. Defaulting to 'scroll'."


-- | Corresponds to the CSS property __background-color__.
--
-- __Examples:__
--
-- >>> backgroundColor (rgb 120 75 182)
-- "background-color:#784bb6"
backgroundColor :: Color -> Attribute
backgroundColor = prop "background-color"


-- | Encompasses any value accepted by the CSS property __background-position__.
--
-- Accepted values:
--
-- +--------------+--------------+
-- | Input        | Output       |
-- +==============+==============+
-- | \<position\> | \<position\> |
-- +--------------+--------------+
newtype BackgroundPosition' = BackgroundPosition' Value
    deriving
        ( Inherit
        , Initial
        , Revert
        , RevertLayer
        , Unset
        , All2
        , Val
        )


class Val a => BackgroundPosition a where
    backgroundPosition :: a -> Attribute
    backgroundPosition = prop "background-position"

    --backgroundPositions :: [a] -> Attribute
    --backgroundPositions = prop "background-position"


instance BackgroundPosition BackgroundPosition'
--instance BackgroundPosition (Size a)


class    Val a => All2 a     where all2 :: a
instance All2 Value where all2 = "all2"

-- | Corresponds to the CSS property __background-position__.
--
-- __Examples:__
--
-- >>> backgroundPosition (pct 25)
-- "background-color:#784bb6"
--backgroundPosition :: BackgroundPosition -> Attribute
--backgroundPosition = prop "background-position"


--backgroundPositions :: [BackgroundPosition] -> Attribute
--backgroundPositions = prop "background-position"


--placed :: Side -> Side -> BackgroundPosition
--placed a b = BackgroundPosition $ value (a, b)


--positioned :: Size a -> Size a -> BackgroundPosition
--positioned a b = BackgroundPosition $ value (a, b)


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
