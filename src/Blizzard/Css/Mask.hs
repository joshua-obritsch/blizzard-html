{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Blizzard.Css.Mask
    ( Mask(..)
    , MaskComposite
    , clear, copy
    , sourceOver, sourceIn, sourceOut, sourceAtop
    , destinationOver, destinationIn, destinationOut, destinationAtop
    , xor
    , maskComposite
    , maskComposites
    --, maskPosition
    --, maskPositions
    , maskSize
    , maskSizes
    , maskRepeat
    , maskRepeats
    , maskImage
    , maskImages
    , maskOrigin
    , maskOrigins
    , maskClip
    , maskClips
    , maskAttachment
    , maskAttachments
    ) where


import Blizzard.Internal (Attribute(..))
import Blizzard.Css.Background
    ( BackgroundPosition
    , BackgroundSize
    , BackgroundRepeat
    , BackgroundOrigin
    , BackgroundClip
    , BackgroundAttachment
    , BackgroundImage
    )
import Blizzard.Css.Common (Inherit(..), None, Other(..))
import Blizzard.Css.Property (Val, Value)
import Blizzard.Css.Stylesheet (prop)


class Val a => Mask a where
    mask :: a -> Attribute
    mask = prop "mask"


instance Mask a => Mask [a]
instance (Mask a, Mask b) => Mask (a, b)

instance Mask MaskComposite
--instance Mask BackgroundPosition
instance Mask BackgroundSize
instance Mask BackgroundRepeat
instance Mask BackgroundOrigin
instance Mask BackgroundClip
instance Mask BackgroundAttachment
instance Mask BackgroundImage


newtype MaskComposite = MaskComposite Value
    deriving (Inherit, None, Other, Val)


maskComposite :: MaskComposite -> Attribute
maskComposite = prop "mask-composite"


maskComposites :: [MaskComposite] -> Attribute
maskComposites = prop "mask-composite"


clear, copy
    , sourceOver, sourceIn, sourceOut, sourceAtop
    , destinationOver, destinationIn, destinationOut, destinationAtop
    , xor :: MaskComposite

clear           = other "clear"
copy            = other "copy"
sourceOver      = other "source-over"
sourceIn        = other "source-in"
sourceOut       = other "source-out"
sourceAtop      = other "source-atop"
destinationOver = other "destination-over"
destinationIn   = other "destination-in"
destinationOut  = other "destination-out"
destinationAtop = other "destination-atop"
xor             = other "xor"


--maskPosition :: BackgroundPosition -> Attribute
--maskPosition = prop "mask-position"


--maskPositions :: [BackgroundPosition] -> Attribute
--maskPositions = prop "mask-position"


maskSize :: BackgroundSize -> Attribute
maskSize = prop "mask-size"


maskSizes :: [BackgroundSize] -> Attribute
maskSizes = prop "mask-size"


maskRepeat :: BackgroundRepeat -> Attribute
maskRepeat = prop "mask-repeat"


maskRepeats :: [BackgroundRepeat] -> Attribute
maskRepeats = prop "mask-repeat"


maskImage :: BackgroundImage -> Attribute
maskImage = prop "mask-image"


maskImages :: [BackgroundImage] -> Attribute
maskImages = prop "mask-image"


maskOrigin :: BackgroundOrigin -> Attribute
maskOrigin = prop "mask-origin"


maskOrigins :: [BackgroundOrigin] -> Attribute
maskOrigins = prop "mask-origin"


maskClip :: BackgroundClip -> Attribute
maskClip = prop "mask-clip"


maskClips :: [BackgroundClip] -> Attribute
maskClips = prop "mask-clip"


maskAttachment :: BackgroundAttachment -> Attribute
maskAttachment = prop "mask-attachment"


maskAttachments :: [BackgroundAttachment] -> Attribute
maskAttachments = prop "mask-attachment"
