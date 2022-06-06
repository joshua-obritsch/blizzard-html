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
    , maskPosition
    , maskPositions
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
import Clay.Background
    ( BackgroundPosition
    , BackgroundSize
    , BackgroundRepeat
    , BackgroundOrigin
    , BackgroundClip
    , BackgroundAttachment
    , BackgroundImage
    )
import Clay.Common (browsers)
import Clay.Mask
    ( MaskComposite
    , clear, copy
    , sourceOver, sourceIn, sourceOut, sourceAtop
    , destinationOver, destinationIn, destinationOut, destinationAtop
    , xor
    )
import Clay.Property (Prefixed, Val)
import Clay.Stylesheet (prefixed)


pkey :: Val a => Prefixed -> a -> Attribute
pkey k a = AttrCss $ prefixed (browsers <> k) a


class Val a => Mask a where
    mask :: a -> Attribute
    mask = pkey "mask"


instance Mask a => Mask [a]
instance (Mask a, Mask b) => Mask (a, b)

instance Mask MaskComposite
instance Mask BackgroundPosition
instance Mask BackgroundSize
instance Mask BackgroundRepeat
instance Mask BackgroundOrigin
instance Mask BackgroundClip
instance Mask BackgroundAttachment
instance Mask BackgroundImage


maskComposite :: MaskComposite -> Attribute
maskComposite = pkey "mask-composite"


maskComposites :: [MaskComposite] -> Attribute
maskComposites = pkey "mask-composite"


maskPosition :: BackgroundPosition -> Attribute
maskPosition = pkey "mask-position"


maskPositions :: [BackgroundPosition] -> Attribute
maskPositions = pkey "mask-position"


maskSize :: BackgroundSize -> Attribute
maskSize = pkey "mask-size"


maskSizes :: [BackgroundSize] -> Attribute
maskSizes = pkey "mask-size"


maskRepeat :: BackgroundRepeat -> Attribute
maskRepeat = pkey "mask-repeat"


maskRepeats :: [BackgroundRepeat] -> Attribute
maskRepeats = pkey "mask-repeat"


maskImage :: BackgroundImage -> Attribute
maskImage = pkey "mask-image"


maskImages :: [BackgroundImage] -> Attribute
maskImages = pkey "mask-image"


maskOrigin :: BackgroundOrigin -> Attribute
maskOrigin = pkey "mask-origin"


maskOrigins :: [BackgroundOrigin] -> Attribute
maskOrigins = pkey "mask-origin"


maskClip :: BackgroundClip -> Attribute
maskClip = pkey "mask-clip"


maskClips :: [BackgroundClip] -> Attribute
maskClips = pkey "mask-clip"


maskAttachment :: BackgroundAttachment -> Attribute
maskAttachment = pkey "mask-attachment"


maskAttachments :: [BackgroundAttachment] -> Attribute
maskAttachments = pkey "mask-attachment"
