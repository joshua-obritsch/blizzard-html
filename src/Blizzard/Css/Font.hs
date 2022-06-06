{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Blizzard.Css.Font
    ( Font(..)
    , Optional(..)
    , Required(..)
    , fontColor
    , color
    , fontFamily
    , sansSerif
    , serif
    , monospace
    , cursive
    , fantasy
    , FontSize
    , fontSize
    , fontSizeCustom
    , xxSmall, xSmall, small, medium, large, xLarge, xxLarge, smaller, larger
    , FontStyle
    , fontStyle
    , italic, oblique
    , FontVariant
    , fontVariant
    , smallCaps
    , FontWeight
    , fontWeight
    , bold, bolder, lighter
    , weight
    , NamedFont
    , caption, icon, menu, messageBox, smallCaption, statusBar
    , lineHeight
    ) where


import Blizzard.Internal (Attribute(..))
import Clay.Color (Color)
import Clay.Common (Auto, Inherit, Other)
import Clay.Font
    ( Optional(..)
    , Required(..)
    , FontSize
    , xxSmall, xSmall, small, medium, large, xLarge, xxLarge, smaller, larger
    , FontStyle
    , italic, oblique
    , FontVariant
    , smallCaps
    , FontWeight
    , bold, bolder, lighter
    , weight
    , NamedFont
    , caption, icon, menu, messageBox, smallCaption, statusBar
    )
import Clay.Property (Literal(..), Val, Value, value)
import Clay.Size (Size)
import Clay.Stylesheet (key)
import Data.Text (Text)

import qualified Clay.Font as F


class Val a => Font a where
    font :: a -> Attribute
    font a = AttrCss $ key "font" a


instance Font (          Required a)
instance Font (Optional, Required a)


fontColor :: Color -> Attribute
fontColor a = AttrCss $ F.fontColor a


color :: Color -> Attribute
color a = AttrCss $ F.color a


newtype GenericFontFamily = GenericFontFamily Value
    deriving (Auto, Inherit, Other, Val)


sansSerif, serif, monospace, cursive, fantasy :: GenericFontFamily

sansSerif = GenericFontFamily "sans-serif"
serif     = GenericFontFamily "serif"
monospace = GenericFontFamily "monospace"
cursive   = GenericFontFamily "cursive"
fantasy   = GenericFontFamily "fantasy"


fontFamily :: [Text] -> [GenericFontFamily] -> Attribute
fontFamily a b = AttrCss $ key "font-family" $
    let sep = if null a || null b then "" else ", "
        in value (Literal <$> a) <> sep <> value b


fontSize :: Size a -> Attribute
fontSize a = AttrCss $ F.fontSize a


fontSizeCustom :: FontSize -> Attribute
fontSizeCustom a = AttrCss $ F.fontSizeCustom a


fontStyle :: FontStyle -> Attribute
fontStyle a = AttrCss $ F.fontStyle a


fontVariant :: FontVariant -> Attribute
fontVariant a = AttrCss $ F.fontVariant a


fontWeight :: FontWeight -> Attribute
fontWeight a = AttrCss $ F.fontWeight a


lineHeight :: Size a -> Attribute
lineHeight a = AttrCss $ F.lineHeight a
