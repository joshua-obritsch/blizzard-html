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

import Data.Text (Text, pack)

import Blizzard.Internal (Attribute(..))
import Blizzard.Css.Color (Color)
import Blizzard.Css.Common (Auto, Inherit, Normal, Other)
import Blizzard.Css.Property ((!), Literal(..), Val, Value, value)
import Blizzard.Css.Size (Size)
import Blizzard.Css.Stylesheet (prop)


class Val a => Font a where
    font :: a -> Attribute
    font = prop "font"


data Optional = Optional (Maybe FontWeight) (Maybe FontVariant) (Maybe FontStyle)


instance Val Optional where
    value (Optional a b c) = value (a ! b ! c)


data Required a = Required (Size a) (Maybe (Size a)) [Text] [GenericFontFamily]


instance Val (Required a) where
    value (Required a Nothing  c d) = value a <> " " <> value (Literal <$> c) <> sep <> value d
      where sep = if null c || null d then "" else ","
    value (Required a (Just b) c d) = value ((value a <> "/" <> value b) ! (Literal <$> c) ! d)


instance Font (          Required a)
instance Font (Optional, Required a)


fontColor :: Color -> Attribute
fontColor = prop "color"


color :: Color -> Attribute
color = prop "color"


newtype GenericFontFamily = GenericFontFamily Value
    deriving (Auto, Inherit, Other, Val)


cursive, fantasy, monospace, sansSerif, serif :: GenericFontFamily

cursive   = GenericFontFamily "cursive"
fantasy   = GenericFontFamily "fantasy"
monospace = GenericFontFamily "monospace"
sansSerif = GenericFontFamily "sans-serif"
serif     = GenericFontFamily "serif"


fontFamily :: [Text] -> [GenericFontFamily] -> Attribute
fontFamily a b = prop "font-family" $
    let sep = if null a || null b then "" else ", "
        in value (Literal <$> a) <> sep <> value b


newtype FontSize = FontSize Value
    deriving (Auto, Inherit, Other, Val)


large, larger, medium, small, smaller, xLarge, xSmall, xxLarge, xxSmall :: FontSize

large   = FontSize "large"
larger  = FontSize "larger"
medium  = FontSize "medium"
small   = FontSize "small"
smaller = FontSize "smaller"
xLarge  = FontSize "x-large"
xSmall  = FontSize "x-small"
xxLarge = FontSize "xx-large"
xxSmall = FontSize "xx-small"


fontSize :: Size a -> Attribute
fontSize = prop "font-size"


fontSizeCustom :: FontSize -> Attribute
fontSizeCustom = prop "font-size"


newtype FontStyle = FontStyle Value
    deriving (Inherit, Normal, Other, Val)


fontStyle :: FontStyle -> Attribute
fontStyle = prop "font-style"


italic, oblique :: FontStyle

italic  = FontStyle "italic"
oblique = FontStyle "oblique"


newtype FontVariant = FontVariant Value
    deriving (Inherit, Normal, Other, Val)


fontVariant :: FontVariant -> Attribute
fontVariant = prop "font-variant"


smallCaps :: FontVariant
smallCaps = FontVariant "small-caps"


newtype FontWeight = FontWeight Value
    deriving (Inherit, Normal, Other, Val)


fontWeight :: FontWeight -> Attribute
fontWeight = prop "font-weight"


weight :: Int -> FontWeight
weight = FontWeight . value . pack . show


bold, bolder, lighter :: FontWeight

bold    = FontWeight "bold"
bolder  = FontWeight "bolder"
lighter = FontWeight "lighter"


newtype NamedFont = NamedFont Value
    deriving (Other, Val)


caption, icon, menu, messageBox, smallCaption, statusBar :: NamedFont

caption      = NamedFont "caption"
icon         = NamedFont "icon"
menu         = NamedFont "menu"
messageBox   = NamedFont "message-box"
smallCaption = NamedFont "small-caption"
statusBar    = NamedFont "status-bar"


lineHeight :: Size a -> Attribute
lineHeight = prop "line-height"
