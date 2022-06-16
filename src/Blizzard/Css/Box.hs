{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Blizzard.Css.Box
    ( BoxType
    , paddingBox, borderBox, contentBox
    , boxSizing
    , boxShadow
    , shadow
    , shadowWithBlur
    , shadowWithSpread
    , bsInset
    , bsColor
    ) where


import Blizzard.Internal (Attribute(..))
import Blizzard.Css.Color (Color)
import Blizzard.Css.Common (Inherit, Initial, None, Other, Unset)
import Blizzard.Css.Property ((!), Val, Value, value)
import Blizzard.Css.Size (Size)
import Blizzard.Css.Stylesheet (prop)
import Data.List.NonEmpty (NonEmpty)


newtype BoxType = BoxType Value
    deriving (Inherit, Val)


borderBox, contentBox, paddingBox :: BoxType

borderBox  = BoxType "border-box"
contentBox = BoxType "content-box"
paddingBox = BoxType "padding-box"


boxSizing :: BoxType -> Attribute
boxSizing = prop "box-sizing"


newtype BoxShadow = BoxShadow Value
    deriving (Val, Inherit, Initial, Unset, None, Other)


boxShadow :: NonEmpty BoxShadow -> Attribute
boxShadow = prop "box-shadow" . value


shadow :: Size a -> Size a -> BoxShadow
shadow x y = BoxShadow . value $ (x ! y)


shadowWithBlur :: Size a -> Size a -> Size a -> BoxShadow
shadowWithBlur x y w = BoxShadow . value $ (x ! y ! w)


shadowWithSpread :: Size a -> Size a -> Size a -> Size a -> BoxShadow
shadowWithSpread x y blurRadius spreadRadius =
    BoxShadow . value $ (x ! y ! blurRadius ! spreadRadius)


bsInset :: BoxShadow -> BoxShadow
bsInset (BoxShadow v) = BoxShadow . value $ ("inset" :: Value, v)


bsColor :: Color -> BoxShadow -> BoxShadow
bsColor c (BoxShadow v) = BoxShadow . value $ (v, c)
infixr 9 `bsColor`
