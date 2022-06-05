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
import Clay.Box
    ( BoxType
    , paddingBox, borderBox, contentBox
    )
import Clay.Color (Color)
import Clay.Common (Inherit, Initial, None, Other, Unset, browsers)
import Clay.Property ((!), Val, Value, value)
import Clay.Size (Size)
import Clay.Stylesheet (prefixed)
import Data.List.NonEmpty (NonEmpty)

import qualified Clay.Box as B


boxSizing :: BoxType -> Attribute
boxSizing a = AttrCss $ B.boxSizing a


newtype BoxShadow = BoxShadow Value
    deriving (Val, Inherit, Initial, Unset, None, Other)


boxShadow :: NonEmpty BoxShadow -> Attribute
boxShadow a = AttrCss $ prefixed (browsers <> "box-shadow") . value $ a


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
