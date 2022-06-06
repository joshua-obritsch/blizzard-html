module Blizzard.Css.Flexbox
    ( FlexEnd(..)
    , FlexStart(..)
    , SpaceAround(..)
    , SpaceBetween(..)
    , SpaceEvenly(..)
    , Stretch(..)
    , AlignContentValue
    , alignContent
    , AlignItemsValue
    , alignItems
    , AlignSelfValue
    , alignSelf
    , flex
    , flexBasis
    , FlexDirection
    , flexDirection
    , row, rowReverse, column, columnReverse
    , flexFlow
    , flexGrow
    , flexShrink
    , FlexWrap
    , flexWrap
    , nowrap, wrap, wrapReverse
    , JustifyContentValue
    , justifyContent
    , order
    ) where


import Blizzard.Internal (Attribute(..))
import Clay.Flexbox
    ( FlexEnd(..)
    , FlexStart(..)
    , SpaceAround(..)
    , SpaceBetween(..)
    , SpaceEvenly(..)
    , Stretch(..)
    , AlignContentValue
    , AlignItemsValue
    , AlignSelfValue
    , FlexDirection
    , row, rowReverse, column, columnReverse
    , FlexWrap
    , nowrap, wrap, wrapReverse
    , JustifyContentValue
    )
import Clay.Size (Size)

import qualified Clay.Flexbox as F


alignContent :: AlignContentValue -> Attribute
alignContent a = AttrCss $ F.alignContent a


alignItems :: AlignItemsValue -> Attribute
alignItems a = AttrCss $ F.alignItems a


alignSelf :: AlignSelfValue -> Attribute
alignSelf a = AttrCss $ F.alignSelf a


flex :: Int -> Int -> Size b -> Attribute
flex a b c = AttrCss $ F.flex a b c


flexBasis :: Size a -> Attribute
flexBasis a = AttrCss $ F.flexBasis a


flexDirection :: FlexDirection -> Attribute
flexDirection a = AttrCss $ F.flexDirection a


flexFlow :: FlexDirection -> FlexWrap -> Attribute
flexFlow a b = AttrCss $ F.flexFlow a b


flexGrow :: Int -> Attribute
flexGrow a = AttrCss $ F.flexGrow a


flexShrink :: Int -> Attribute
flexShrink a = AttrCss $ F.flexShrink a


flexWrap :: FlexWrap -> Attribute
flexWrap a = AttrCss $ F.flexWrap a


justifyContent :: JustifyContentValue -> Attribute
justifyContent a = AttrCss $ F.justifyContent a


order :: Int -> Attribute
order a = AttrCss $ F.order a
