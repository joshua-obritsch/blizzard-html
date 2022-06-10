{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

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


import Data.String (fromString)

import Blizzard.Internal (Attribute(..))
import Blizzard.Css.Common
    ( Auto
    , Baseline
    , Center
    , Inherit
    , Other
    )
import Blizzard.Css.Property ((!), Val, Value, value)
import Blizzard.Css.Size (Size)
import Blizzard.Css.Stylesheet (prop)


class FlexEnd      a where flexEnd      :: a
class FlexStart    a where flexStart    :: a
class SpaceAround  a where spaceAround  :: a
class SpaceBetween a where spaceBetween :: a
class SpaceEvenly  a where spaceEvenly  :: a
class Stretch      a where stretch      :: a


instance FlexEnd      Value where flexEnd      = "flex-end"
instance FlexStart    Value where flexStart    = "flex-start"
instance SpaceAround  Value where spaceAround  = "space-around"
instance SpaceBetween Value where spaceBetween = "space-between"
instance SpaceEvenly  Value where spaceEvenly  = "space-evenly"
instance Stretch      Value where stretch      = "stretch"


newtype AlignContentValue = AlignContentValue Value
    deriving
        ( Center
        , FlexEnd
        , FlexStart
        , Inherit
        , Other
        , SpaceAround
        , SpaceBetween
        , SpaceEvenly
        , Stretch
        , Val
        )


alignContent :: AlignContentValue -> Attribute
alignContent = prop "align-content"


newtype AlignItemsValue = AlignItemsValue Value
    deriving
        ( Baseline
        , Center
        , FlexEnd
        , FlexStart
        , Inherit
        , Other
        , Stretch
        , Val
        )


alignItems :: AlignItemsValue -> Attribute
alignItems = prop "align-items"


newtype AlignSelfValue = AlignSelfValue Value
    deriving
        ( Auto
        , Baseline
        , Center
        , FlexEnd
        , FlexStart
        , Inherit
        , Other
        , Stretch
        , Val
        )


alignSelf :: AlignSelfValue -> Attribute
alignSelf = prop "align-self"


flex :: Int -> Int -> Size b -> Attribute
flex a b c = prop "flex" (a' ! b' ! value b)
  where
    a' = fromString . show $ a :: Value
    b' = fromString . show $ b :: Value


flexBasis :: Size a -> Attribute
flexBasis = prop "flex-basis"


newtype FlexDirection = FlexDirection Value
    deriving (Other, Val)


flexDirection :: FlexDirection -> Attribute
flexDirection = prop "flex-direction"


column, columnReverse, row, rowReverse :: FlexDirection

row           = FlexDirection "row"
rowReverse    = FlexDirection "row-reverse"
column        = FlexDirection "column"
columnReverse = FlexDirection "column-reverse"


flexFlow :: FlexDirection -> FlexWrap -> Attribute
flexFlow a b = prop "flex-flow" (a ! b)


flexGrow :: Int -> Attribute
flexGrow = prop "flex-grow" . fromString' . show
  where
    fromString' :: String -> Value
    fromString' = fromString


flexShrink :: Int -> Attribute
flexShrink = prop "flex-shrink" . fromString' . show
  where
    fromString' :: String -> Value
    fromString' = fromString


newtype FlexWrap = FlexWrap Value
    deriving (Other, Val)


flexWrap :: FlexWrap -> Attribute
flexWrap = prop "flex-wrap"


nowrap, wrap, wrapReverse :: FlexWrap

nowrap      = FlexWrap "nowrap"
wrap        = FlexWrap "wrap"
wrapReverse = FlexWrap "wrap-reverse"


newtype JustifyContentValue = JustifyContentValue Value
    deriving
        ( Center
        , FlexEnd
        , FlexStart
        , Inherit
        , Other
        , SpaceAround
        , SpaceBetween
        , SpaceEvenly
        , Val
        )


justifyContent :: JustifyContentValue -> Attribute
justifyContent = prop "justify-content"


order :: Int -> Attribute
order = prop "order" . fromString' . show
  where
    fromString' :: String -> Value
    fromString' = fromString
