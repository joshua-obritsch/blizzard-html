{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Blizzard.Css.List
    ( ListStyleType
    , listStyleType
    , disc
    , armenian
    , circleListStyle
    , cjkIdeographic
    , decimal
    , decimalLeadingZero
    , georgian
    , hebrew
    , hiragana
    , hiraganaIroha
    , katakana
    , katakanaIroha
    , lowerAlpha
    , lowerGreek
    , lowerLatin
    , lowerRoman
    , square
    , upperAlpha
    , upperLatin
    , upperRoman
    , ListStylePosition
    , listStylePosition
    , inside
    , outside
    , ListStyleImage
    , listStyleImage
    , imageUrl
    , listStyle
    ) where


import Data.Text (Text)

import Blizzard.Internal (Attribute(..))
import Blizzard.Css.Common (Inherit, Initial, None, Other)
import Blizzard.Css.Property ((!), Literal(..), Val(..), Value)
import Blizzard.Css.Stylesheet (prop)


newtype ListStyleType = ListStyleType Value
    deriving (Inherit, Initial, None, Other, Val)


disc, armenian, circleListStyle, cjkIdeographic, decimal, decimalLeadingZero, georgian
    , hebrew, hiragana, hiraganaIroha, katakana, katakanaIroha, lowerAlpha
    , lowerGreek, lowerLatin, lowerRoman, square, upperAlpha, upperLatin, upperRoman :: ListStyleType

disc               = ListStyleType "disc"
armenian           = ListStyleType "armenian"
circleListStyle    = ListStyleType "circle"
cjkIdeographic     = ListStyleType "cjk-ideographic"
decimal            = ListStyleType "decimal"
decimalLeadingZero = ListStyleType "decimal-leading-zero"
georgian           = ListStyleType "georgian"
hebrew             = ListStyleType "hebrew"
hiragana           = ListStyleType "hiragana"
hiraganaIroha      = ListStyleType "hiragana-iroha"
katakana           = ListStyleType "katakana"
katakanaIroha      = ListStyleType "katakana-iroha"
lowerAlpha         = ListStyleType "lower-alpha"
lowerGreek         = ListStyleType "lower-greek"
lowerLatin         = ListStyleType "lower-latin"
lowerRoman         = ListStyleType "lower-roman"
square             = ListStyleType "square"
upperAlpha         = ListStyleType "upper-alpha"
upperLatin         = ListStyleType "upper-latin"
upperRoman         = ListStyleType "upper-roman"


listStyleType :: ListStyleType -> Attribute
listStyleType = prop "list-style-type"


newtype ListStylePosition = ListStylePosition Value
    deriving (Inherit, Initial, Other, Val)


listStylePosition :: ListStylePosition -> Attribute
listStylePosition = prop "list-style-position"


inside, outside :: ListStylePosition

inside  = ListStylePosition "inside"
outside = ListStylePosition "outside"


newtype ListStyleImage = ListStyleImage Value
    deriving (Inherit, Initial, None, Other, Val)


listStyleImage :: ListStyleImage -> Attribute
listStyleImage = prop "list-style-image"


imageUrl :: Text -> ListStyleImage
imageUrl a = ListStyleImage $ "url(" <> value (Literal a) <> ")"


listStyle :: ListStyleType -> ListStylePosition -> ListStyleImage -> Attribute
listStyle a b c = prop "list-style" (a ! b ! c)
