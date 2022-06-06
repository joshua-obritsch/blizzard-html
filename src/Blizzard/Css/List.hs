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


import Blizzard.Internal (Attribute(..))
import Clay.List
    ( ListStyleType
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
    , inside
    , outside
    , ListStyleImage
    , imageUrl
    )

import qualified Clay.List as L


listStyleType :: ListStyleType -> Attribute
listStyleType a = AttrCss $ L.listStyleType a


listStylePosition :: ListStylePosition -> Attribute
listStylePosition a = AttrCss $ L.listStylePosition a


listStyleImage :: ListStyleImage -> Attribute
listStyleImage a = AttrCss $ L.listStyleImage a


listStyle :: ListStyleType -> ListStylePosition -> ListStyleImage -> Attribute
listStyle a b c = AttrCss $ L.listStyle a b c
