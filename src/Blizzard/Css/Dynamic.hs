module Blizzard.Css.Dynamic
    ( UserInput
    , userInput
    , inputEnabled, inputDisabled
    , UserModify
    , userModify
    , readOnly, readWrite, writeOnly
    , UserSelect
    , userSelect
    , selectText, selectToggle, selectElement, selectElements
    , UserFocus
    , userFocus
    , selectAll, selectBefore, selectAfter, selectSame, selectMenu
    ) where


import Blizzard.Internal (Attribute(..))
import Clay.Dynamic
    ( UserInput
    , inputEnabled, inputDisabled
    , UserModify
    , readOnly, readWrite, writeOnly
    , UserSelect
    , selectText, selectToggle, selectElement, selectElements
    , UserFocus
    , selectAll, selectBefore, selectAfter, selectSame, selectMenu
    )

import qualified Clay.Dynamic as D


userInput :: UserInput -> Attribute
userInput a = AttrCss $ D.userInput a


userModify :: UserModify -> Attribute
userModify a = AttrCss $ D.userModify a


userSelect :: UserSelect -> Attribute
userSelect a = AttrCss $ D.userSelect a


userFocus :: UserFocus -> Attribute
userFocus a = AttrCss $ D.userFocus a
