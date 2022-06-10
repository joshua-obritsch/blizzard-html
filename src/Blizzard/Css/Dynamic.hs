{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

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
import Blizzard.Css.Common (All, Auto, Inherit, None, Normal)
import Blizzard.Css.Property (Val, Value)
import Blizzard.Css.Stylesheet (prop)


newtype UserInput = UserInput Value
    deriving (Inherit, None, Val)


userInput :: UserInput -> Attribute
userInput = prop "user-input"


inputDisabled, inputEnabled :: UserInput

inputDisabled = UserInput "disabled"
inputEnabled  = UserInput "enabled"


newtype UserModify = UserModify Value
    deriving (Inherit, Val)


userModify :: UserModify -> Attribute
userModify = prop "user-modify"


readOnly, readWrite, writeOnly :: UserModify

readOnly  = UserModify "readonly"
readWrite = UserModify "read-write"
writeOnly = UserModify "write-only"


newtype UserSelect = UserSelect Value
    deriving (All, Inherit, None, Val)


userSelect :: UserSelect -> Attribute
userSelect = prop "user-select"


selectElement, selectElements, selectText, selectToggle :: UserSelect

selectElement  = UserSelect "element"
selectElements = UserSelect "elements"
selectText     = UserSelect "text"
selectToggle   = UserSelect "toggle"


newtype UserFocus = UserFocus Value
    deriving (Auto, Inherit, None, Normal, Val)


userFocus :: UserFocus -> Attribute
userFocus = prop "user-focus"


selectAfter, selectAll, selectBefore, selectMenu, selectSame :: UserFocus

selectAfter  = UserFocus "select-after"
selectAll    = UserFocus "select-all"
selectBefore = UserFocus "select-before"
selectMenu   = UserFocus "select-menu"
selectSame   = UserFocus "select-same"
