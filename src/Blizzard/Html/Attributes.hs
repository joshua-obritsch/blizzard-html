{-# LANGUAGE NoImplicitPrelude #-}

module Blizzard.Html.Attributes
    ( module Text.Blaze.Html5.Attributes
    , css
    ) where


import Data.Text (Text, unwords)
import Prelude ((.))
import Text.Blaze.Html5 (Attribute, textValue)
import Text.Blaze.Html5.Attributes


css :: [Text] -> Attribute
css = class_ . textValue . unwords
