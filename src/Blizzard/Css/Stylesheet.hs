{-# LANGUAGE OverloadedStrings #-}

module Blizzard.Css.Stylesheet
    ( key
    ) where


import Data.Text (Text)

import Blizzard.Css.Property (Val, Value(..), value)
import Blizzard.Internal (Attribute(..))


key :: Val a => Text -> a -> Attribute
key k v = Css $ k <> ":" <> unvalue (value v)
