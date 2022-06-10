{-# LANGUAGE OverloadedStrings #-}

module Blizzard.Css.Stylesheet
    ( prop
    ) where


import Data.Text (Text)

import Blizzard.Css.Property (Val, Value(..), value)
import Blizzard.Internal (Attribute(..))


prop :: Val a => Text -> a -> Attribute
prop k v = Css $ k <> ":" <> unvalue (value v)
