{-# LANGUAGE OverloadedStrings #-}

module Blizzard.Html.Attributes
    ( alt
    , src
    ) where


import Data.Text (Text)

import Blizzard.Internal (Attribute(..))


alt :: Text -> Attribute
alt = Attr "alt"


src :: Text -> Attribute
src = Attr "src"
