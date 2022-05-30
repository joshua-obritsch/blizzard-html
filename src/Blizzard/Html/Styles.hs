{-# LANGUAGE OverloadedStrings #-}

module Blizzard.Html.Styles
    ( width
    ) where


import Data.Text (Text)

import Blizzard.Internal (Attribute(..), intToText)


width :: Int -> Attribute
width w = Style $ "w-[" <> intToText w <> "]"
