{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Blizzard.Css.Color
    ( Color
    , hsl
    , hsla
    , rgb
    , rgba
    ) where


import Data.Text (Text, pack)
import Text.Printf (printf)

import Blizzard.Css.Common
    ( Auto(..)
    , Inherit(..)
    , None(..)
    , Other(..)
    )
import Blizzard.Css.Property (Val, Value, value)


data Color
    = Rgba Int Int   Int   Float
    | Hsla Int Float Float Float
    | Other Value
    deriving (Eq, Show)


instance Val Color where
    value = \case
        Rgba  r g b 1.0 -> value $ mconcat ["#",     h r,      h g,      h b                ]
        Rgba  r g b a   -> value $ mconcat ["rgba(", t r, ",", t g, ",", t b, ",", o a, ")" ]
        Hsla  h s l 1.0 -> value $ mconcat ["hsl(",  t h, ",", p s, ",", p l,            ")"]
        Hsla  h s l a   -> value $ mconcat ["hsla(", t h, ",", p s, ",", p l, ",", o a, ")" ]
        Other other     -> other
      where
        h = pack . printf "%02x"   -- hexadecimal
        o = pack . take 6 . show   -- opacity
        p = pack . printf "%.4f%%" -- percentage
        t = pack . show            -- text


instance Auto    Color where auto    = Other "auto"
instance Inherit Color where inherit = Other "inherit"
instance None    Color where none    = Other "none"
instance Other   Color where other   = Other


rgba :: Int -> Int -> Int -> Float -> Color
rgba = Rgba


rgb :: Int -> Int -> Int -> Color
rgb r g b = rgba r g b 1


hsla :: Int -> Float -> Float -> Float -> Color
hsla = Hsla


hsl :: Int -> Float -> Float -> Color
hsl h s l = hsla h s l 1
