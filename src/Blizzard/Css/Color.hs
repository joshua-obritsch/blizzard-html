{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Blizzard.Css.Color
    ( Color
    ) where


import Data.Text (Text, pack)
import Text.Printf (printf)

import Blizzard.Css.Property (Val, Value, value)


data Color
    = Rgba Int Int   Int   Float
    | Hsla Int Float Float Float
    | Other Value
    deriving (Eq, Show)


instance Val Color where
    value = \case
        Rgba  r g b 1.0 -> value $ mconcat ["#",     p' r,      p' g,      p' b                ]
        Rgba  r g b a   -> value $ mconcat ["rgba(", p  r, ",", p  g, ",", p  b, ",", ah a, ")"]
        Hsla  h s l 1.0 -> value $ mconcat ["hsl(",  p  h, ",", f  s, ",", f  l,            ")"]
        Hsla  h s l a   -> value $ mconcat ["hsla(", p  h, ",", f  s, ",", f  l, ",", ah a, ")"]
        Other other     -> other
      where
        p  = pack . show
        p' = pack . printf "%02x"
        f  = pack . printf "%.4f%%"
        ah = pack . take 6 . show


rgba :: Int -> Int -> Int -> Float -> Color
rgba = Rgba


rgb :: Int -> Int -> Int -> Color
rgb r g b = rgba r g b 1


hsla :: Int -> Float -> Float -> Float -> Color
hsla = Hsla


hsl :: Int -> Float -> Float -> Color
hsl h s l = hsla h s l 1
