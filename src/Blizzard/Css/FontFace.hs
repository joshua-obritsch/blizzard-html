{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Blizzard.Css.FontFace
    ( FontFaceFormat(..)
    , FontFaceSrc(..)
    , fontFaceSrc
    ) where


import Data.Text (Text)

import Blizzard.Internal (Attribute(..))
import Blizzard.Css.Common (call)
import Blizzard.Css.Property (Val(..), Value(..), quote)
import Blizzard.Css.Stylesheet (prop)


data FontFaceFormat
    = EmbeddedOpenType
    | OpenType
    | SVG
    | TrueType
    | WOFF
    | WOFF2
    deriving Show


formatName :: FontFaceFormat -> Text
formatName = \case
    EmbeddedOpenType -> "embedded-opentype"
    OpenType         -> "opentype"
    SVG              -> "svg"
    TrueType         -> "truetype"
    WOFF             -> "woff"
    WOFF2            -> "woff2"


data FontFaceSrc
    = FontFaceSrcUrl Text (Maybe FontFaceFormat)
    | FontFaceSrcLocal Text
    deriving Show


instance Val FontFaceSrc where
    value src = Value $ case src of
        FontFaceSrcLocal name        -> call "local" (quote name)
        FontFaceSrcUrl   url mformat -> call "url" (quote url) <> maybe "" (call "format" . quote . formatName) mformat


fontFaceSrc :: [FontFaceSrc] -> Attribute
fontFaceSrc = prop "src"
