{-# LANGUAGE OverloadedStrings #-}

module Text.Blizzard.Css
    ( css
    ) where


import Clay (Css, compact, renderWith)
import Data.String (fromString)
import Data.Text (unpack)
import Data.Text.Lazy (toStrict)
import Text.Blaze.Html5 (Attribute)
import Text.Blaze.Html5.Attributes (style)


css :: [Css] -> Attribute
css []     = style ""
css styles = style . fromString . firstLast . unpack . toStrict . renderWith compact [] . foldl1 (>>) $ styles

firstLast :: [a] -> [a]
firstLast []  = []
firstLast [x] = []
firstLast xs  = tail (init xs)
