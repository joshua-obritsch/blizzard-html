{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Blizzard.Internal
    ( Attribute(..)
    , documentTag
    , normalTag
    , voidTag
    ) where


import Clay.Stylesheet (Css)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Text.Blaze.Html ((!), Html, customAttribute, textTag, textValue, toHtml)

import qualified Text.Blaze.Html as H


-- PUBLIC DATA TYPES


data Attribute
    = AttrCss Css
    | AttrRaw Text Text


-- PUBLIC FUNCTIONS


documentTag :: (Html -> Html) -> [Html] -> Html
documentTag element []       = element $ toHtml ("" :: String)
documentTag element children = element $ foldl1 (>>) children


normalTag :: (Html -> Html) -> [Attribute] -> [Html] -> Html
normalTag element attributes []       = foldl (!) element (split attributes) $ toHtml ("" :: String)
normalTag element attributes children = foldl (!) element (split attributes) $ foldl1 (>>) children


voidTag :: Html -> [Attribute] -> Html
voidTag element attributes = foldl (!) element (split attributes)


-- HELPER FUNCTIONS


split :: [Attribute] -> [H.Attribute]
split = catMaybes . mapCase
  where
    mapCase = map $ \case
        AttrRaw tag value -> Just $ customAttribute (textTag tag) (textValue value)
        _                 -> Nothing
