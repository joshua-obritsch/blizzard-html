{-# LANGUAGE OverloadedStrings #-}

module Blizzard.Internal.Html
    ( documentTag
    , normalTag
    , voidTag
    ) where


import Data.Maybe (catMaybes)
import Data.Text (Text)
import Text.Blaze.Html


documentTag :: (Html -> Html) -> [Html] -> Html
documentTag element []       = element $ toHtml ("" :: Text)
documentTag element children = element $ foldl1 (>>) children


normalTag :: (Html -> Html) -> [Maybe Attribute] -> [Html] -> Html
normalTag element attributes []       = foldl (!) element (catMaybes attributes) $ toHtml ("" :: Text)
normalTag element attributes children = foldl (!) element (catMaybes attributes) $ foldl1 (>>) children


voidTag :: Html -> [Maybe Attribute] -> Html
voidTag element attributes = foldl (!) element (catMaybes attributes)
