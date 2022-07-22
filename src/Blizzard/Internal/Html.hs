{-# LANGUAGE OverloadedStrings #-}

module Blizzard.Internal.Html
    ( documentTag
    , normalTag
    , voidTag
    ) where


import Data.Text (Text)
import Text.Blaze.Html


documentTag :: (Html -> Html) -> [Html] -> Html
documentTag element []       = element $ toHtml ("" :: Text)
documentTag element children = element $ foldl1 (>>) children


normalTag :: (Html -> Html) -> [Attribute] -> [Html] -> Html
normalTag element attributes []       = foldl (!) element attributes $ toHtml ("" :: Text)
normalTag element attributes children = foldl (!) element attributes $ foldl1 (>>) children


voidTag :: Html -> [Attribute] -> Html
voidTag = foldl (!)
