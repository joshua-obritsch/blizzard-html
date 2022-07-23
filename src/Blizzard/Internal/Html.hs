{-# LANGUAGE OverloadedStrings #-}

module Blizzard.Internal.Html
    ( boolAttribute
    , textAttribute
    , documentTag
    , normalTag
    , voidTag
    ) where


import Data.Maybe (catMaybes)
import Data.Text (Text, null)
import Prelude hiding (null)
import Text.Blaze.Html


boolAttribute :: Attribute -> Bool -> Maybe Attribute
boolAttribute attr False = Nothing
boolAttribute attr True  = Just attr


textAttribute :: (AttributeValue -> Attribute) -> Text -> Maybe Attribute
textAttribute attr text | null text = Nothing
textAttribute attr text             = Just . attr . textValue $ text


documentTag :: (Html -> Html) -> [Html] -> Html
documentTag element []       = element $ toHtml ("" :: Text)
documentTag element children = element $ foldl1 (>>) children


normalTag :: (Html -> Html) -> [Maybe Attribute] -> [Html] -> Html
normalTag element attributes []       = foldl (!) element (catMaybes attributes) $ toHtml ("" :: Text)
normalTag element attributes children = foldl (!) element (catMaybes attributes) $ foldl1 (>>) children


voidTag :: Html -> [Maybe Attribute] -> Html
voidTag element attributes = foldl (!) element (catMaybes attributes)
