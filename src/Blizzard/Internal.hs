{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Blizzard.Internal
    ( Attribute(..)
    , documentTag
    , intToText
    , normalTag
    , toBlaze
    , voidTag
    ) where


import Prelude hiding (unwords)

import Data.Maybe (catMaybes)
import Data.Text (Text, unwords)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder (toLazyText)
import Data.Text.Lazy.Builder.Int (decimal)
import Text.Blaze.Html ((!), Html, customAttribute, textTag, textValue, toHtml)

import qualified Text.Blaze.Html as H


-- PUBLIC DATA TYPES


data Attribute
    = Attr Text Text
    | Style Text


-- PUBLIC FUNCTIONS


documentTag :: (Html -> Html) -> [Html] -> Html
documentTag element []       = element $ toHtml ("" :: String)
documentTag element children = element $ foldl1 (>>) children


intToText :: Integral a => a -> Text
intToText = toStrict . toLazyText . decimal


normalTag :: (Html -> Html) -> [Attribute] -> [Html] -> Html
normalTag element attributes []       = foldl (!) element (toBlaze attributes) $ toHtml ("" :: String)
normalTag element attributes children = foldl (!) element (toBlaze attributes) $ foldl1 (>>) children


toBlaze :: [Attribute] -> [H.Attribute]
toBlaze attrs = styles attrs : attributes attrs


voidTag :: Html -> [Attribute] -> Html
voidTag element attributes = foldl (!) element (toBlaze attributes)


-- HELPER FUNCTIONS


attributes :: [Attribute] -> [H.Attribute]
attributes = catMaybes . mapCase
  where
    mapCase = map $ \case
        Attr tag value -> Just $ customAttribute (textTag tag) (textValue value)
        _              -> Nothing


styles :: [Attribute] -> H.Attribute
styles = customAttribute "styles" . textValue . unwords . catMaybes . mapCase
  where
    mapCase = map $ \case
        Style value -> Just value
        _           -> Nothing
