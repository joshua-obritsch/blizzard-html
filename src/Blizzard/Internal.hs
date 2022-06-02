{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Blizzard.Internal
    ( Attribute(..)
    , documentTag
    , normalTag
    , voidTag
    ) where


import Clay.Render (compact, renderWith)
import Clay.Stylesheet (Css)
import Data.Maybe (catMaybes)
import Data.String (fromString)
import Data.Text (Text, unpack)
import Data.Text.Lazy (toStrict)
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
normalTag element attributes []       = foldl (!) element (toBlaze attributes) $ toHtml ("" :: String)
normalTag element attributes children = foldl (!) element (toBlaze attributes) $ foldl1 (>>) children


voidTag :: Html -> [Attribute] -> Html
voidTag element attributes = foldl (!) element (toBlaze attributes)


-- HELPER FUNCTIONS


toBlaze :: [Attribute] -> [H.Attribute]
toBlaze attrs = case class_ of
    Just class_ -> class_ : attributes
    Nothing     -> attributes
  where
    attributes = splitAttributes attrs
    class_ = splitClass attrs


splitAttributes :: [Attribute] -> [H.Attribute]
splitAttributes = catMaybes . mapCase
  where
    mapCase = map $ \case
        AttrRaw tag value -> Just $ customAttribute (textTag tag) (textValue value)
        _                 -> Nothing

splitClass :: [Attribute] -> Maybe H.Attribute
splitClass attrs = case result of
    Just value ->
        Just $ customAttribute "class" (textValue value)
    Nothing ->
        Nothing
  where
    mapCase = map $ \case
        AttrCss value -> Just value
        _             -> Nothing

    result = css . catMaybes . mapCase $ attrs


css :: [Css] -> Maybe Text
css []     = Nothing
css styles = Just $ fromString . firstLast . unpack . toStrict . renderWith compact [] . foldl1 (>>) $ styles

firstLast :: [a] -> [a]
firstLast []  = []
firstLast [x] = []
firstLast xs  = tail (init xs)
