{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Blizzard.Internal
    ( Attribute(..)
    , Css(..)
    , documentTag
    , intToText
    , normalTag
    , toBlaze
    , voidTag
    ) where


import Prelude hiding (null, unwords)

import Data.Maybe (catMaybes)
import Data.Text (Text, null, unwords)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder (toLazyText)
import Data.Text.Lazy.Builder.Int (decimal)
import Text.Blaze.Html ((!), Html, customAttribute, textTag, textValue, toHtml)

import qualified Text.Blaze.Html as H


-- PUBLIC DATA TYPES


data Attribute
    = AttrCss Css
    | AttrMul [Css]
    | AttrRaw Text Text


data Css
    = Class Text
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
toBlaze attrs = case class_ of
    Just class_ -> case style of
        Just style -> class_ : style : attributes
        Nothing    -> class_ : attributes
    Nothing     -> case style of
        Just style -> style : attributes
        Nothing    -> attributes
  where
    attributes = splitAttributes attrs
    style = splitStyle attrs
    class_ = splitClass attrs


voidTag :: Html -> [Attribute] -> Html
voidTag element attributes = foldl (!) element (toBlaze attributes)


-- HELPER FUNCTIONS


splitAttributes :: [Attribute] -> [H.Attribute]
splitAttributes = catMaybes . mapCase
  where
    mapCase = map $ \case
        AttrRaw tag value -> Just $ customAttribute (textTag tag) (textValue value)
        _                 -> Nothing


unbatchClasses :: [Css] -> Text
unbatchClasses = unwords . catMaybes . mapCase
  where
    mapCase = map $ \case
        Class value -> Just value
        Style _     -> Nothing


unbatchStyles :: [Css] -> Text
unbatchStyles = unwords . catMaybes . mapCase
  where
    mapCase = map $ \case
        Class _     -> Nothing
        Style value -> Just value


splitClass :: [Attribute] -> Maybe H.Attribute
splitClass attrs =
    if null result then
        Nothing
    else
        Just $ customAttribute "class" . textValue $ result
  where
    mapCase = map $ \case
        AttrCss (Class value) -> Just value
        AttrMul values        -> Just $ unbatchClasses values
        _                     -> Nothing

    result = unwords . catMaybes . mapCase $ attrs


splitStyle :: [Attribute] -> Maybe H.Attribute
splitStyle attrs =
    if null result then
        Nothing
    else
        Just $ customAttribute "style" . textValue $ result
  where
    mapCase = map $ \case
        AttrCss (Style value) -> Just value
        AttrMul values        -> Just $ unbatchStyles values
        _                     -> Nothing

    result = unwords . catMaybes . mapCase $ attrs
