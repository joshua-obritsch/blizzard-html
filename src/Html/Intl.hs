-- | Module    : Html.Intl
-- Copyright   : (c) Joshua Obritsch, 2025
-- License     : MIT
-- Maintainer  : joshua@obritsch.com
-- Stability   : Experimental
--
-- The "Html.Intl" module provides a set of types and functions that facilitate internationalization.
module Html.Intl
    ( -- * Primitives
      -- ** Locale
      Locale(..)
      -- ** localize
    , localize
      -- ** text
    , text
    ) where


import Data.Maybe             (listToMaybe)
import Data.Text.Lazy.Builder (Builder, singleton)
import Html                   (Html(..))
import Html.Lazy.Builder      (ToLazyBuilder(..))
import Html.Locale            (Locale(..))


-- PRIMITIVES


localize :: Locale -> Html -> Builder
localize locale html = case html of
    BatchNode                          []    -> mempty
    BatchNode                       children ->                                                       localize' children
    EmptyNode                                -> mempty
    IntlNode                        text alt -> translate locale text alt
    LeafNode   start         []              -> start <>                             singleton '>'
    LeafNode   start     attributes          -> start <> toLazyBuilder attributes <> singleton '>'
    ParentNode start end     []        []    -> start <>                             singleton '>' <>                       end
    ParentNode start end attributes    []    -> start <> toLazyBuilder attributes <> singleton '>' <>                       end
    ParentNode start end     []     children -> start <>                             singleton '>' <> localize' children <> end
    ParentNode start end attributes children -> start <> toLazyBuilder attributes <> singleton '>' <> localize' children <> end
    RootNode   start                   []    -> start
    RootNode   start                children -> start <>                                              localize' children
    TextNode                          text   -> text
  where
    localize' :: [Html] -> Builder
    localize' = foldr ((<>) . localize locale) mempty


text :: Builder -> [(Locale, Builder)] -> Html
text = IntlNode


-- HELPER FUNCTIONS


translate :: Locale -> Builder -> [(Locale, Builder)] -> Builder
translate locale defaultText translations = case listToMaybe (filter (\(locale', _) -> locale == locale') translations) of
    Nothing -> defaultText
    Just (_, translation) -> translation
