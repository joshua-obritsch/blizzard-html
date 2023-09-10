-- | Module    : Html.Intl
-- Copyright   : (c) Joshua Obritsch, 2021
-- License     : MIT
-- Maintainer  : joshua@obritsch.com
-- Stability   : Experimental
--
-- The "Html.Intl" module provides a set of functions for internationalization in HTML.
module Html.Intl
    ( -- * Internationalization
      -- ** translate
      translate
      -- ** intl
    , intl
    ) where


import Data.Text.Lazy.Builder (Builder)
import Html (Html(..), Translatable)


-- INTERNATIONALIZATION


-- | Converts all multilingual HTML text nodes to simple HTML text nodes given a target language and HTML.
translate :: Translatable a => (a -> Builder) -> Html a -> Html a
translate lang html = case html of
    ParentNode startTag endTag attributes children -> ParentNode startTag endTag attributes (map (translate lang) children)
    RootNode   startTag                   children -> RootNode   startTag                   (map (translate lang) children)
    LeafNode   startTag        attributes          -> LeafNode   startTag        attributes
    TextNode   text                                -> TextNode   text
    IntlNode   intl                                -> TextNode   text
      where text = lang intl


-- | Generates a multilingual HTML text node given a set of languages.
intl :: Translatable a => a -> Html a
intl = IntlNode
{-# INLINE intl #-}
