{-# LANGUAGE NoImplicitPrelude #-}

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


import Data.Foldable          (foldr)
import Data.Function          ((.))
import Data.Monoid            ((<>), mempty)
import Data.Text.Lazy.Builder (Builder, singleton)
import Html                   (Html(..), Buildable(..), Translatable)


-- INTERNATIONALIZATION


-- | Translates all multilingual HTML text nodes and converts 'Html.Html' to 'Data.Text.Lazy.Builder'.
translate :: Translatable a => (a -> Builder) -> Html a -> Builder
translate lang html = case html of
    ParentNode startTag endTag []         []       -> startTag <>                     singleton '>' <>                    endTag
    ParentNode startTag endTag attributes []       -> startTag <> build attributes <> singleton '>' <>                    endTag
    ParentNode startTag endTag []         children -> startTag <>                     singleton '>' <> build' children <> endTag
    ParentNode startTag endTag attributes children -> startTag <> build attributes <> singleton '>' <> build' children <> endTag
    LeafNode   startTag        []                  -> startTag <>                     singleton '>'
    LeafNode   startTag        attributes          -> startTag <> build attributes <> singleton '>'
    RootNode   startTag                   []       -> startTag
    RootNode   startTag                   children -> startTag <>                                      build' children
    TextNode   text                                -> text
    IntlNode   intl                                -> text
      where text = lang intl
  where
    build' = foldr ((<>) . translate lang) mempty


-- | Generates a multilingual HTML text node given a set of languages.
intl :: Translatable a => a -> Html a
intl = IntlNode
{-# INLINE intl #-}
