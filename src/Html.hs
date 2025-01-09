{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}


-- | Module    : Html
-- Copyright   : (c) Joshua Obritsch, 2021
-- License     : MIT
-- Maintainer  : joshua@obritsch.com
-- Stability   : Experimental
--
-- The "Html" module provides a set of types, classes and functions for generating HTML elements.
--
-- These elements along with their attributes and event handlers, found in the "Html.Attributes" and "Html.Events" modules respectively, can
-- be used to dynamically compose HTML documents natively in Haskell, without relying on templating engines or other techniques that can be
-- error-prone and difficult to maintain.
module Html
    ( -- * Types
      -- ** Html
      Html(..)
      -- ** Attribute
    , Attribute(..)

      -- * Classes
      -- ** Buildable
    , Buildable(..)

      -- * Declarations
      -- ** \<!DOCTYPE\>
    , doctype

      -- * Elements
      -- ** \<a\>
    , a
      -- ** \<abbr\>
    , abbr
      -- ** \<address\>
    , address
      -- ** \<area\>
    , area
      -- ** \<article\>
    , article
      -- ** \<aside\>
    , aside
      -- ** \<audio\>
    , audio
      -- ** \<b\>
    , b
      -- ** \<base\>
    , base
      -- ** \<bdi\>
    , bdi
      -- ** \<bdo\>
    , bdo
      -- ** \<blockquote\>
    , blockquote
      -- ** \<body\>
    , body
      -- ** \<br\>
    , br
      -- ** \<button\>
    , button
      -- ** \<canvas\>
    , canvas
      -- ** \<caption\>
    , caption
      -- ** \<cite\>
    , cite
      -- ** \<code\>
    , code
      -- ** \<col\>
    , col
      -- ** \<colgroup\>
    , colgroup
      -- ** \<data\>
    , data_
      -- ** \<datalist\>
    , datalist
      -- ** \<dd\>
    , dd
      -- ** \<del\>
    , del
      -- ** \<details\>
    , details
      -- ** \<dfn\>
    , dfn
      -- ** \<dialog\>
    , dialog
      -- ** \<div\>
    , div
      -- ** \<dl\>
    , dl
      -- ** \<dt\>
    , dt
      -- ** \<em\>
    , em
      -- ** \<embed\>
    , embed
      -- ** \<fieldset\>
    , fieldset
      -- ** \<figcaption\>
    , figcaption
      -- ** \<figure\>
    , figure
      -- ** \<footer\>
    , footer
      -- ** \<form\>
    , form
      -- ** \<h1\>
    , h1
      -- ** \<h2\>
    , h2
      -- ** \<h3\>
    , h3
      -- ** \<h4\>
    , h4
      -- ** \<h5\>
    , h5
      -- ** \<h6\>
    , h6
      -- ** \<head\>
    , head
      -- ** \<header\>
    , header
      -- ** \<hgroup\>
    , hgroup
      -- ** \<hr\>
    , hr
      -- ** \<html\>
    , html
      -- ** \<i\>
    , i
      -- ** \<iframe\>
    , iframe
      -- ** \<img\>
    , img
      -- ** \<input\>
    , input
      -- ** \<ins\>
    , ins
      -- ** \<kbd\>
    , kbd
      -- ** \<label\>
    , label
      -- ** \<legend\>
    , legend
      -- ** \<li\>
    , li
      -- ** \<link\>
    , link
      -- ** \<main\>
    , main
      -- ** \<map\>
    , map
      -- ** \<mark\>
    , mark
      -- ** \<menu\>
    , menu
      -- ** \<meta\>
    , meta
      -- ** \<meter\>
    , meter
      -- ** \<nav\>
    , nav
      -- ** \<noscript\>
    , noscript
      -- ** \<object\>
    , object
      -- ** \<ol\>
    , ol
      -- ** \<optgroup\>
    , optgroup
      -- ** \<option\>
    , option
      -- ** \<output\>
    , output
      -- ** \<p\>
    , p
      -- ** \<picture\>
    , picture
      -- ** \<pre\>
    , pre
      -- ** \<progress\>
    , progress
      -- ** \<q\>
    , q
      -- ** \<rp\>
    , rp
      -- ** \<rt\>
    , rt
      -- ** \<ruby\>
    , ruby
      -- ** \<s\>
    , s
      -- ** \<samp\>
    , samp
      -- ** \<script\>
    , script
      -- ** \<search\>
    , search
      -- ** \<section\>
    , section
      -- ** \<select\>
    , select
      -- ** \<slot\>
    , slot
      -- ** \<small\>
    , small
      -- ** \<source\>
    , source
      -- ** \<span\>
    , span
      -- ** \<strong\>
    , strong
      -- ** \<style\>
    , style
      -- ** \<sub\>
    , sub
      -- ** \<summary\>
    , summary
      -- ** \<sup\>
    , sup
      -- ** \<table\>
    , table
      -- ** \<tbody\>
    , tbody
      -- ** \<td\>
    , td
      -- ** \<template\>
    , template
      -- ** \<textarea\>
    , textarea
      -- ** \<tfoot\>
    , tfoot
      -- ** \<th\>
    , th
      -- ** \<thead\>
    , thead
      -- ** \<time\>
    , time
      -- ** \<title\>
    , title
      -- ** \<tr\>
    , tr
      -- ** \<track\>
    , track
      -- ** \<u\>
    , u
      -- ** \<ul\>
    , ul
      -- ** \<var\>
    , var
      -- ** \<video\>
    , video
      -- ** \<wbr\>
    , wbr
    ) where


import Prelude hiding (div, head, map, span)

import Data.String            (IsString(..))
import Data.Text.Lazy         (unpack)
import Data.Text.Lazy.Builder (Builder, singleton, toLazyText)


-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import           Html.Attributes (href)
-- >>> import qualified Html.Attributes as Attributes


-- TYPES


-- | Represents an HTML element.
data Html

    -- | Constructs an HTML parent node.
    = ParentNode Builder Builder [Attribute] [Html]

    -- | Constructs an HTML leaf node.
    | LeafNode Builder [Attribute]

    -- | Constructs an HTML root node.
    | RootNode Builder [Html]

    -- | Constructs an HTML text node.
    | TextNode Builder


instance IsString (Html) where
    fromString = TextNode . fromString


instance Show (Html) where
    show = unpack . toLazyText . build


instance Buildable (Html) where
    build html = case html of
        ParentNode startTag endTag []         []       -> startTag <>                     singleton '>' <>                   endTag
        ParentNode startTag endTag attributes []       -> startTag <> build attributes <> singleton '>' <>                   endTag
        ParentNode startTag endTag []         children -> startTag <>                     singleton '>' <> build children <> endTag
        ParentNode startTag endTag attributes children -> startTag <> build attributes <> singleton '>' <> build children <> endTag
        LeafNode   startTag        []                  -> startTag <>                     singleton '>'
        LeafNode   startTag        attributes          -> startTag <> build attributes <> singleton '>'
        RootNode   startTag                   []       -> startTag
        RootNode   startTag                   children -> startTag <>                                      build children
        TextNode   text                                -> text


instance {-# OVERLAPPING #-} Show [Html] where
    show = unpack . toLazyText . build


instance Buildable [Html] where
    build = foldr ((<>) . build) mempty


-- | Represents an HTML attribute.
data Attribute

    -- | Constructs a boolean HTML attribute.
    = BoolAttribute Builder Bool

    -- | Constructs a textual HTML attribute.
    | TextAttribute Builder Builder


instance Show Attribute where
    show = unpack . toLazyText . build


instance Buildable Attribute where
    build attribute = case attribute of
        BoolAttribute _   False -> mempty
        BoolAttribute key True  -> key
        TextAttribute key value -> key <> value <> singleton '"'


instance {-# OVERLAPPING #-} Show [Attribute] where
    show = unpack . toLazyText . build


instance Buildable [Attribute] where
    build = foldr ((<>) . build) mempty


-- CLASSES


-- | Enables conversion to 'Data.Text.Lazy.Builder.Builder'.
class Buildable a where

    -- | Converts to 'Data.Text.Lazy.Builder.Builder'.
    build :: a -> Builder


-- DECLARATIONS


-- | Generates an HTML @\<!DOCTYPE\>@ declaration with the given contents.
doctype :: [Html] -> Html
doctype = RootNode "<!DOCTYPE html>\n"
{-# INLINE doctype #-}


-- ELEMENTS


-- | Generates an HTML @\<a\>@ element with the given attributes and contents.
--
-- [@Description@]: Hyperlink
-- [@Categories@]: /flow/; /phrasing/*; /interactive/; /palpable/
-- [@Parents@]: /phrasing/
-- [@Children@]: /transparent/*
-- [@Attributes@]: /globals/; /'Html.Attributes.href'/; /'Html.Attributes.target'/; /'Html.Attributes.download'/; /'Html.Attributes.ping'/; /'Html.Attributes.rel'/; /'Html.Attributes.hreflang'/; /'Html.Attributes.type_'/; /'Html.Attributes.referrerpolicy'/
-- [@Interface@]: /HTMLAnchorElement/
-- [@Example@]:
--
-- >>> a [ href "https://example.com" ] [ "Click here" ]
-- <a href="https://example.com">Click here</a>
--
-- /* Indicates that the rules are more complicated./
a :: [Attribute] -> [Html] -> Html
a = ParentNode "<a" "</a>"
{-# INLINE a #-}


-- | Generates an HTML @\<abbr\>@ element with the given attributes and contents.
--
-- [@Description@]: Abbreviation
-- [@Categories@]: /flow/; /phrasing/; /palpable/
-- [@Parents@]: /phrasing/
-- [@Children@]: /phrasing/
-- [@Attributes@]: /globals/
-- [@Interface@]: /HTMLElement/
-- [@Example@]:
--
-- >>> abbr [ Attributes.title "HyperText Markup Language" ] [ "HTML" ]
-- <abbr title="HyperText Markup Language">HTML</abbr>
--
-- /Note: This element collides with the 'Html.Attributes.abbr' attribute./
abbr :: [Attribute] -> [Html] -> Html
abbr = ParentNode "<abbr" "</abbr>"
{-# INLINE abbr #-}


-- | Generates an HTML @\<address\>@ element with the given attributes and contents.
--
-- [@Description@]: Contract information for a page or 'Html.article' element
-- [@Categories@]: /flow/; /palpable/
-- [@Parents@]: /flow/
-- [@Children@]: /flow/*
-- [@Attributes@]: /globals/
-- [@Interface@]: /HTMLElement/
-- [@Example@]:
--
-- >>> :{
-- address []
--     [ "123 Main Street"
--     , br []
--     , "Anytown, USA"
--     ]
-- :}
-- <address>123 Main Street<br>Anytown, USA</address>
--
-- /* Indicates that the rules are more complicated./
address :: [Attribute] -> [Html] -> Html
address = ParentNode "<address" "</address>"
{-# INLINE address #-}


-- | Generates an HTML @\<area\>@ element with the given attributes.
--
-- [@Description@]: Hyperlink or dead area on an image map
-- [@Categories@]: /flow/; /phrasing/
-- [@Parents@]: /phrasing/*
-- [@Children@]: empty
-- [@Attributes@]: /globals/; /'Html.Attributes.alt'/; /'Html.Attributes.coords'/; /'Html.Attributes.shape'/; /'Html.Attributes.href'/; /'Html.Attributes.target'/; /'Html.Attributes.download'/; /'Html.Attributes.ping'/; /'Html.Attributes.rel'/; /'Html.Attributes.referrerpolicy'/
-- [@Interface@]: /HTMLElement/
-- [@Example@]:
--
-- >>> :{
-- area
--     [ href "https://example.com"
--     , alt "Example"
--     , coords "34,44,270,350"
--     , shape "rect"
--     ]
-- :}
-- <area href="https://example.com" alt="Example" coords="34,44,270,350" shape="rect">
--
-- /* Indicates that the rules are more complicated./
area :: [Attribute] -> Html
area = LeafNode "<area"
{-# INLINE area #-}


-- | Generates an HTML @\<article\>@ element with the given attributes and contents.
--
-- [@Description@]: Self-contained syndicatable or reusable composition
-- [@Categories@]: /flow/; /sectioning/; /palpable/
-- [@Parents@]: /flow/
-- [@Children@]: /flow/
-- [@Attributes@]: /globals/
-- [@Interface@]: /HTMLElement/
-- [@Example@]:
--
-- >>> article [] [ h2 [] [ "Title" ], p [] [ "Body" ] ]
-- <article><h2>Title</h2><p>Body</p></article>
article :: [Attribute] -> [Html] -> Html
article = ParentNode "<article" "</article>"
{-# INLINE article #-}


-- | Generates an HTML @\<aside\>@ element with the given attributes and contents.
--
-- [@Description@]: Sidebar for tangentially related content
-- [@Categories@]: /flow/; /sectioning/; /palpable/
-- [@Parents@]: /flow/
-- [@Children@]: /flow/
-- [@Attributes@]: /globals/
-- [@Interface@]: /HTMLElement/
-- [@Example@]:
--
-- > aside
-- >     [ class_ "sidebar" ]
-- >     [ h3 [] [ "Related Articles" ]
-- >     , ul []
-- >         [ li [] [ a [ href "https://example.com/article1" ] [ "Article 1" ] ]
-- >         , li [] [ a [ href "https://example.com/article2" ] [ "Article 2" ] ]
-- >         ]
-- >     ]
aside :: [Attribute] -> [Html] -> Html
aside = ParentNode "<aside" "</aside>"
{-# INLINE aside #-}


-- | Generates an HTML @\<audio\>@ element with the given attributes and contents.
--
-- [@Description@]: Audio player
-- [@Categories@]: /flow/; /phrasing/; /embedded/; /interactive/; /palpable/*
-- [@Parents@]: /phrasing/
-- [@Children@]: /'Html.source'/*; /'Html.track'/*; /transparent/*
-- [@Attributes@]: /globals/; /'Html.Attributes.src'/; /'Html.Attributes.crossorigin'/; /'Html.Attributes.preload'/; /'Html.Attributes.autoplay'/; /'Html.Attributes.loop'/; /'Html.Attributes.muted'/; /'Html.Attributes.controls'/
-- [@Interface@]: /HTMLAudioElement/
-- [@Example@]:
--
-- > audio
-- >     [ controls True ]
-- >     [ source [ src "audio-file.mp3", type_ "audio/mpeg" ]
-- >     , p [] [ "Your browser does not support the audio element." ]
-- >     ]
--
-- /* Indicates that the rules are more complicated./
audio :: [Attribute] -> [Html] -> Html
audio = ParentNode "<audio" "</audio>"
{-# INLINE audio #-}


-- | Generates an HTML @\<b\>@ element with the given attributes and contents.
--
-- [@Description@]: Keywords
-- [@Categories@]: /flow/; /phrasing/; /palpable/
-- [@Parents@]: /phrasing/
-- [@Children@]: /phrasing/
-- [@Attributes@]: /globals/
-- [@Interface@]: /HTMLElement/
-- [@Example@]:
--
-- > b [] [ "This text is bold." ]
b :: [Attribute] -> [Html] -> Html
b = ParentNode "<b" "</b>"
{-# INLINE b #-}


-- | Generates an HTML @\<base\>@ element with the given attributes.
--
-- [@Description@]: Base URL and default target navigable for hyperlinks and forms
-- [@Categories@]: /metadata/
-- [@Parents@]: /'Html.head'/
-- [@Children@]: empty
-- [@Attributes@]: /globals/; /'Html.Attributes.href'/; /'Html.Attributes.target'/
-- [@Interface@]: /HTMLBaseElement/
-- [@Example@]:
--
-- > base [ href "https://example.com/" ]
base :: [Attribute] -> Html
base = LeafNode "<base"
{-# INLINE base #-}


-- | Generates an HTML @\<bdi\>@ element with the given attributes and contents.
--
-- [@Description@]: Text directionality isolation
-- [@Categories@]: /flow/; /phrasing/; /palpable/
-- [@Parents@]: /phrasing/
-- [@Children@]: /phrasing/
-- [@Attributes@]: /globals/
-- [@Interface@]: /HTMLElement/
-- [@Example@]:
--
-- > bdi [] [ "This is some text that may need bidirectional isolation." ]
bdi :: [Attribute] -> [Html] -> Html
bdi = ParentNode "<bdi" "</bdi>"
{-# INLINE bdi #-}


-- | Generates an HTML @\<bdo\>@ element with the given attributes and contents.
--
-- [@Description@]: Text directionality formatting
-- [@Categories@]: /flow/; /phrasing/; /palpable/
-- [@Parents@]: /phrasing/
-- [@Children@]: /phrasing/
-- [@Attributes@]: /globals/
-- [@Interface@]: /HTMLElement/
-- [@Example@]:
--
-- > bdo [ dir "rtl" ] [ "This text will be displayed right-to-left." ]
bdo :: [Attribute] -> [Html] -> Html
bdo = ParentNode "<bdo" "</bdo>"
{-# INLINE bdo #-}


-- | Generates an HTML @\<blockquote\>@ element with the given attributes and contents.
--
-- [@Description@]: A section quoted from another source
-- [@Categories@]: /flow/; /palpable/
-- [@Parents@]: /flow/
-- [@Children@]: /flow/
-- [@Attributes@]: /globals/; /'Html.Attributes.cite'/
-- [@Interface@]: /HTMLQuoteElement/
-- [@Example@]:
--
-- > blockquote
-- >     [ cite "https://example.com/source" ]
-- >     [ p [] [ "This is a quoted text from a source." ] ]
blockquote :: [Attribute] -> [Html] -> Html
blockquote = ParentNode "<blockquote" "</blockquote>"
{-# INLINE blockquote #-}


-- | Generates an HTML @\<body\>@ element with the given attributes and contents.
--
-- [@Description@]: Document body
-- [@Categories@]: none
-- [@Parents@]: /'Html.html'/
-- [@Children@]: /flow/
-- [@Attributes@]: /globals/; /'Html.Events.onafterprint'/; /'Html.Events.onbeforeprint'/; /'Html.Events.onbeforeunload'/; /'Html.Events.onhashchange'/; /'Html.Events.onlanguagechange'/; /'Html.Events.onmessage'/; /'Html.Events.onmessageerror'/; /'Html.Events.onoffline'/; /'Html.Events.ononline'/; /'Html.Events.onpageswap'/; /'Html.Events.onpagehide'/; /'Html.Events.onpagereveal'/; /'Html.Events.onpageshow'/; /'Html.Events.onpopstate'/; /'Html.Events.onrejectionhandled'/; /'Html.Events.onstorage'/; /'Html.Events.onunhandledrejection'/; /'Html.Events.onunload'/
-- [@Interface@]: /HTMLBodyElement/
-- [@Example@]:
--
-- >>> body [] [ h1 [] [ "Title" ], p [] [ "Body" ] ]
-- <body><h1>Title</h1><p>Body</p></body>
body :: [Attribute] -> [Html] -> Html
body = ParentNode "<body" "</body>"
{-# INLINE body #-}


-- | Generates an HTML @\<br\>@ element with the given attributes.
--
-- [@Description@]: Line break, e.g. in poem or postal address
-- [@Categories@]: /flow/; /phrasing/
-- [@Parents@]: /phrasing/
-- [@Children@]: empty
-- [@Attributes@]: /globals/
-- [@Interface@]: /HTMLBRElement/
-- [@Example@]:
--
-- >>> br []
-- <br>
br :: [Attribute] -> Html
br = LeafNode "<br"
{-# INLINE br #-}


-- | Generates an HTML @\<button\>@ element with the given attributes and contents.
--
-- [@Description@]: Button control
-- [@Categories@]: /flow/; /phrasing/; /interactive/; /listed/; /labelable/; /submittable/; /form-associated/; /palpable/
-- [@Parents@]: /phrasing/
-- [@Children@]: /phrasing/*
-- [@Attributes@]: /globals/; /'Html.Attributes.disabled'/; /'Html.Attributes.form'/; /'Html.Attributes.formaction'/; /'Html.Attributes.formenctype'/; /'Html.Attributes.formmethod'/; /'Html.Attributes.formnovalidate'/; /'Html.Attributes.formtarget'/; /'Html.Attributes.name'/; /'Html.Attributes.popovertarget'/; /'Html.Attributes.popovertargetaction'/; /'Html.Attributes.type_'/; /'Html.Attributes.value'/
-- [@Interface@]: /HTMLButtonElement/
-- [@Example@]:
--
-- >>> button [ type_ "submit" ] [ "Submit" ]
-- <button type="submit">Submit</button>
--
-- /* Indicates that the rules are more complicated./
button :: [Attribute] -> [Html] -> Html
button = ParentNode "<button" "</button>"
{-# INLINE button #-}


-- | Generates an HTML @\<canvas\>@ element with the given attributes and contents.
--
-- [@Description@]: Scriptable bitmap canvas
-- [@Categories@]: /flow/; /phrasing/; /embedded/; /palpable/
-- [@Parents@]: /phrasing/
-- [@Children@]: /transparent/
-- [@Attributes@]: /globals/; /'Html.Attributes.width'/; /'Html.Attributes.height'/
-- [@Interface@]: /HTMLCanvasElement/
-- [@Example@]:
--
-- >>> canvas [ width "300", height "150" ] []
-- <canvas width="300" height="150"></canvas>
canvas :: [Attribute] -> [Html] -> Html
canvas = ParentNode "<canvas" "</canvas>"
{-# INLINE canvas #-}


-- | Generates an HTML @\<caption\>@ element with the given attributes and contents.
--
-- [@Description@]: Table caption
-- [@Categories@]: none
-- [@Parents@]: /'Html.table'/
-- [@Children@]: /flow/*
-- [@Attributes@]: /globals/
-- [@Interface@]: /HTMLTableCaptionElement/
--
-- /* Indicates that the rules are more complicated./
caption :: [Attribute] -> [Html] -> Html
caption = ParentNode "<caption" "</caption>"
{-# INLINE caption #-}


-- | Generates an HTML @\<cite\>@ element with the given attributes and contents.
--
-- [@Description@]: Title of a work
-- [@Categories@]: /flow/; /phrasing/; /palpable/
-- [@Parents@]: /phrasing/
-- [@Children@]: /phrasing/
-- [@Attributes@]: /globals/
-- [@Interface@]: /HTMLElement/
cite :: [Attribute] -> [Html] -> Html
cite = ParentNode "<cite" "</cite>"
{-# INLINE cite #-}


-- | Generates an HTML @\<code\>@ element with the given attributes and contents.
--
-- [@Description@]: Computer code
-- [@Categories@]: /flow/; /phrasing/; /palpable/
-- [@Parents@]: /phrasing/
-- [@Children@]: /phrasing/
-- [@Attributes@]: /globals/
-- [@Interface@]: /HTMLElement/
code :: [Attribute] -> [Html] -> Html
code = ParentNode "<code" "</code>"
{-# INLINE code #-}


-- | Generates an HTML @\<col\>@ element with the given attributes.
--
-- [@Description@]: Table column
-- [@Categories@]: none
-- [@Parents@]: /'Html.colgroup'/
-- [@Children@]: empty
-- [@Attributes@]: /globals/; /'Html.Attributes.span'/
-- [@Interface@]: /HTMLTableColElement/
col :: [Attribute] -> Html
col = LeafNode "<col"
{-# INLINE col #-}


-- | Generates an HTML @\<colgroup\>@ element with the given attributes and contents.
--
-- [@Description@]: Group of columns in a table
-- [@Categories@]: none
-- [@Parents@]: /'Html.table'/
-- [@Children@]: /'Html.col'/*; /'Html.template'/*
-- [@Attributes@]: /globals/; /'Html.Attributes.span'/
-- [@Interface@]: /HTMLTableColElement/
--
-- /* Indicates that the rules are more complicated./
colgroup :: [Attribute] -> [Html] -> Html
colgroup = ParentNode "<colgroup" "</colgroup>"
{-# INLINE colgroup #-}


-- | Generates an HTML @\<data\>@ element with the given attributes and contents.
--
-- [@Description@]: Machine-readable equivalent
-- [@Categories@]: /flow/; /phrasing/; /palpable/
-- [@Parents@]: /phrasing/
-- [@Children@]: /phrasing/
-- [@Attributes@]: /globals/; /'Html.Attributes.value'/
-- [@Interface@]: /HTMLDataElement/
data_ :: [Attribute] -> [Html] -> Html
data_ = ParentNode "<data" "</data>"
{-# INLINE data_ #-}


-- | Generates an HTML @\<datalist\>@ element with the given attributes and contents.
--
-- [@Description@]: Container for options for combo box control
-- [@Categories@]: /flow/; /phrasing/
-- [@Parents@]: /phrasing/
-- [@Children@]: /phrasing/*; /'Html.option'/*; /script-supporting elements/*
-- [@Attributes@]: /globals/
-- [@Interface@]: /HTMLDataListElement/
--
-- /* Indicates that the rules are more complicated./
datalist :: [Attribute] -> [Html] -> Html
datalist = ParentNode "<datalist" "</datalist>"
{-# INLINE datalist #-}


-- | Generates an HTML @\<dd\>@ element with the given attributes and contents.
--
-- [@Description@]: Content for corresponding /'Html.dt'/ element(s)
-- [@Categories@]: none
-- [@Parents@]: /'Html.dl'/; /'Html.div'/*
-- [@Children@]: /flow/
-- [@Attributes@]: /globals/
-- [@Interface@]: /HTMLElement/
--
-- /* Indicates that the rules are more complicated./
dd :: [Attribute] -> [Html] -> Html
dd = ParentNode "<dd" "</dd>"
{-# INLINE dd #-}


-- | Generates an HTML @\<del\>@ element with the given attributes and contents.
--
-- [@Description@]: A removal from the document
-- [@Categories@]: /flow/; /phrasing/*; /palpable/
-- [@Parents@]: /phrasing/
-- [@Children@]: /transparent/
-- [@Attributes@]: /globals/; /'Html.Attributes.cite'/; /'Html.Attributes.datetime'/
-- [@Interface@]: /HTMLModElement/
--
-- /* Indicates that the rules are more complicated./
del :: [Attribute] -> [Html] -> Html
del = ParentNode "<del" "</del>"
{-# INLINE del #-}


-- | Generates an HTML @\<details\>@ element with the given attributes and contents.
--
-- [@Description@]: Disclosure control for hiding details
-- [@Categories@]: /flow/; /interactive/; /palpable/
-- [@Parents@]: /flow/
-- [@Children@]: /'Html.summary'/*; /flow/
-- [@Attributes@]: /globals/; /'Html.Attributes.name'/; /'Html.Attributes.open'/
-- [@Interface@]: /HTMLDetailsElement/
--
-- /* Indicates that the rules are more complicated./
details :: [Attribute] -> [Html] -> Html
details = ParentNode "<details" "</details>"
{-# INLINE details #-}


-- | Generates an HTML @\<dfn\>@ element with the given attributes and contents.
--
-- [@Description@]: Defining instance
-- [@Categories@]: /flow/; /phrasing/; /palpable/
-- [@Parents@]: /phrasing/
-- [@Children@]: /phrasing/*
-- [@Attributes@]: /globals/
-- [@Interface@]: /HTMLElement/
--
-- /* Indicates that the rules are more complicated./
dfn :: [Attribute] -> [Html] -> Html
dfn = ParentNode "<dfn" "</dfn>"
{-# INLINE dfn #-}


-- | Generates an HTML @\<dialog\>@ element with the given attributes and contents.
--
-- [@Description@]: Dialog box or window
-- [@Categories@]: /flow/
-- [@Parents@]: /flow/
-- [@Children@]: /flow/
-- [@Attributes@]: /globals/; /'Html.Attributes.open'/
-- [@Interface@]: /HTMLDialogElement/
dialog :: [Attribute] -> [Html] -> Html
dialog = ParentNode "<dialog" "</dialog>"
{-# INLINE dialog #-}


-- | Generates an HTML @\<div\>@ element with the given attributes and contents.
--
-- [@Description@]: Generic flow container, or container for name-value groups in /'Html.dl'/ elements
-- [@Categories@]: /flow/; /palpable/
-- [@Parents@]: /flow/; /'Html.dl'/
-- [@Children@]: /flow/
-- [@Attributes@]: /globals/
-- [@Interface@]: /HTMLDivElement/
div :: [Attribute] -> [Html] -> Html
div = ParentNode "<div" "</div>"
{-# INLINE div #-}


-- | Generates an HTML @\<dl\>@ element with the given attributes and contents.
--
-- [@Description@]: Association list consisting of zero or more name-value groups
-- [@Categories@]: /flow/; /palpable/
-- [@Parents@]: /flow/
-- [@Children@]: /'Html.dt'/*; /'Html.dd'/*; /'Html.div'/*; /script-supporting elements/
-- [@Attributes@]: /globals/
-- [@Interface@]: /HTMLDListElement/
--
-- /* Indicates that the rules are more complicated./
dl :: [Attribute] -> [Html] -> Html
dl = ParentNode "<dl" "</dl>"
{-# INLINE dl #-}


-- | Generates an HTML @\<dt\>@ element with the given attributes and contents.
--
-- [@Description@]: Legend for corresponding /'Html.dd'/ element(s)
-- [@Categories@]: none
-- [@Parents@]: /'Html.dl'/; /'Html.div'/*
-- [@Children@]: /flow/*
-- [@Attributes@]: /globals/
-- [@Interface@]: /HTMLElement/
--
-- /* Indicates that the rules are more complicated./
dt :: [Attribute] -> [Html] -> Html
dt = ParentNode "<dt" "</dt>"
{-# INLINE dt #-}


-- | Generates an HTML @\<em\>@ element with the given attributes and contents.
--
-- [@Description@]: Stress emphasis
-- [@Categories@]: /flow/; /phrasing/; /palpable/
-- [@Parents@]: /phrasing/
-- [@Children@]: /phrasing/
-- [@Attributes@]: /globals/
-- [@Interface@]: /HTMLElement/
em :: [Attribute] -> [Html] -> Html
em = ParentNode "<em" "</em>"
{-# INLINE em #-}


-- | Generates an HTML @\<embed\>@ element with the given attributes.
--
-- [@Description@]: Plugin
-- [@Categories@]: /flow/; /phrasing/; /embedded/; /interactive/; /palpable/
-- [@Parents@]: /phrasing/
-- [@Children@]: none
-- [@Attributes@]: /globals/; /'Html.Attributes.src'/; /'Html.Attributes.type_'/; /'Html.Attributes.width'/; /'Html.Attributes.height'/; any*
-- [@Interface@]: /HTMLEmbedElement/
--
-- /* Indicates that the rules are more complicated./
embed :: [Attribute] -> Html
embed = LeafNode "<embed"
{-# INLINE embed #-}


-- | Generates an HTML @\<fieldset\>@ element with the given attributes and contents.
--
-- [@Description@]: Group of form controls
-- [@Categories@]: /flow/; /listed/; /form-associated/; /palpable/
-- [@Parents@]: /flow/
-- [@Children@]: /'Html.legend'/*; /flow/
-- [@Attributes@]: /globals/; /'Html.Attributes.disabled'/; /'Html.Attributes.form'/; /'Html.Attributes.name'/
-- [@Interface@]: /HTMLFieldSetElement/
--
-- /* Indicates that the rules are more complicated./
fieldset :: [Attribute] -> [Html] -> Html
fieldset = ParentNode "<fieldset" "</fieldset>"
{-# INLINE fieldset #-}


-- | Generates an HTML @\<figcaption\>@ element with the given attributes and contents.
--
-- [@Description@]: Caption for /'Html.figure'/
-- [@Categories@]: none
-- [@Parents@]: /'Html.figure'/
-- [@Children@]: /flow/
-- [@Attributes@]: /globals/
-- [@Interface@]: /HTMLElement/
figcaption :: [Attribute] -> [Html] -> Html
figcaption = ParentNode "<figcaption" "</figcaption>"
{-# INLINE figcaption #-}


-- | Generates an HTML @\<figure\>@ element with the given attributes and contents.
--
-- [@Description@]: Figure with optional caption
-- [@Categories@]: /flow/; /palpable/
-- [@Parents@]: /flow/
-- [@Children@]: /'Html.figcaption'/*; /flow/
-- [@Attributes@]: /globals/
-- [@Interface@]: /HTMLElement/
--
-- /* Indicates that the rules are more complicated./
figure :: [Attribute] -> [Html] -> Html
figure = ParentNode "<figure" "</figure>"
{-# INLINE figure #-}


-- | Generates an HTML @\<footer\>@ element with the given attributes and contents.
--
-- [@Description@]: Footer for a page or section
-- [@Categories@]: /flow/; /palpable/
-- [@Parents@]: /flow/
-- [@Children@]: /flow/*
-- [@Attributes@]: /globals/
-- [@Interface@]: /HTMLElement/
--
-- /* Indicates that the rules are more complicated./
footer :: [Attribute] -> [Html] -> Html
footer = ParentNode "<footer" "</footer>"
{-# INLINE footer #-}


-- | Generates an HTML @\<form\>@ element with the given attributes and contents.
--
-- [@Description@]: User-submittable form
-- [@Categories@]: /flow/; /palpable/
-- [@Parents@]: /flow/
-- [@Children@]: /flow/*
-- [@Attributes@]: /globals/; /'Html.Attributes.acceptCharset'/; /'Html.Attributes.action'/; /'Html.Attributes.autocomplete'/; /'Html.Attributes.enctype'/; /'Html.Attributes.method'/; /'Html.Attributes.name'/; /'Html.Attributes.novalidate'/; /'Html.Attributes.rel'/; /'Html.Attributes.target'/
-- [@Interface@]: /HTMLFormElement/
--
-- /* Indicates that the rules are more complicated./
form :: [Attribute] -> [Html] -> Html
form = ParentNode "<form" "</form>"
{-# INLINE form #-}


-- | Generates an HTML @\<h1\>@ element with the given attributes and contents.
--
-- [@Description@]: Heading
-- [@Categories@]: /flow/; /heading/; /palpable/
-- [@Parents@]: /'Html.legend'/; /'Html.summary'/; /flow/
-- [@Children@]: /phrasing/
-- [@Attributes@]: /globals/
-- [@Interface@]: /HTMLHeadingElement/
h1 :: [Attribute] -> [Html] -> Html
h1 = ParentNode "<h1" "</h1>"
{-# INLINE h1 #-}


-- | Generates an HTML @\<h2\>@ element with the given attributes and contents.
--
-- [@Description@]: Heading
-- [@Categories@]: /flow/; /heading/; /palpable/
-- [@Parents@]: /'Html.legend'/; /'Html.summary'/; /flow/
-- [@Children@]: /phrasing/
-- [@Attributes@]: /globals/
-- [@Interface@]: /HTMLHeadingElement/
h2 :: [Attribute] -> [Html] -> Html
h2 = ParentNode "<h2" "</h2>"
{-# INLINE h2 #-}


-- | Generates an HTML @\<h3\>@ element with the given attributes and contents.
--
-- [@Description@]: Heading
-- [@Categories@]: /flow/; /heading/; /palpable/
-- [@Parents@]: /'Html.legend'/; /'Html.summary'/; /flow/
-- [@Children@]: /phrasing/
-- [@Attributes@]: /globals/
-- [@Interface@]: /HTMLHeadingElement/
h3 :: [Attribute] -> [Html] -> Html
h3 = ParentNode "<h3" "</h3>"
{-# INLINE h3 #-}


-- | Generates an HTML @\<h4\>@ element with the given attributes and contents.
--
-- [@Description@]: Heading
-- [@Categories@]: /flow/; /heading/; /palpable/
-- [@Parents@]: /'Html.legend'/; /'Html.summary'/; /flow/
-- [@Children@]: /phrasing/
-- [@Attributes@]: /globals/
-- [@Interface@]: /HTMLHeadingElement/
h4 :: [Attribute] -> [Html] -> Html
h4 = ParentNode "<h4" "</h4>"
{-# INLINE h4 #-}


-- | Generates an HTML @\<h5\>@ element with the given attributes and contents.
--
-- [@Description@]: Heading
-- [@Categories@]: /flow/; /heading/; /palpable/
-- [@Parents@]: /'Html.legend'/; /'Html.summary'/; /flow/
-- [@Children@]: /phrasing/
-- [@Attributes@]: /globals/
-- [@Interface@]: /HTMLHeadingElement/
h5 :: [Attribute] -> [Html] -> Html
h5 = ParentNode "<h5" "</h5>"
{-# INLINE h5 #-}


-- | Generates an HTML @\<h6\>@ element with the given attributes and contents.
--
-- [@Description@]: Heading
-- [@Categories@]: /flow/; /heading/; /palpable/
-- [@Parents@]: /'Html.legend'/; /'Html.summary'/; /flow/
-- [@Children@]: /phrasing/
-- [@Attributes@]: /globals/
-- [@Interface@]: /HTMLHeadingElement/
h6 :: [Attribute] -> [Html] -> Html
h6 = ParentNode "<h6" "</h6>"
{-# INLINE h6 #-}


-- | Generates an HTML @\<head\>@ element with the given attributes and contents.
--
-- [@Description@]: Container for document metadata
-- [@Categories@]: none
-- [@Parents@]: /'Html.html'/
-- [@Children@]: /metadata content/*
-- [@Attributes@]: /globals/
-- [@Interface@]: /HTMLHeadElement/
--
-- /* Indicates that the rules are more complicated./
head :: [Attribute] -> [Html] -> Html
head = ParentNode "<head" "</head>"
{-# INLINE head #-}


-- | Generates an HTML @\<header\>@ element with the given attributes and contents.
--
-- [@Description@]: Introductory or navigational aids for a page or section
-- [@Categories@]: /flow/; /palpable/
-- [@Parents@]: /flow/
-- [@Children@]: /flow/*
-- [@Attributes@]: /globals/
-- [@Interface@]: /HTMLElement/
--
-- /* Indicates that the rules are more complicated./
header :: [Attribute] -> [Html] -> Html
header = ParentNode "<header" "</header>"
{-# INLINE header #-}


-- | Generates an HTML @\<hgroup\>@ element with the given attributes and contents.
--
-- [@Description@]: Heading container
-- [@Categories@]: /flow/; /palpable/
-- [@Parents@]: /'Html.legend'/; /'Html.summary'/; /flow/
-- [@Children@]: /'Html.h1'/; /'Html.h2'/; /'Html.h3'/; /'Html.h4'/; /'Html.h5'/; /'Html.h6'/; /script-supporting elements/
-- [@Attributes@]: /globals/
-- [@Interface@]: /HTMLElement/
hgroup :: [Attribute] -> [Html] -> Html
hgroup = ParentNode "<hgroup" "</hgroup>"
{-# INLINE hgroup #-}


-- | Generates an HTML @\<hr\>@ element with the given attributes.
--
-- [@Description@]: Thematic break
-- [@Categories@]: /flow/
-- [@Parents@]: /flow/
-- [@Children@]: empty
-- [@Attributes@]: /globals/
-- [@Interface@]: /HTMLHRElement/
hr :: [Attribute] -> Html
hr = LeafNode "<hr"
{-# INLINE hr #-}


-- | Generates an HTML @\<html\>@ element with the given attributes and contents.
--
-- [@Description@]: Root element
-- [@Categories@]: none
-- [@Parents@]: none*
-- [@Children@]: /'Html.head'/*; /'Html.body'/*
-- [@Attributes@]: /globals/
-- [@Interface@]: /HTMLHtmlElement/
--
-- /* Indicates that the rules are more complicated./
html :: [Attribute] -> [Html] -> Html
html = ParentNode "<html" "</html>"
{-# INLINE html #-}


-- | Generates an HTML @\<i\>@ element with the given attributes and contents.
--
-- [@Description@]: Alternate voice
-- [@Categories@]: /flow/; /phrasing/; /palpable/
-- [@Parents@]: /phrasing/
-- [@Children@]: /phrasing/
-- [@Attributes@]: /globals/
-- [@Interface@]: /HTMLElement/
i :: [Attribute] -> [Html] -> Html
i = ParentNode "<i" "</i>"
{-# INLINE i #-}


-- | Generates an HTML @\<iframe\>@ element with the given attributes and contents.
--
-- [@Description@]: Child navigable
-- [@Categories@]: /flow/; /phrasing/; /embedded/; /interactive/; /palpable/
-- [@Parents@]: /phrasing/
-- [@Children@]: empty
-- [@Attributes@]: /globals/; /'Html.Attributes.src'/; /'Html.Attributes.srcdoc'/; /'Html.Attributes.name'/; /'Html.Attributes.sandbox'/; /'Html.Attributes.allow'/; /'Html.Attributes.allowfullscreen'/; /'Html.Attributes.width'/; /'Html.Attributes.height'/; /'Html.Attributes.referrerpolicy'/; /'Html.Attributes.loading'/
-- [@Interface@]: /HTMLIFrameElement/
iframe :: [Attribute] -> [Html] -> Html
iframe = ParentNode "<iframe" "</iframe>"
{-# INLINE iframe #-}


-- | Generates an HTML @\<img\>@ element with the given attributes.
--
-- [@Description@]: Image
-- [@Categories@]: /flow/; /phrasing/; /embedded/; /interactive/*; /form-associated/; /palpable/
-- [@Parents@]: /phrasing/; /'Html.picture'/
-- [@Children@]: empty
-- [@Attributes@]: /globals/; /'Html.Attributes.alt'/; /'Html.Attributes.src'/; /'Html.Attributes.srcset'/; /'Html.Attributes.sizes'/; /'Html.Attributes.crossorigin'/; /'Html.Attributes.usemap'/; /'Html.Attributes.ismap'/; /'Html.Attributes.width'/; /'Html.Attributes.height'/; /'Html.Attributes.referrerpolicy'/; /'Html.Attributes.decoding'/; /'Html.Attributes.loading'/; /'Html.Attributes.fetchpriority'/
-- [@Interface@]: /HTMLImageElement/
--
-- /* Indicates that the rules are more complicated./
img :: [Attribute] -> Html
img = LeafNode "<img"
{-# INLINE img #-}


-- | Generates an HTML @\<input\>@ element with the given attributes.
--
-- [@Description@]: Form control
-- [@Categories@]: /flow/; /phrasing/; /interactive/*; /listed/; /labelable/; /submittable/; /resettable/; /form-associated/; /palpable/*
-- [@Parents@]: /phrasing/
-- [@Children@]: empty
-- [@Attributes@]: /globals/; /'Html.Attributes.accept'/; /'Html.Attributes.alpha'/; /'Html.Attributes.alt'/; /'Html.Attributes.autocomplete'/; /'Html.Attributes.checked'/; /'Html.Attributes.colorspace'/; /'Html.Attributes.dirname'/; /'Html.Attributes.disabled'/; /'Html.Attributes.form'/; /'Html.Attributes.formaction'/; /'Html.Attributes.formenctype'/; /'Html.Attributes.formmethod'/; /'Html.Attributes.formnovalidate'/; /'Html.Attributes.formtarget'/; /'Html.Attributes.height'/; /'Html.Attributes.list'/; /'Html.Attributes.max'/; /'Html.Attributes.maxlength'/; /'Html.Attributes.min'/; /'Html.Attributes.minlength'/; /'Html.Attributes.multiple'/; /'Html.Attributes.name'/; /'Html.Attributes.pattern'/; /'Html.Attributes.placeholder'/; /'Html.Attributes.popovertarget'/; /'Html.Attributes.popovertargetaction'/; /'Html.Attributes.readonly'/; /'Html.Attributes.required'/; /'Html.Attributes.size'/; /'Html.Attributes.src'/; /'Html.Attributes.step'/; /'Html.Attributes.type_'/; /'Html.Attributes.value'/; /'Html.Attributes.width'/
-- [@Interface@]: /HTMLInputElement/
--
-- /* Indicates that the rules are more complicated./
input :: [Attribute] -> Html
input = LeafNode "<input"
{-# INLINE input #-}


-- | Generates an HTML @\<ins\>@ element with the given attributes and contents.
--
-- [@Description@]: An addition to the document
-- [@Categories@]: /flow/; /phrasing/*; /palpable/
-- [@Parents@]: /phrasing/
-- [@Children@]: /transparent/
-- [@Attributes@]: /globals/; /'Html.Attributes.cite'/; /'Html.Attributes.datetime'/
-- [@Interface@]: /HTMLModElement/
--
-- /* Indicates that the rules are more complicated./
ins :: [Attribute] -> [Html] -> Html
ins = ParentNode "<ins" "</ins>"
{-# INLINE ins #-}


-- | Generates an HTML @\<kbd\>@ element with the given attributes and contents.
--
-- [@Description@]: User input
-- [@Categories@]: /flow/; /phrasing/; /palpable/
-- [@Parents@]: /phrasing/
-- [@Children@]: /phrasing/
-- [@Attributes@]: /globals/
-- [@Interface@]: /HTMLElement/
kbd :: [Attribute] -> [Html] -> Html
kbd = ParentNode "<kbd" "</kbd>"
{-# INLINE kbd #-}


-- | Generates an HTML @\<label\>@ element with the given attributes and contents.
--
-- [@Description@]: Caption for a form control
-- [@Categories@]: /flow/; /phrasing/; /interactive/; /palpable/
-- [@Parents@]: /phrasing/
-- [@Children@]: /phrasing/*
-- [@Attributes@]: /globals/; /'Html.Attributes.for'/
-- [@Interface@]: /HTMLLabelElement/
--
-- /* Indicates that the rules are more complicated./
label :: [Attribute] -> [Html] -> Html
label = ParentNode "<label" "</label>"
{-# INLINE label #-}


-- | Generates an HTML @\<legend\>@ element with the given attributes and contents.
--
-- [@Description@]: Caption for /'Html.fieldset'/
-- [@Categories@]: none
-- [@Parents@]: /'Html.fieldset'/
-- [@Children@]: /phrasing/; /heading content/
-- [@Attributes@]: /globals/
-- [@Interface@]: /HTMLLegendElement/
legend :: [Attribute] -> [Html] -> Html
legend = ParentNode "<legend" "</legend>"
{-# INLINE legend #-}


-- | Generates an HTML @\<li\>@ element with the given attributes and contents.
--
-- [@Description@]: List item
-- [@Categories@]: none
-- [@Parents@]: /'Html.ol'/; /'Html.ul'/; /'Html.menu'/*
-- [@Children@]: /flow/
-- [@Attributes@]: /globals/; /'Html.Attributes.value'/*
-- [@Interface@]: /HTMLLIElement/
--
-- /* Indicates that the rules are more complicated./
li :: [Attribute] -> [Html] -> Html
li = ParentNode "<li" "</li>"
{-# INLINE li #-}


-- | Generates an HTML @\<link\>@ element with the given attributes.
--
-- [@Description@]: Link metadata
-- [@Categories@]: /metadata/; /flow/*; /phrasing/*
-- [@Parents@]: /'Html.head'/; /'Html.noscript'/*; /phrasing/*
-- [@Children@]: empty
-- [@Attributes@]: /globals/; /'Html.Attributes.href'/; /'Html.Attributes.crossorigin'/; /'Html.Attributes.rel'/; /'Html.Attributes.as'/; /'Html.Attributes.media'/; /'Html.Attributes.hreflang'/; /'Html.Attributes.type_'/; /'Html.Attributes.sizes'/; /'Html.Attributes.imagesrcset'/; /'Html.Attributes.imagesizes'/; /'Html.Attributes.referrerpolicy'/; /'Html.Attributes.integrity'/; /'Html.Attributes.blocking'/; /'Html.Attributes.color'/; /'Html.Attributes.disabled'/; /'Html.Attributes.fetchpriority'/
-- [@Interface@]: /HTMLLinkElement/
--
-- /* Indicates that the rules are more complicated./
link :: [Attribute] -> Html
link = LeafNode "<link"
{-# INLINE link #-}


-- | Generates an HTML @\<main\>@ element with the given attributes and contents.
--
-- [@Description@]: Container for the dominant contents of the document
-- [@Categories@]: /flow/; /palpable/
-- [@Parents@]: /flow/*
-- [@Children@]: /flow/
-- [@Attributes@]: /globals/
-- [@Interface@]: /HTMLElement/
--
-- /* Indicates that the rules are more complicated./
main :: [Attribute] -> [Html] -> Html
main = ParentNode "<main" "</main>"
{-# INLINE main #-}


-- | Generates an HTML @\<map\>@ element with the given attributes and contents.
--
-- [@Description@]: Image map
-- [@Categories@]: /flow/; /phrasing/*; /palpable/
-- [@Parents@]: /phrasing/
-- [@Children@]: /transparent/; /'Html.area'/*
-- [@Attributes@]: /globals/; /'Html.Attributes.name'/
-- [@Interface@]: /HTMLMapElement/
--
-- /* Indicates that the rules are more complicated./
map :: [Attribute] -> [Html] -> Html
map = ParentNode "<map" "</map>"
{-# INLINE map #-}


-- | Generates an HTML @\<mark\>@ element with the given attributes and contents.
--
-- [@Description@]: Highlight
-- [@Categories@]: /flow/; /phrasing/; /palpable/
-- [@Parents@]: /phrasing/
-- [@Children@]: /phrasing/
-- [@Attributes@]: /globals/
-- [@Interface@]: /HTMLElement/
mark :: [Attribute] -> [Html] -> Html
mark = ParentNode "<mark" "</mark>"
{-# INLINE mark #-}


-- | Generates an HTML @\<menu\>@ element with the given attributes and contents.
--
-- [@Description@]: Menu of commands
-- [@Categories@]: /flow/; /palpable/*
-- [@Parents@]: /flow/
-- [@Children@]: /'Html.li'/; /script-supporting elements/
-- [@Attributes@]: /globals/
-- [@Interface@]: /HTMLMenuElement/
--
-- /* Indicates that the rules are more complicated./
menu :: [Attribute] -> [Html] -> Html
menu = ParentNode "<menu" "</menu>"
{-# INLINE menu #-}


-- | Generates an HTML @\<meta\>@ element with the given attributes.
--
-- [@Description@]: Text metadata
-- [@Categories@]: /metadata/; /flow/*; /phrasing/*
-- [@Parents@]: /'Html.head'/; /'Html.noscript'/*; /phrasing/*
-- [@Children@]: empty
-- [@Attributes@]: /globals/; /'Html.Attributes.name'/; /'Html.Attributes.httpEquiv'/; /'Html.Attributes.content'/; /'Html.Attributes.charset'/; /'Html.Attributes.media'/
-- [@Interface@]: /HTMLMetaElement/
--
-- /* Indicates that the rules are more complicated./
meta :: [Attribute] -> Html
meta = LeafNode "<meta"
{-# INLINE meta #-}


-- | Generates an HTML @\<meter\>@ element with the given attributes and contents.
--
-- [@Description@]: Gauge
-- [@Categories@]: /flow/; /phrasing/; /labelable/; /palpable/
-- [@Parents@]: /phrasing/
-- [@Children@]: /phrasing/*
-- [@Attributes@]: /globals/; /'Html.Attributes.value'/; /'Html.Attributes.min'/; /'Html.Attributes.max'/; /'Html.Attributes.low'/; /'Html.Attributes.high'/; /'Html.Attributes.optimum'/
-- [@Interface@]: /HTMLMeterElement/
--
-- /* Indicates that the rules are more complicated./
meter :: [Attribute] -> [Html] -> Html
meter = ParentNode "<meter" "</meter>"
{-# INLINE meter #-}


-- | Generates an HTML @\<nav\>@ element with the given attributes and contents.
--
-- [@Description@]: Section with navigational links
-- [@Categories@]: /flow/; /sectioning/; /palpable/
-- [@Parents@]: /flow/
-- [@Children@]: /flow/
-- [@Attributes@]: /globals/
-- [@Interface@]: /HTMLElement/
nav :: [Attribute] -> [Html] -> Html
nav = ParentNode "<nav" "</nav>"
{-# INLINE nav #-}


-- | Generates an HTML @\<noscript\>@ element with the given attributes and contents.
--
-- [@Description@]: Fallback content for script
-- [@Categories@]: /metadata/; /flow/; /phrasing/
-- [@Parents@]: /'Html.head'/*; /phrasing/*
-- [@Children@]: varies*
-- [@Attributes@]: /globals/
-- [@Interface@]: /HTMLElement/
--
-- /* Indicates that the rules are more complicated./
noscript :: [Attribute] -> [Html] -> Html
noscript = ParentNode "<noscript" "</noscript>"
{-# INLINE noscript #-}


-- | Generates an HTML @\<object\>@ element with the given attributes and contents.
--
-- [@Description@]: Image, child navigable, or plugin
-- [@Categories@]: /flow/; /phrasing/; /embedded/; /interactive/*; /listed/; /form-associated/; /palpable/
-- [@Parents@]: /phrasing/
-- [@Children@]: /transparent/
-- [@Attributes@]: /globals/; /'Html.Attributes.data_'/; /'Html.Attributes.type_'/; /'Html.Attributes.name'/; /'Html.Attributes.form'/; /'Html.Attributes.width'/; /'Html.Attributes.height'/
-- [@Interface@]: /HTMLObjectElement/
--
-- /* Indicates that the rules are more complicated./
object :: [Attribute] -> [Html] -> Html
object = ParentNode "<object" "</object>"
{-# INLINE object #-}


-- | Generates an HTML @\<ol\>@ element with the given attributes and contents.
--
-- [@Description@]: Ordered list
-- [@Categories@]: /flow/; /palpable/*
-- [@Parents@]: /flow/
-- [@Children@]: /'Html.li'/; /script-supporting elements/
-- [@Attributes@]: /globals/; /'Html.Attributes.reversed'/; /'Html.Attributes.start'/; /'Html.Attributes.type_'/
-- [@Interface@]: /HTMLOListElement/
--
-- /* Indicates that the rules are more complicated./
ol :: [Attribute] -> [Html] -> Html
ol = ParentNode "<ol" "</ol>"
{-# INLINE ol #-}


-- | Generates an HTML @\<optgroup\>@ element with the given attributes and contents.
--
-- [@Description@]: Group of options in a list box
-- [@Categories@]: none
-- [@Parents@]: /'Html.select'/
-- [@Children@]: /'Html.option'/; /script-supporting elements/
-- [@Attributes@]: /globals/; /'Html.Attributes.disabled'/; /'Html.Attributes.label'/
-- [@Interface@]: /HTMLOptGroupElement/
optgroup :: [Attribute] -> [Html] -> Html
optgroup = ParentNode "<optgroup" "</optgroup>"
{-# INLINE optgroup #-}


-- | Generates an HTML @\<option\>@ element with the given attributes and contents.
--
-- [@Description@]: Option in a list box or combo box control
-- [@Categories@]: none
-- [@Parents@]: /'Html.select'/; /'Html.datalist'/; /'Html.optgroup'/
-- [@Children@]: /text/*
-- [@Attributes@]: /globals/; /'Html.Attributes.disabled'/; /'Html.Attributes.label'/; /'Html.Attributes.selected'/; /'Html.Attributes.value'/
-- [@Interface@]: /HTMLOptionElement/
--
-- /* Indicates that the rules are more complicated./
option :: [Attribute] -> [Html] -> Html
option = ParentNode "<option" "</option>"
{-# INLINE option #-}


-- | Generates an HTML @\<output\>@ element with the given attributes and contents.
--
-- [@Description@]: Calculated output value
-- [@Categories@]: /flow/; /phrasing/; /listed/; /labelable/; /resettable/; /form-associated/; /palpable/
-- [@Parents@]: /phrasing/
-- [@Children@]: /phrasing/
-- [@Attributes@]: /globals/; /'Html.Attributes.for'/; /'Html.Attributes.form'/; /'Html.Attributes.name'/
-- [@Interface@]: /HTMLOutputElement/
output :: [Attribute] -> [Html] -> Html
output = ParentNode "<output" "</output>"
{-# INLINE output #-}


-- | Generates an HTML @\<p\>@ element with the given attributes and contents.
--
-- [@Description@]: Paragraph
-- [@Categories@]: /flow/; /palpable/
-- [@Parents@]: /flow/
-- [@Children@]: /phrasing/
-- [@Attributes@]: /globals/
-- [@Interface@]: /HTMLParagraphElement/
p :: [Attribute] -> [Html] -> Html
p = ParentNode "<p" "</p>"
{-# INLINE p #-}


-- | Generates an HTML @\<picture\>@ element with the given attributes and contents.
--
-- [@Description@]: Image
-- [@Categories@]: /flow/; /phrasing/; /embedded/; /palpable/
-- [@Parents@]: /phrasing/
-- [@Children@]: /'Html.source'/*; one /'Html.img'/; /script-supporting elements/
-- [@Attributes@]: /globals/
-- [@Interface@]: /HTMLPictureElement/
--
-- /* Indicates that the rules are more complicated./
picture :: [Attribute] -> [Html] -> Html
picture = ParentNode "<picture" "</picture>"
{-# INLINE picture #-}


-- | Generates an HTML @\<pre\>@ element with the given attributes and contents.
--
-- [@Description@]: Block of preformatted text
-- [@Categories@]: /flow/; /palpable/
-- [@Parents@]: /flow/
-- [@Children@]: /phrasing/
-- [@Attributes@]: /globals/
-- [@Interface@]: /HTMLPreElement/
pre :: [Attribute] -> [Html] -> Html
pre = ParentNode "<pre" "</pre>"
{-# INLINE pre #-}


-- | Generates an HTML @\<progress\>@ element with the given attributes and contents.
--
-- [@Description@]: Progress bar
-- [@Categories@]: /flow/; /phrasing/; /labelable/; /palpable/
-- [@Parents@]: /phrasing/
-- [@Children@]: /phrasing/*
-- [@Attributes@]: /globals/; /'Html.Attributes.value'/; /'Html.Attributes.max'/
-- [@Interface@]: /HTMLProgressElement/
--
-- /* Indicates that the rules are more complicated./
progress :: [Attribute] -> [Html] -> Html
progress = ParentNode "<progress" "</progress>"
{-# INLINE progress #-}


-- | Generates an HTML @\<q\>@ element with the given attributes and contents.
--
-- [@Description@]: Quotation
-- [@Categories@]: /flow/; /phrasing/; /palpable/
-- [@Parents@]: /phrasing/
-- [@Children@]: /phrasing/
-- [@Attributes@]: /globals/; /'Html.Attributes.cite'/
-- [@Interface@]: /HTMLQuoteElement/
q :: [Attribute] -> [Html] -> Html
q = ParentNode "<q" "</q>"
{-# INLINE q #-}


-- | Generates an HTML @\<rp\>@ element with the given attributes and contents.
--
-- [@Description@]: Parenthesis for ruby annotation text
-- [@Categories@]: none
-- [@Parents@]: /'Html.ruby'/
-- [@Children@]: /text/
-- [@Attributes@]: /globals/
-- [@Interface@]: /HTMLElement/
rp :: [Attribute] -> [Html] -> Html
rp = ParentNode "<rp" "</rp>"
{-# INLINE rp #-}


-- | Generates an HTML @\<rt\>@ element with the given attributes and contents.
--
-- [@Description@]: Ruby annotation text
-- [@Categories@]: none
-- [@Parents@]: /'Html.ruby'/
-- [@Children@]: /phrasing/
-- [@Attributes@]: /globals/
-- [@Interface@]: /HTMLElement/
rt :: [Attribute] -> [Html] -> Html
rt = ParentNode "<rt" "</rt>"
{-# INLINE rt #-}


-- | Generates an HTML @\<ruby\>@ element with the given attributes and contents.
--
-- [@Description@]: Ruby annotation(s)
-- [@Categories@]: /flow/; /phrasing/; /palpable/
-- [@Parents@]: /phrasing/
-- [@Children@]: /phrasing/*; /'Html.rt'/; /'Html.rp'/*
-- [@Attributes@]: /globals/
-- [@Interface@]: /HTMLElement/
--
-- /* Indicates that the rules are more complicated./
ruby :: [Attribute] -> [Html] -> Html
ruby = ParentNode "<ruby" "</ruby>"
{-# INLINE ruby #-}


-- | Generates an HTML @\<s\>@ element with the given attributes and contents.
--
-- [@Description@]: Inaccurate text
-- [@Categories@]: /flow/; /phrasing/; /palpable/
-- [@Parents@]: /phrasing/
-- [@Children@]: /phrasing/
-- [@Attributes@]: /globals/
-- [@Interface@]: /HTMLElement/
s :: [Attribute] -> [Html] -> Html
s = ParentNode "<s" "</s>"
{-# INLINE s #-}


-- | Generates an HTML @\<samp\>@ element with the given attributes and contents.
--
-- [@Description@]: Computer output
-- [@Categories@]: /flow/; /phrasing/; /palpable/
-- [@Parents@]: /phrasing/
-- [@Children@]: /phrasing/
-- [@Attributes@]: /globals/
-- [@Interface@]: /HTMLElement/
samp :: [Attribute] -> [Html] -> Html
samp = ParentNode "<samp" "</samp>"
{-# INLINE samp #-}


-- | Generates an HTML @\<script\>@ element with the given attributes and contents.
--
-- [@Description@]: Embedded script
-- [@Categories@]: /metadata/; /flow/; /phrasing/; /script-supporting elements/
-- [@Parents@]: /'Html.head'/; /phrasing/; /script-supporting elements/
-- [@Children@]: script, data, or script documentation*
-- [@Attributes@]: /globals/; /'Html.Attributes.src'/; /'Html.Attributes.type_'/; /'Html.Attributes.nomodule'/; /'Html.Attributes.async'/; /'Html.Attributes.defer'/; /'Html.Attributes.crossorigin'/; /'Html.Attributes.integrity'/; /'Html.Attributes.referrerpolicy'/; /'Html.Attributes.blocking'/; /'Html.Attributes.fetchpriority'/
-- [@Interface@]: /HTMLScriptElement/
--
-- /* Indicates that the rules are more complicated./
script :: [Attribute] -> [Html] -> Html
script = ParentNode "<script" "</script>"
{-# INLINE script #-}


-- | Generates an HTML @\<search\>@ element with the given attributes and contents.
--
-- [@Description@]: Container for search controls
-- [@Categories@]: /flow/; /palpable/
-- [@Parents@]: /flow/
-- [@Children@]: /flow/
-- [@Attributes@]: /globals/
-- [@Interface@]: /HTMLElement/
search :: [Attribute] -> [Html] -> Html
search = ParentNode "<search" "</search>"
{-# INLINE search #-}


-- | Generates an HTML @\<section\>@ element with the given attributes and contents.
--
-- [@Description@]: Generic document or application section
-- [@Categories@]: /flow/; /sectioning/; /palpable/
-- [@Parents@]: /flow/
-- [@Children@]: /flow/
-- [@Attributes@]: /globals/
-- [@Interface@]: /HTMLElement/
section :: [Attribute] -> [Html] -> Html
section = ParentNode "<section" "</section>"
{-# INLINE section #-}


-- | Generates an HTML @\<select\>@ element with the given attributes and contents.
--
-- [@Description@]: List box control
-- [@Categories@]: /flow/; /phrasing/; /interactive/; /listed/; /labelable/; /submittable/; /resettable/; /form-associated/; /palpable/
-- [@Parents@]: /phrasing/
-- [@Children@]: /'Html.option'/; /'Html.optgroup'/; /script-supporting elements/
-- [@Attributes@]: /globals/; /'Html.Attributes.autocomplete'/; /'Html.Attributes.disabled'/; /'Html.Attributes.form'/; /'Html.Attributes.multiple'/; /'Html.Attributes.name'/; /'Html.Attributes.required'/; /'Html.Attributes.size'/
-- [@Interface@]: /HTMLSelectElement/
select :: [Attribute] -> [Html] -> Html
select = ParentNode "<select" "</select>"
{-# INLINE select #-}


-- | Generates an HTML @\<slot\>@ element with the given attributes and contents.
--
-- [@Description@]: Shadow tree slot
-- [@Categories@]: /flow/; /phrasing/
-- [@Parents@]: /phrasing/
-- [@Children@]: /transparent/
-- [@Attributes@]: /globals/; /'Html.Attributes.name'/
-- [@Interface@]: /HTMLSlotElement/
slot :: [Attribute] -> [Html] -> Html
slot = ParentNode "<slot" "</slot>"
{-# INLINE slot #-}


-- | Generates an HTML @\<small\>@ element with the given attributes and contents.
--
-- [@Description@]: Side comment
-- [@Categories@]: /flow/; /phrasing/; /palpable/
-- [@Parents@]: /phrasing/
-- [@Children@]: /phrasing/
-- [@Attributes@]: /globals/
-- [@Interface@]: /HTMLElement/
small :: [Attribute] -> [Html] -> Html
small = ParentNode "<small" "</small>"
{-# INLINE small #-}


-- | Generates an HTML @\<source\>@ element with the given attributes.
--
-- [@Description@]: Image source for /'Html.img'/ or media source for /'Html.video'/ or /'Html.audio'/
-- [@Categories@]: none
-- [@Parents@]: /'Html.picture'/; /'Html.video'/; /'Html.audio'/
-- [@Children@]: empty
-- [@Attributes@]: /globals/; /'Html.Attributes.type_'/; /'Html.Attributes.media'/; /'Html.Attributes.src'/; /'Html.Attributes.srcset'/; /'Html.Attributes.sizes'/; /'Html.Attributes.width'/; /'Html.Attributes.height'/
-- [@Interface@]: /HTMLSourceElement/
source :: [Attribute] -> Html
source = LeafNode "<source"
{-# INLINE source #-}


-- | Generates an HTML @\<span\>@ element with the given attributes and contents.
--
-- [@Description@]: Generic phrasing container
-- [@Categories@]: /flow/; /phrasing/; /palpable/
-- [@Parents@]: /phrasing/
-- [@Children@]: /phrasing/
-- [@Attributes@]: /globals/
-- [@Interface@]: /HTMLSpanElement/
span :: [Attribute] -> [Html] -> Html
span = ParentNode "<span" "</span>"
{-# INLINE span #-}


-- | Generates an HTML @\<strong\>@ element with the given attributes and contents.
--
-- [@Description@]: Importance
-- [@Categories@]: /flow/; /phrasing/; /palpable/
-- [@Parents@]: /phrasing/
-- [@Children@]: /phrasing/
-- [@Attributes@]: /globals/
-- [@Interface@]: /HTMLElement/
strong :: [Attribute] -> [Html] -> Html
strong = ParentNode "<strong" "</strong>"
{-# INLINE strong #-}


-- | Generates an HTML @\<style\>@ element with the given attributes and contents.
--
-- [@Description@]: Embedded styling information
-- [@Categories@]: /metadata/
-- [@Parents@]: /'Html.head'/; /'Html.noscript'/*
-- [@Children@]: /text/*
-- [@Attributes@]: /globals/; /'Html.Attributes.media'/; /'Html.Attributes.blocking'/
-- [@Interface@]: /HTMLStyleElement/
--
-- /* Indicates that the rules are more complicated./
style :: [Attribute] -> [Html] -> Html
style = ParentNode "<style" "</style>"
{-# INLINE style #-}


-- | Generates an HTML @\<sub\>@ element with the given attributes and contents.
--
-- [@Description@]: Subscript
-- [@Categories@]: /flow/; /phrasing/; /palpable/
-- [@Parents@]: /phrasing/
-- [@Children@]: /phrasing/
-- [@Attributes@]: /globals/
-- [@Interface@]: /HTMLElement/
sub :: [Attribute] -> [Html] -> Html
sub = ParentNode "<sub" "</sub>"
{-# INLINE sub #-}


-- | Generates an HTML @\<summary\>@ element with the given attributes and contents.
--
-- [@Description@]: Caption for /'Html.details'/
-- [@Categories@]: none
-- [@Parents@]: /'Html.details'/
-- [@Children@]: /phrasing/; /heading content/
-- [@Attributes@]: /globals/
-- [@Interface@]: /HTMLElement/
summary :: [Attribute] -> [Html] -> Html
summary = ParentNode "<summary" "</summary>"
{-# INLINE summary #-}


-- | Generates an HTML @\<sup\>@ element with the given attributes and contents.
--
-- [@Description@]: Superscript
-- [@Categories@]: /flow/; /phrasing/; /palpable/
-- [@Parents@]: /phrasing/
-- [@Children@]: /phrasing/
-- [@Attributes@]: /globals/
-- [@Interface@]: /HTMLElement/
sup :: [Attribute] -> [Html] -> Html
sup = ParentNode "<sup" "</sup>"
{-# INLINE sup #-}


-- | Generates an HTML @\<table\>@ element with the given attributes and contents.
--
-- [@Description@]: Table
-- [@Categories@]: /flow/; /palpable/
-- [@Parents@]: /flow/
-- [@Children@]: /'Html.caption'/*; /'Html.colgroup'/*; /'Html.thead'/*; /'Html.tbody'/*; /'Html.tfoot'/*; /'Html.tr'/*; /script-supporting elements/
-- [@Attributes@]: /globals/
-- [@Interface@]: /HTMLTableElement/
--
-- /* Indicates that the rules are more complicated./
table :: [Attribute] -> [Html] -> Html
table = ParentNode "<table" "</table>"
{-# INLINE table #-}


-- | Generates an HTML @\<tbody\>@ element with the given attributes and contents.
--
-- [@Description@]: Group of rows in a table
-- [@Categories@]: none
-- [@Parents@]: /'Html.table'/
-- [@Children@]: /'Html.tr'/; /script-supporting elements/
-- [@Attributes@]: /globals/
-- [@Interface@]: /HTMLTableSectionElement/
tbody :: [Attribute] -> [Html] -> Html
tbody = ParentNode "<tbody" "</tbody>"
{-# INLINE tbody #-}


-- | Generates an HTML @\<td\>@ element with the given attributes and contents.
--
-- [@Description@]: Table cell
-- [@Categories@]: none
-- [@Parents@]: /'Html.tr'/
-- [@Children@]: /flow/
-- [@Attributes@]: /globals/; /'Html.Attributes.colspan'/; /'Html.Attributes.rowspan'/; /'Html.Attributes.headers'/
-- [@Interface@]: /HTMLTableCellElement/
td :: [Attribute] -> [Html] -> Html
td = ParentNode "<td" "</td>"
{-# INLINE td #-}


-- | Generates an HTML @\<template\>@ element with the given attributes and contents.
--
-- [@Description@]: Template
-- [@Categories@]: /metadata/; /flow/; /phrasing/; /script-supporting elements/
-- [@Parents@]: /metadata/; /phrasing/; /script-supporting elements/; /'Html.colgroup'/*
-- [@Children@]: none
-- [@Attributes@]: /globals/; /'Html.Attributes.shadowrootmode'/; /'Html.Attributes.shadowrootdelegatesfocus'/; /'Html.Attributes.shadowrootclonable'/; /'Html.Attributes.shadowrootserializable'/
-- [@Interface@]: /HTMLTemplateElement/
--
-- /* Indicates that the rules are more complicated./
template :: [Attribute] -> [Html] -> Html
template = ParentNode "<template" "</template>"
{-# INLINE template #-}


-- | Generates an HTML @\<textarea\>@ element with the given attributes and contents.
--
-- [@Description@]: Multiline text controls
-- [@Categories@]: /flow/; /phrasing/; /interactive/; /listed/; /labelable/; /submittable/; /resettable/; /form-associated/; /palpable/
-- [@Parents@]: /phrasing/
-- [@Children@]: /text/
-- [@Attributes@]: /globals/; /'Html.Attributes.autocomplete'/; /'Html.Attributes.cols'/; /'Html.Attributes.dirname'/; /'Html.Attributes.disabled'/; /'Html.Attributes.form'/; /'Html.Attributes.maxlength'/; /'Html.Attributes.minlength'/; /'Html.Attributes.name'/; /'Html.Attributes.placeholder'/; /'Html.Attributes.readonly'/; /'Html.Attributes.required'/; /'Html.Attributes.rows'/; /'Html.Attributes.wrap'/
-- [@Interface@]: /HTMLTextAreaElement/
textarea :: [Attribute] -> [Html] -> Html
textarea = ParentNode "<textarea" "</textarea>"
{-# INLINE textarea #-}


-- | Generates an HTML @\<tfoot\>@ element with the given attributes and contents.
--
-- [@Description@]: Group of footer rows in a table
-- [@Categories@]: none
-- [@Parents@]: /'Html.table'/
-- [@Children@]: /'Html.tr'/; /script-supporting elements/
-- [@Attributes@]: /globals/
-- [@Interface@]: /HTMLTableSectionElement/
tfoot :: [Attribute] -> [Html] -> Html
tfoot = ParentNode "<tfoot" "</tfoot>"
{-# INLINE tfoot #-}


-- | Generates an HTML @\<th\>@ element with the given attributes and contents.
--
-- [@Description@]: Table header cell
-- [@Categories@]: /interactive/*
-- [@Parents@]: /'Html.tr'/
-- [@Children@]: /flow/*
-- [@Attributes@]: /globals/; /'Html.Attributes.colspan'/; /'Html.Attributes.rowspan'/; /'Html.Attributes.headers'/; /'Html.Attributes.scope'/; /'Html.Attributes.abbr'/
-- [@Interface@]: /HTMLTableCellElement/
--
-- /* Indicates that the rules are more complicated./
th :: [Attribute] -> [Html] -> Html
th = ParentNode "<th" "</th>"
{-# INLINE th #-}


-- | Generates an HTML @\<thead\>@ element with the given attributes and contents.
--
-- [@Description@]: Group of heading rows in a table
-- [@Categories@]: none
-- [@Parents@]: /'Html.table'/
-- [@Children@]: /'Html.tr'/; /script-supporting elements/
-- [@Attributes@]: /globals/
-- [@Interface@]: /HTMLTableSectionElement/
thead :: [Attribute] -> [Html] -> Html
thead = ParentNode "<thead" "</thead>"
{-# INLINE thead #-}


-- | Generates an HTML @\<time\>@ element with the given attributes and contents.
--
-- [@Description@]: Machine-readable equivalent of date- or time-related data
-- [@Categories@]: /flow/; /phrasing/; /palpable/
-- [@Parents@]: /phrasing/
-- [@Children@]: /phrasing/
-- [@Attributes@]: /globals/; /'Html.Attributes.datetime'/
-- [@Interface@]: /HTMLTimeElement/
time :: [Attribute] -> [Html] -> Html
time = ParentNode "<time" "</time>"
{-# INLINE time #-}


-- | Generates an HTML @\<title\>@ element with the given attributes and contents.
--
-- [@Description@]: Document title
-- [@Categories@]: /metadata/
-- [@Parents@]: /'Html.head'/
-- [@Children@]: /text/*
-- [@Attributes@]: /globals/
-- [@Interface@]: /HTMLTitleElement/
--
-- /* Indicates that the rules are more complicated./
title :: [Attribute] -> [Html] -> Html
title = ParentNode "<title" "</title>"
{-# INLINE title #-}


-- | Generates an HTML @\<tr\>@ element with the given attributes and contents.
--
-- [@Description@]: Table row
-- [@Categories@]: none
-- [@Parents@]: /'Html.table'/; /'Html.thead'/; /'Html.tbody'/; /'Html.tfoot'/
-- [@Children@]: /'Html.th'/*; /'Html.td'/; /script-supporting elements/
-- [@Attributes@]: /globals/
-- [@Interface@]: /HTMLTableRowElement/
--
-- /* Indicates that the rules are more complicated./
tr :: [Attribute] -> [Html] -> Html
tr = ParentNode "<tr" "</tr>"
{-# INLINE tr #-}


-- | Generates an HTML @\<track\>@ element with the given attributes.
--
-- [@Description@]: Timed text track
-- [@Categories@]: none
-- [@Parents@]: /'Html.audio'/; /'Html.video'/
-- [@Children@]: empty
-- [@Attributes@]: /globals/; /'Html.Attributes.default_'/; /'Html.Attributes.kind'/; /'Html.Attributes.label'/; /'Html.Attributes.src'/; /'Html.Attributes.srclang'/
-- [@Interface@]: /HTMLTrackElement/
track :: [Attribute] -> Html
track = LeafNode "<track"
{-# INLINE track #-}


-- | Generates an HTML @\<u\>@ element with the given attributes and contents.
--
-- [@Description@]: Unarticulated annotation
-- [@Categories@]: /flow/; /phrasing/; /palpable/
-- [@Parents@]: /phrasing/
-- [@Children@]: /phrasing/
-- [@Attributes@]: /globals/
-- [@Interface@]: /HTMLElement/
u :: [Attribute] -> [Html] -> Html
u = ParentNode "<u" "</u>"
{-# INLINE u #-}


-- | Generates an HTML @\<ul\>@ element with the given attributes and contents.
--
-- [@Description@]: List
-- [@Categories@]: /flow/; /palpable/*
-- [@Parents@]: /flow/
-- [@Children@]: /'Html.li'/; /script-supporting elements/
-- [@Attributes@]: /globals/
-- [@Interface@]: /HTMLUListElement/
--
-- /* Indicates that the rules are more complicated./
ul :: [Attribute] -> [Html] -> Html
ul = ParentNode "<ul" "</ul>"
{-# INLINE ul #-}


-- | Generates an HTML @\<var\>@ element with the given attributes and contents.
--
-- [@Description@]: Variable
-- [@Categories@]: /flow/; /phrasing/; /palpable/
-- [@Parents@]: /phrasing/
-- [@Children@]: /phrasing/
-- [@Attributes@]: /globals/
-- [@Interface@]: /HTMLElement/
var :: [Attribute] -> [Html] -> Html
var = ParentNode "<var" "</var>"
{-# INLINE var #-}


-- | Generates an HTML @\<video\>@ element with the given attributes and contents.
--
-- [@Description@]: Video player
-- [@Categories@]: /flow/; /phrasing/; /embedded/; /interactive/; /palpable/
-- [@Parents@]: /phrasing/
-- [@Children@]: /'Html.source'/*; /'Html.track'/*; /transparent/*
-- [@Attributes@]: /globals/; /'Html.Attributes.src'/; /'Html.Attributes.crossorigin'/; /'Html.Attributes.poster'/; /'Html.Attributes.preload'/; /'Html.Attributes.autoplay'/; /'Html.Attributes.playsinline'/; /'Html.Attributes.loop'/; /'Html.Attributes.muted'/; /'Html.Attributes.controls'/; /'Html.Attributes.width'/; /'Html.Attributes.height'/
-- [@Interface@]: /HTMLVideoElement/
--
-- /* Indicates that the rules are more complicated./
video :: [Attribute] -> [Html] -> Html
video = ParentNode "<video" "</video>"
{-# INLINE video #-}


-- | Generates an HTML @\<wbr\>@ element with the given attributes.
--
-- [@Description@]: Line breaking opportunity
-- [@Categories@]: /flow/; /phrasing/
-- [@Parents@]: /phrasing/
-- [@Children@]: empty
-- [@Attributes@]: /globals/
-- [@Interface@]: /HTMLElement/
wbr :: [Attribute] -> Html
wbr = LeafNode "<wbr"
{-# INLINE wbr #-}
