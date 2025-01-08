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
-- $hello
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
caption :: [Attribute] -> [Html] -> Html
caption = ParentNode "<caption" "</caption>"
{-# INLINE caption #-}


-- | Generates an HTML @\<cite\>@ element with the given attributes and contents.
cite :: [Attribute] -> [Html] -> Html
cite = ParentNode "<cite" "</cite>"
{-# INLINE cite #-}


-- | Generates an HTML @\<code\>@ element with the given attributes and contents.
code :: [Attribute] -> [Html] -> Html
code = ParentNode "<code" "</code>"
{-# INLINE code #-}


-- | Generates an HTML @\<col\>@ element with the given attributes.
col :: [Attribute] -> Html
col = LeafNode "<col"
{-# INLINE col #-}


-- | Generates an HTML @\<colgroup\>@ element with the given attributes and contents.
colgroup :: [Attribute] -> [Html] -> Html
colgroup = ParentNode "<colgroup" "</colgroup>"
{-# INLINE colgroup #-}


-- | Generates an HTML @\<data\>@ element with the given attributes and contents.
data_ :: [Attribute] -> [Html] -> Html
data_ = ParentNode "<data" "</data>"
{-# INLINE data_ #-}


-- | Generates an HTML @\<datalist\>@ element with the given attributes and contents.
datalist :: [Attribute] -> [Html] -> Html
datalist = ParentNode "<datalist" "</datalist>"
{-# INLINE datalist #-}


-- | Generates an HTML @\<dd\>@ element with the given attributes and contents.
dd :: [Attribute] -> [Html] -> Html
dd = ParentNode "<dd" "</dd>"
{-# INLINE dd #-}


-- | Generates an HTML @\<del\>@ element with the given attributes and contents.
del :: [Attribute] -> [Html] -> Html
del = ParentNode "<del" "</del>"
{-# INLINE del #-}


-- | Generates an HTML @\<details\>@ element with the given attributes and contents.
details :: [Attribute] -> [Html] -> Html
details = ParentNode "<details" "</details>"
{-# INLINE details #-}


-- | Generates an HTML @\<dfn\>@ element with the given attributes and contents.
dfn :: [Attribute] -> [Html] -> Html
dfn = ParentNode "<dfn" "</dfn>"
{-# INLINE dfn #-}


-- | Generates an HTML @\<dialog\>@ element with the given attributes and contents.
dialog :: [Attribute] -> [Html] -> Html
dialog = ParentNode "<dialog" "</dialog>"
{-# INLINE dialog #-}


-- | Generates an HTML @\<div\>@ element with the given attributes and contents.
div :: [Attribute] -> [Html] -> Html
div = ParentNode "<div" "</div>"
{-# INLINE div #-}


-- | Generates an HTML @\<dl\>@ element with the given attributes and contents.
dl :: [Attribute] -> [Html] -> Html
dl = ParentNode "<dl" "</dl>"
{-# INLINE dl #-}


-- | Generates an HTML @\<dt\>@ element with the given attributes and contents.
dt :: [Attribute] -> [Html] -> Html
dt = ParentNode "<dt" "</dt>"
{-# INLINE dt #-}


-- | Generates an HTML @\<em\>@ element with the given attributes and contents.
em :: [Attribute] -> [Html] -> Html
em = ParentNode "<em" "</em>"
{-# INLINE em #-}


-- | Generates an HTML @\<embed\>@ element with the given attributes.
embed :: [Attribute] -> Html
embed = LeafNode "<embed"
{-# INLINE embed #-}


-- | Generates an HTML @\<fieldset\>@ element with the given attributes and contents.
fieldset :: [Attribute] -> [Html] -> Html
fieldset = ParentNode "<fieldset" "</fieldset>"
{-# INLINE fieldset #-}


-- | Generates an HTML @\<figcaption\>@ element with the given attributes and contents.
figcaption :: [Attribute] -> [Html] -> Html
figcaption = ParentNode "<figcaption" "</figcaption>"
{-# INLINE figcaption #-}


-- | Generates an HTML @\<figure\>@ element with the given attributes and contents.
figure :: [Attribute] -> [Html] -> Html
figure = ParentNode "<figure" "</figure>"
{-# INLINE figure #-}


-- | Generates an HTML @\<footer\>@ element with the given attributes and contents.
footer :: [Attribute] -> [Html] -> Html
footer = ParentNode "<footer" "</footer>"
{-# INLINE footer #-}


-- | Generates an HTML @\<form\>@ element with the given attributes and contents.
form :: [Attribute] -> [Html] -> Html
form = ParentNode "<form" "</form>"
{-# INLINE form #-}


-- | Generates an HTML @\<h1\>@ element with the given attributes and contents.
h1 :: [Attribute] -> [Html] -> Html
h1 = ParentNode "<h1" "</h1>"
{-# INLINE h1 #-}


-- | Generates an HTML @\<h2\>@ element with the given attributes and contents.
h2 :: [Attribute] -> [Html] -> Html
h2 = ParentNode "<h2" "</h2>"
{-# INLINE h2 #-}


-- | Generates an HTML @\<h3\>@ element with the given attributes and contents.
h3 :: [Attribute] -> [Html] -> Html
h3 = ParentNode "<h3" "</h3>"
{-# INLINE h3 #-}


-- | Generates an HTML @\<h4\>@ element with the given attributes and contents.
h4 :: [Attribute] -> [Html] -> Html
h4 = ParentNode "<h4" "</h4>"
{-# INLINE h4 #-}


-- | Generates an HTML @\<h5\>@ element with the given attributes and contents.
h5 :: [Attribute] -> [Html] -> Html
h5 = ParentNode "<h5" "</h5>"
{-# INLINE h5 #-}


-- | Generates an HTML @\<h6\>@ element with the given attributes and contents.
h6 :: [Attribute] -> [Html] -> Html
h6 = ParentNode "<h6" "</h6>"
{-# INLINE h6 #-}


-- | Generates an HTML @\<head\>@ element with the given attributes and contents.
head :: [Attribute] -> [Html] -> Html
head = ParentNode "<head" "</head>"
{-# INLINE head #-}


-- | Generates an HTML @\<header\>@ element with the given attributes and contents.
header :: [Attribute] -> [Html] -> Html
header = ParentNode "<header" "</header>"
{-# INLINE header #-}


-- | Generates an HTML @\<hgroup\>@ element with the given attributes and contents.
hgroup :: [Attribute] -> [Html] -> Html
hgroup = ParentNode "<hgroup" "</hgroup>"
{-# INLINE hgroup #-}


-- | Generates an HTML @\<hr\>@ element with the given attributes.
hr :: [Attribute] -> Html
hr = LeafNode "<hr"
{-# INLINE hr #-}


-- | Generates an HTML @\<html\>@ element with the given attributes and contents.
html :: [Attribute] -> [Html] -> Html
html = ParentNode "<html" "</html>"
{-# INLINE html #-}


-- | Generates an HTML @\<i\>@ element with the given attributes and contents.
i :: [Attribute] -> [Html] -> Html
i = ParentNode "<i" "</i>"
{-# INLINE i #-}


-- | Generates an HTML @\<iframe\>@ element with the given attributes and contents.
iframe :: [Attribute] -> [Html] -> Html
iframe = ParentNode "<iframe" "</iframe>"
{-# INLINE iframe #-}


-- | Generates an HTML @\<img\>@ element with the given attributes.
img :: [Attribute] -> Html
img = LeafNode "<img"
{-# INLINE img #-}


-- | Generates an HTML @\<input\>@ element with the given attributes.
input :: [Attribute] -> Html
input = LeafNode "<input"
{-# INLINE input #-}


-- | Generates an HTML @\<ins\>@ element with the given attributes and contents.
ins :: [Attribute] -> [Html] -> Html
ins = ParentNode "<ins" "</ins>"
{-# INLINE ins #-}


-- | Generates an HTML @\<kbd\>@ element with the given attributes and contents.
kbd :: [Attribute] -> [Html] -> Html
kbd = ParentNode "<kbd" "</kbd>"
{-# INLINE kbd #-}


-- | Generates an HTML @\<label\>@ element with the given attributes and contents.
label :: [Attribute] -> [Html] -> Html
label = ParentNode "<label" "</label>"
{-# INLINE label #-}


-- | Generates an HTML @\<legend\>@ element with the given attributes and contents.
legend :: [Attribute] -> [Html] -> Html
legend = ParentNode "<legend" "</legend>"
{-# INLINE legend #-}


-- | Generates an HTML @\<li\>@ element with the given attributes and contents.
li :: [Attribute] -> [Html] -> Html
li = ParentNode "<li" "</li>"
{-# INLINE li #-}


-- | Generates an HTML @\<link\>@ element with the given attributes.
link :: [Attribute] -> Html
link = LeafNode "<link"
{-# INLINE link #-}


-- | Generates an HTML @\<main\>@ element with the given attributes and contents.
main :: [Attribute] -> [Html] -> Html
main = ParentNode "<main" "</main>"
{-# INLINE main #-}


-- | Generates an HTML @\<map\>@ element with the given attributes and contents.
map :: [Attribute] -> [Html] -> Html
map = ParentNode "<map" "</map>"
{-# INLINE map #-}


-- | Generates an HTML @\<mark\>@ element with the given attributes and contents.
mark :: [Attribute] -> [Html] -> Html
mark = ParentNode "<mark" "</mark>"
{-# INLINE mark #-}


-- | Generates an HTML @\<menu\>@ element with the given attributes and contents.
menu :: [Attribute] -> [Html] -> Html
menu = ParentNode "<menu" "</menu>"
{-# INLINE menu #-}


-- | Generates an HTML @\<meta\>@ element with the given attributes.
meta :: [Attribute] -> Html
meta = LeafNode "<meta"
{-# INLINE meta #-}


-- | Generates an HTML @\<meter\>@ element with the given attributes and contents.
meter :: [Attribute] -> [Html] -> Html
meter = ParentNode "<meter" "</meter>"
{-# INLINE meter #-}


-- | Generates an HTML @\<nav\>@ element with the given attributes and contents.
nav :: [Attribute] -> [Html] -> Html
nav = ParentNode "<nav" "</nav>"
{-# INLINE nav #-}


-- | Generates an HTML @\<noscript\>@ element with the given attributes and contents.
noscript :: [Attribute] -> [Html] -> Html
noscript = ParentNode "<noscript" "</noscript>"
{-# INLINE noscript #-}


-- | Generates an HTML @\<object\>@ element with the given attributes and contents.
object :: [Attribute] -> [Html] -> Html
object = ParentNode "<object" "</object>"
{-# INLINE object #-}


-- | Generates an HTML @\<ol\>@ element with the given attributes and contents.
ol :: [Attribute] -> [Html] -> Html
ol = ParentNode "<ol" "</ol>"
{-# INLINE ol #-}


-- | Generates an HTML @\<optgroup\>@ element with the given attributes and contents.
optgroup :: [Attribute] -> [Html] -> Html
optgroup = ParentNode "<optgroup" "</optgroup>"
{-# INLINE optgroup #-}


-- | Generates an HTML @\<option\>@ element with the given attributes and contents.
option :: [Attribute] -> [Html] -> Html
option = ParentNode "<option" "</option>"
{-# INLINE option #-}


-- | Generates an HTML @\<output\>@ element with the given attributes and contents.
output :: [Attribute] -> [Html] -> Html
output = ParentNode "<output" "</output>"
{-# INLINE output #-}


-- | Generates an HTML @\<p\>@ element with the given attributes and contents.
p :: [Attribute] -> [Html] -> Html
p = ParentNode "<p" "</p>"
{-# INLINE p #-}


-- | Generates an HTML @\<picture\>@ element with the given attributes and contents.
picture :: [Attribute] -> [Html] -> Html
picture = ParentNode "<picture" "</picture>"
{-# INLINE picture #-}


-- | Generates an HTML @\<pre\>@ element with the given attributes and contents.
pre :: [Attribute] -> [Html] -> Html
pre = ParentNode "<pre" "</pre>"
{-# INLINE pre #-}


-- | Generates an HTML @\<progress\>@ element with the given attributes and contents.
progress :: [Attribute] -> [Html] -> Html
progress = ParentNode "<progress" "</progress>"
{-# INLINE progress #-}


-- | Generates an HTML @\<q\>@ element with the given attributes and contents.
q :: [Attribute] -> [Html] -> Html
q = ParentNode "<q" "</q>"
{-# INLINE q #-}


-- | Generates an HTML @\<rp\>@ element with the given attributes and contents.
rp :: [Attribute] -> [Html] -> Html
rp = ParentNode "<rp" "</rp>"
{-# INLINE rp #-}


-- | Generates an HTML @\<rt\>@ element with the given attributes and contents.
rt :: [Attribute] -> [Html] -> Html
rt = ParentNode "<rt" "</rt>"
{-# INLINE rt #-}


-- | Generates an HTML @\<ruby\>@ element with the given attributes and contents.
ruby :: [Attribute] -> [Html] -> Html
ruby = ParentNode "<ruby" "</ruby>"
{-# INLINE ruby #-}


-- | Generates an HTML @\<s\>@ element with the given attributes and contents.
s :: [Attribute] -> [Html] -> Html
s = ParentNode "<s" "</s>"
{-# INLINE s #-}


-- | Generates an HTML @\<samp\>@ element with the given attributes and contents.
samp :: [Attribute] -> [Html] -> Html
samp = ParentNode "<samp" "</samp>"
{-# INLINE samp #-}


-- | Generates an HTML @\<script\>@ element with the given attributes and contents.
script :: [Attribute] -> [Html] -> Html
script = ParentNode "<script" "</script>"
{-# INLINE script #-}


-- | Generates an HTML @\<search\>@ element with the given attributes and contents.
search :: [Attribute] -> [Html] -> Html
search = ParentNode "<search" "</search>"
{-# INLINE search #-}


-- | Generates an HTML @\<section\>@ element with the given attributes and contents.
section :: [Attribute] -> [Html] -> Html
section = ParentNode "<section" "</section>"
{-# INLINE section #-}


-- | Generates an HTML @\<select\>@ element with the given attributes and contents.
select :: [Attribute] -> [Html] -> Html
select = ParentNode "<select" "</select>"
{-# INLINE select #-}


-- | Generates an HTML @\<slot\>@ element with the given attributes and contents.
slot :: [Attribute] -> [Html] -> Html
slot = ParentNode "<slot" "</slot>"
{-# INLINE slot #-}


-- | Generates an HTML @\<small\>@ element with the given attributes and contents.
small :: [Attribute] -> [Html] -> Html
small = ParentNode "<small" "</small>"
{-# INLINE small #-}


-- | Generates an HTML @\<source\>@ element with the given attributes.
source :: [Attribute] -> Html
source = LeafNode "<source"
{-# INLINE source #-}


-- | Generates an HTML @\<span\>@ element with the given attributes and contents.
span :: [Attribute] -> [Html] -> Html
span = ParentNode "<span" "</span>"
{-# INLINE span #-}


-- | Generates an HTML @\<strong\>@ element with the given attributes and contents.
strong :: [Attribute] -> [Html] -> Html
strong = ParentNode "<strong" "</strong>"
{-# INLINE strong #-}


-- | Generates an HTML @\<style\>@ element with the given attributes and contents.
style :: [Attribute] -> [Html] -> Html
style = ParentNode "<style" "</style>"
{-# INLINE style #-}


-- | Generates an HTML @\<sub\>@ element with the given attributes and contents.
sub :: [Attribute] -> [Html] -> Html
sub = ParentNode "<sub" "</sub>"
{-# INLINE sub #-}


-- | Generates an HTML @\<summary\>@ element with the given attributes and contents.
summary :: [Attribute] -> [Html] -> Html
summary = ParentNode "<summary" "</summary>"
{-# INLINE summary #-}


-- | Generates an HTML @\<sup\>@ element with the given attributes and contents.
sup :: [Attribute] -> [Html] -> Html
sup = ParentNode "<sup" "</sup>"
{-# INLINE sup #-}


-- | Generates an HTML @\<table\>@ element with the given attributes and contents.
table :: [Attribute] -> [Html] -> Html
table = ParentNode "<table" "</table>"
{-# INLINE table #-}


-- | Generates an HTML @\<tbody\>@ element with the given attributes and contents.
tbody :: [Attribute] -> [Html] -> Html
tbody = ParentNode "<tbody" "</tbody>"
{-# INLINE tbody #-}


-- | Generates an HTML @\<td\>@ element with the given attributes and contents.
td :: [Attribute] -> [Html] -> Html
td = ParentNode "<td" "</td>"
{-# INLINE td #-}


-- | Generates an HTML @\<template\>@ element with the given attributes and contents.
template :: [Attribute] -> [Html] -> Html
template = ParentNode "<template" "</template>"
{-# INLINE template #-}


-- | Generates an HTML @\<textarea\>@ element with the given attributes and contents.
textarea :: [Attribute] -> [Html] -> Html
textarea = ParentNode "<textarea" "</textarea>"
{-# INLINE textarea #-}


-- | Generates an HTML @\<tfoot\>@ element with the given attributes and contents.
tfoot :: [Attribute] -> [Html] -> Html
tfoot = ParentNode "<tfoot" "</tfoot>"
{-# INLINE tfoot #-}


-- | Generates an HTML @\<th\>@ element with the given attributes and contents.
th :: [Attribute] -> [Html] -> Html
th = ParentNode "<th" "</th>"
{-# INLINE th #-}


-- | Generates an HTML @\<thead\>@ element with the given attributes and contents.
thead :: [Attribute] -> [Html] -> Html
thead = ParentNode "<thead" "</thead>"
{-# INLINE thead #-}


-- | Generates an HTML @\<time\>@ element with the given attributes and contents.
time :: [Attribute] -> [Html] -> Html
time = ParentNode "<time" "</time>"
{-# INLINE time #-}


-- | Generates an HTML @\<title\>@ element with the given attributes and contents.
title :: [Attribute] -> [Html] -> Html
title = ParentNode "<title" "</title>"
{-# INLINE title #-}


-- | Generates an HTML @\<tr\>@ element with the given attributes and contents.
tr :: [Attribute] -> [Html] -> Html
tr = ParentNode "<tr" "</tr>"
{-# INLINE tr #-}


-- | Generates an HTML @\<track\>@ element with the given attributes.
track :: [Attribute] -> Html
track = LeafNode "<track"
{-# INLINE track #-}


-- | Generates an HTML @\<u\>@ element with the given attributes and contents.
u :: [Attribute] -> [Html] -> Html
u = ParentNode "<u" "</u>"
{-# INLINE u #-}


-- | Generates an HTML @\<ul\>@ element with the given attributes and contents.
ul :: [Attribute] -> [Html] -> Html
ul = ParentNode "<ul" "</ul>"
{-# INLINE ul #-}


-- | Generates an HTML @\<var\>@ element with the given attributes and contents.
var :: [Attribute] -> [Html] -> Html
var = ParentNode "<var" "</var>"
{-# INLINE var #-}


-- | Generates an HTML @\<video\>@ element with the given attributes and contents.
video :: [Attribute] -> [Html] -> Html
video = ParentNode "<video" "</video>"
{-# INLINE video #-}


-- | Generates an HTML @\<wbr\>@ element with the given attributes.
wbr :: [Attribute] -> Html
wbr = LeafNode "<wbr"
{-# INLINE wbr #-}
