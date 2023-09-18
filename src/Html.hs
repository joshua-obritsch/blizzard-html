{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
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
--
-- Additionally, the functions provided in the "Html.Intl" module can be used to facilitate internationalization.
module Html
    ( -- * Types
      -- ** Html
      Html(..)
      -- ** Attribute
    , Attribute(..)

      -- * Classes
      -- ** Buildable
    , Buildable(..)
      -- ** Translatable
    , Translatable(..)

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
-- >>> import qualified Html as Html


-- TYPES


-- | Represents an HTML element.
--
-- /Note: The type variable /lng/ stands for /language/ and is used to facilitate internationalization./
data Html lng where

    -- | Constructs an HTML parent node.
    ParentNode :: Builder -> Builder -> [Attribute] -> [Html lng] -> Html lng

    -- | Constructs an HTML leaf node.
    LeafNode :: Builder -> [Attribute] -> Html lng

    -- | Constructs an HTML root node.
    RootNode :: Builder -> [Html lng] -> Html lng

    -- | Constructs a monolingual HTML text node.
    TextNode :: Builder -> Html lng

    -- | Constructs a multilingual HTML text node.
    IntlNode :: Translatable lng => lng -> Html lng


instance IsString (Html lng) where
    fromString = TextNode . fromString


instance Show (Html lng) where
    show = unpack . toLazyText . build


instance Buildable (Html lng) where
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
        IntlNode   intl                                -> text
          where text = defaultLanguage intl


instance {-# OVERLAPPING #-} Show [Html lng] where
    show = unpack . toLazyText . build


instance Buildable [Html lng] where
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
        TextAttribute _   ""    -> mempty
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


-- | Enables the use of multilingual text nodes with 'Html.Html'.
class Translatable a where

    -- | Sets the default language to use for internationalization with 'Html.Html'.
    defaultLanguage :: a -> Builder


-- DECLARATIONS


-- | Generates an HTML @\<!DOCTYPE\>@ declaration with the given contents.
doctype :: [Html lng] -> Html lng
doctype = RootNode "<!DOCTYPE html>\n"
{-# INLINE doctype #-}
-- $doctest
-- >>> Html.doctype [ Html.html [] [ Html.head [] [], Html.body [] [ "test" ] ] ]
-- <!DOCTYPE html>
-- <html><head></head><body>test</body></html>


-- ELEMENTS


-- | Generates an HTML @\<a\>@ element with the given attributes and contents.
a :: [Attribute] -> [Html lng] -> Html lng
a = ParentNode "<a" "</a>"
{-# INLINE a #-}
-- $doctest
-- >>> Html.a [] [ Html.a [] [], Html.a [] [ "test" ] ]
-- <a><a></a><a>test</a></a>


-- | Generates an HTML @\<abbr\>@ element with the given attributes and contents.
abbr :: [Attribute] -> [Html lng] -> Html lng
abbr = ParentNode "<abbr" "</abbr>"
{-# INLINE abbr #-}
-- $doctest
-- >>> Html.abbr [] [ Html.abbr [] [], Html.abbr [] [ "test" ] ]
-- <abbr><abbr></abbr><abbr>test</abbr></abbr>


-- | Generates an HTML @\<address\>@ element with the given attributes and contents.
address :: [Attribute] -> [Html lng] -> Html lng
address = ParentNode "<address" "</address>"
{-# INLINE address #-}
-- $doctest
-- >>> Html.address [] [ Html.address [] [], Html.address [] [ "test" ] ]
-- <address><address></address><address>test</address></address>


-- | Generates an HTML @\<area\>@ element with the given attributes.
area :: [Attribute] -> Html lng
area = LeafNode "<area"
{-# INLINE area #-}
-- $doctest
-- >>> Html.area []
-- <area>


-- | Generates an HTML @\<article\>@ element with the given attributes and contents.
article :: [Attribute] -> [Html lng] -> Html lng
article = ParentNode "<article" "</article>"
{-# INLINE article #-}
-- $doctest
-- >>> Html.article [] [ Html.article [] [], Html.article [] [ "test" ] ]
-- <article><article></article><article>test</article></article>


-- | Generates an HTML @\<aside\>@ element with the given attributes and contents.
aside :: [Attribute] -> [Html lng] -> Html lng
aside = ParentNode "<aside" "</aside>"
{-# INLINE aside #-}
-- $doctest
-- >>> Html.aside [] [ Html.aside [] [], Html.aside [] [ "test" ] ]
-- <aside><aside></aside><aside>test</aside></aside>


-- | Generates an HTML @\<audio\>@ element with the given attributes and contents.
audio :: [Attribute] -> [Html lng] -> Html lng
audio = ParentNode "<audio" "</audio>"
{-# INLINE audio #-}
-- $doctest
-- >>> Html.audio [] [ Html.audio [] [], Html.audio [] [ "test" ] ]
-- <audio><audio></audio><audio>test</audio></audio>


-- | Generates an HTML @\<b\>@ element with the given attributes and contents.
b :: [Attribute] -> [Html lng] -> Html lng
b = ParentNode "<b" "</b>"
{-# INLINE b #-}
-- $doctest
-- >>> Html.b [] [ Html.b [] [], Html.b [] [ "test" ] ]
-- <b><b></b><b>test</b></b>


-- | Generates an HTML @\<base\>@ element with the given attributes.
base :: [Attribute] -> Html lng
base = LeafNode "<base"
{-# INLINE base #-}
-- $doctest
-- >>> Html.base []
-- <base>


-- | Generates an HTML @\<bdi\>@ element with the given attributes and contents.
bdi :: [Attribute] -> [Html lng] -> Html lng
bdi = ParentNode "<bdi" "</bdi>"
{-# INLINE bdi #-}
-- $doctest
-- >>> Html.bdi [] [ Html.bdi [] [], Html.bdi [] [ "test" ] ]
-- <bdi><bdi></bdi><bdi>test</bdi></bdi>


-- | Generates an HTML @\<bdo\>@ element with the given attributes and contents.
bdo :: [Attribute] -> [Html lng] -> Html lng
bdo = ParentNode "<bdo" "</bdo>"
{-# INLINE bdo #-}
-- $doctest
-- >>> Html.bdo [] [ Html.bdo [] [], Html.bdo [] [ "test" ] ]
-- <bdo><bdo></bdo><bdo>test</bdo></bdo>


-- | Generates an HTML @\<blockquote\>@ element with the given attributes and contents.
blockquote :: [Attribute] -> [Html lng] -> Html lng
blockquote = ParentNode "<blockquote" "</blockquote>"
{-# INLINE blockquote #-}
-- $doctest
-- >>> Html.blockquote [] [ Html.blockquote [] [], Html.blockquote [] [ "test" ] ]
-- <blockquote><blockquote></blockquote><blockquote>test</blockquote></blockquote>


-- | Generates an HTML @\<body\>@ element with the given attributes and contents.
body :: [Attribute] -> [Html lng] -> Html lng
body = ParentNode "<body" "</body>"
{-# INLINE body #-}
-- $doctest
-- >>> Html.body [] [ Html.body [] [], Html.body [] [ "test" ] ]
-- <body><body></body><body>test</body></body>


-- | Generates an HTML @\<br\>@ element with the given attributes.
br :: [Attribute] -> Html lng
br = LeafNode "<br"
{-# INLINE br #-}
-- $doctest
-- >>> Html.br []
-- <br>


-- | Generates an HTML @\<button\>@ element with the given attributes and contents.
button :: [Attribute] -> [Html lng] -> Html lng
button = ParentNode "<button" "</button>"
{-# INLINE button #-}
-- $doctest
-- >>> Html.button [] [ Html.button [] [], Html.button [] [ "test" ] ]
-- <button><button></button><button>test</button></button>


-- | Generates an HTML @\<canvas\>@ element with the given attributes and contents.
canvas :: [Attribute] -> [Html lng] -> Html lng
canvas = ParentNode "<canvas" "</canvas>"
{-# INLINE canvas #-}
-- $doctest
-- >>> Html.canvas [] [ Html.canvas [] [], Html.canvas [] [ "test" ] ]
-- <canvas><canvas></canvas><canvas>test</canvas></canvas>


-- | Generates an HTML @\<caption\>@ element with the given attributes and contents.
caption :: [Attribute] -> [Html lng] -> Html lng
caption = ParentNode "<caption" "</caption>"
{-# INLINE caption #-}
-- $doctest
-- >>> Html.caption [] [ Html.caption [] [], Html.caption [] [ "test" ] ]
-- <caption><caption></caption><caption>test</caption></caption>


-- | Generates an HTML @\<cite\>@ element with the given attributes and contents.
cite :: [Attribute] -> [Html lng] -> Html lng
cite = ParentNode "<cite" "</cite>"
{-# INLINE cite #-}
-- $doctest
-- >>> Html.cite [] [ Html.cite [] [], Html.cite [] [ "test" ] ]
-- <cite><cite></cite><cite>test</cite></cite>


-- | Generates an HTML @\<code\>@ element with the given attributes and contents.
code :: [Attribute] -> [Html lng] -> Html lng
code = ParentNode "<code" "</code>"
{-# INLINE code #-}
-- $doctest
-- >>> Html.code [] [ Html.code [] [], Html.code [] [ "test" ] ]
-- <code><code></code><code>test</code></code>


-- | Generates an HTML @\<col\>@ element with the given attributes.
col :: [Attribute] -> Html lng
col = LeafNode "<col"
{-# INLINE col #-}
-- $doctest
-- >>> Html.col []
-- <col>


-- | Generates an HTML @\<colgroup\>@ element with the given attributes and contents.
colgroup :: [Attribute] -> [Html lng] -> Html lng
colgroup = ParentNode "<colgroup" "</colgroup>"
{-# INLINE colgroup #-}
-- $doctest
-- >>> Html.colgroup [] [ Html.colgroup [] [], Html.colgroup [] [ "test" ] ]
-- <colgroup><colgroup></colgroup><colgroup>test</colgroup></colgroup>


-- | Generates an HTML @\<data\>@ element with the given attributes and contents.
data_ :: [Attribute] -> [Html lng] -> Html lng
data_ = ParentNode "<data" "</data>"
{-# INLINE data_ #-}
-- $doctest
-- >>> Html.data_ [] [ Html.data_ [] [], Html.data_ [] [ "test" ] ]
-- <data><data></data><data>test</data></data>


-- | Generates an HTML @\<datalist\>@ element with the given attributes and contents.
datalist :: [Attribute] -> [Html lng] -> Html lng
datalist = ParentNode "<datalist" "</datalist>"
{-# INLINE datalist #-}
-- $doctest
-- >>> Html.datalist [] [ Html.datalist [] [], Html.datalist [] [ "test" ] ]
-- <datalist><datalist></datalist><datalist>test</datalist></datalist>


-- | Generates an HTML @\<dd\>@ element with the given attributes and contents.
dd :: [Attribute] -> [Html lng] -> Html lng
dd = ParentNode "<dd" "</dd>"
{-# INLINE dd #-}
-- $doctest
-- >>> Html.dd [] [ Html.dd [] [], Html.dd [] [ "test" ] ]
-- <dd><dd></dd><dd>test</dd></dd>


-- | Generates an HTML @\<del\>@ element with the given attributes and contents.
del :: [Attribute] -> [Html lng] -> Html lng
del = ParentNode "<del" "</del>"
{-# INLINE del #-}
-- $doctest
-- >>> Html.del [] [ Html.del [] [], Html.del [] [ "test" ] ]
-- <del><del></del><del>test</del></del>


-- | Generates an HTML @\<details\>@ element with the given attributes and contents.
details :: [Attribute] -> [Html lng] -> Html lng
details = ParentNode "<details" "</details>"
{-# INLINE details #-}
-- $doctest
-- >>> Html.details [] [ Html.details [] [], Html.details [] [ "test" ] ]
-- <details><details></details><details>test</details></details>


-- | Generates an HTML @\<dfn\>@ element with the given attributes and contents.
dfn :: [Attribute] -> [Html lng] -> Html lng
dfn = ParentNode "<dfn" "</dfn>"
{-# INLINE dfn #-}
-- $doctest
-- >>> Html.dfn [] [ Html.dfn [] [], Html.dfn [] [ "test" ] ]
-- <dfn><dfn></dfn><dfn>test</dfn></dfn>


-- | Generates an HTML @\<dialog\>@ element with the given attributes and contents.
dialog :: [Attribute] -> [Html lng] -> Html lng
dialog = ParentNode "<dialog" "</dialog>"
{-# INLINE dialog #-}
-- $doctest
-- >>> Html.dialog [] [ Html.dialog [] [], Html.dialog [] [ "test" ] ]
-- <dialog><dialog></dialog><dialog>test</dialog></dialog>


-- | Generates an HTML @\<div\>@ element with the given attributes and contents.
div :: [Attribute] -> [Html lng] -> Html lng
div = ParentNode "<div" "</div>"
{-# INLINE div #-}
-- $doctest
-- >>> Html.div [] [ Html.div [] [], Html.div [] [ "test" ] ]
-- <div><div></div><div>test</div></div>


-- | Generates an HTML @\<dl\>@ element with the given attributes and contents.
dl :: [Attribute] -> [Html lng] -> Html lng
dl = ParentNode "<dl" "</dl>"
{-# INLINE dl #-}
-- $doctest
-- >>> Html.dl [] [ Html.dl [] [], Html.dl [] [ "test" ] ]
-- <dl><dl></dl><dl>test</dl></dl>


-- | Generates an HTML @\<dt\>@ element with the given attributes and contents.
dt :: [Attribute] -> [Html lng] -> Html lng
dt = ParentNode "<dt" "</dt>"
{-# INLINE dt #-}
-- $doctest
-- >>> Html.dt [] [ Html.dt [] [], Html.dt [] [ "test" ] ]
-- <dt><dt></dt><dt>test</dt></dt>


-- | Generates an HTML @\<em\>@ element with the given attributes and contents.
em :: [Attribute] -> [Html lng] -> Html lng
em = ParentNode "<em" "</em>"
{-# INLINE em #-}
-- $doctest
-- >>> Html.em [] [ Html.em [] [], Html.em [] [ "test" ] ]
-- <em><em></em><em>test</em></em>


-- | Generates an HTML @\<embed\>@ element with the given attributes.
embed :: [Attribute] -> Html lng
embed = LeafNode "<embed"
{-# INLINE embed #-}
-- $doctest
-- >>> Html.embed []
-- <embed>


-- | Generates an HTML @\<fieldset\>@ element with the given attributes and contents.
fieldset :: [Attribute] -> [Html lng] -> Html lng
fieldset = ParentNode "<fieldset" "</fieldset>"
{-# INLINE fieldset #-}
-- $doctest
-- >>> Html.fieldset [] [ Html.fieldset [] [], Html.fieldset [] [ "test" ] ]
-- <fieldset><fieldset></fieldset><fieldset>test</fieldset></fieldset>


-- | Generates an HTML @\<figcaption\>@ element with the given attributes and contents.
figcaption :: [Attribute] -> [Html lng] -> Html lng
figcaption = ParentNode "<figcaption" "</figcaption>"
{-# INLINE figcaption #-}
-- $doctest
-- >>> Html.figcaption [] [ Html.figcaption [] [], Html.figcaption [] [ "test" ] ]
-- <figcaption><figcaption></figcaption><figcaption>test</figcaption></figcaption>


-- | Generates an HTML @\<figure\>@ element with the given attributes and contents.
figure :: [Attribute] -> [Html lng] -> Html lng
figure = ParentNode "<figure" "</figure>"
{-# INLINE figure #-}
-- $doctest
-- >>> Html.figure [] [ Html.figure [] [], Html.figure [] [ "test" ] ]
-- <figure><figure></figure><figure>test</figure></figure>


-- | Generates an HTML @\<footer\>@ element with the given attributes and contents.
footer :: [Attribute] -> [Html lng] -> Html lng
footer = ParentNode "<footer" "</footer>"
{-# INLINE footer #-}
-- $doctest
-- >>> Html.footer [] [ Html.footer [] [], Html.footer [] [ "test" ] ]
-- <footer><footer></footer><footer>test</footer></footer>


-- | Generates an HTML @\<form\>@ element with the given attributes and contents.
form :: [Attribute] -> [Html lng] -> Html lng
form = ParentNode "<form" "</form>"
{-# INLINE form #-}
-- $doctest
-- >>> Html.form [] [ Html.form [] [], Html.form [] [ "test" ] ]
-- <form><form></form><form>test</form></form>


-- | Generates an HTML @\<h1\>@ element with the given attributes and contents.
h1 :: [Attribute] -> [Html lng] -> Html lng
h1 = ParentNode "<h1" "</h1>"
{-# INLINE h1 #-}
-- $doctest
-- >>> Html.h1 [] [ Html.h1 [] [], Html.h1 [] [ "test" ] ]
-- <h1><h1></h1><h1>test</h1></h1>


-- | Generates an HTML @\<h2\>@ element with the given attributes and contents.
h2 :: [Attribute] -> [Html lng] -> Html lng
h2 = ParentNode "<h2" "</h2>"
{-# INLINE h2 #-}
-- $doctest
-- >>> Html.h2 [] [ Html.h2 [] [], Html.h2 [] [ "test" ] ]
-- <h2><h2></h2><h2>test</h2></h2>


-- | Generates an HTML @\<h3\>@ element with the given attributes and contents.
h3 :: [Attribute] -> [Html lng] -> Html lng
h3 = ParentNode "<h3" "</h3>"
{-# INLINE h3 #-}
-- $doctest
-- >>> Html.h3 [] [ Html.h3 [] [], Html.h3 [] [ "test" ] ]
-- <h3><h3></h3><h3>test</h3></h3>


-- | Generates an HTML @\<h4\>@ element with the given attributes and contents.
h4 :: [Attribute] -> [Html lng] -> Html lng
h4 = ParentNode "<h4" "</h4>"
{-# INLINE h4 #-}
-- $doctest
-- >>> Html.h4 [] [ Html.h4 [] [], Html.h4 [] [ "test" ] ]
-- <h4><h4></h4><h4>test</h4></h4>


-- | Generates an HTML @\<h5\>@ element with the given attributes and contents.
h5 :: [Attribute] -> [Html lng] -> Html lng
h5 = ParentNode "<h5" "</h5>"
{-# INLINE h5 #-}
-- $doctest
-- >>> Html.h5 [] [ Html.h5 [] [], Html.h5 [] [ "test" ] ]
-- <h5><h5></h5><h5>test</h5></h5>


-- | Generates an HTML @\<h6\>@ element with the given attributes and contents.
h6 :: [Attribute] -> [Html lng] -> Html lng
h6 = ParentNode "<h6" "</h6>"
{-# INLINE h6 #-}
-- $doctest
-- >>> Html.h6 [] [ Html.h6 [] [], Html.h6 [] [ "test" ] ]
-- <h6><h6></h6><h6>test</h6></h6>


-- | Generates an HTML @\<head\>@ element with the given attributes and contents.
head :: [Attribute] -> [Html lng] -> Html lng
head = ParentNode "<head" "</head>"
{-# INLINE head #-}
-- $doctest
-- >>> Html.head [] [ Html.head [] [], Html.head [] [ "test" ] ]
-- <head><head></head><head>test</head></head>


-- | Generates an HTML @\<header\>@ element with the given attributes and contents.
header :: [Attribute] -> [Html lng] -> Html lng
header = ParentNode "<header" "</header>"
{-# INLINE header #-}
-- $doctest
-- >>> Html.header [] [ Html.header [] [], Html.header [] [ "test" ] ]
-- <header><header></header><header>test</header></header>


-- | Generates an HTML @\<hgroup\>@ element with the given attributes and contents.
hgroup :: [Attribute] -> [Html lng] -> Html lng
hgroup = ParentNode "<hgroup" "</hgroup>"
{-# INLINE hgroup #-}
-- $doctest
-- >>> Html.hgroup [] [ Html.hgroup [] [], Html.hgroup [] [ "test" ] ]
-- <hgroup><hgroup></hgroup><hgroup>test</hgroup></hgroup>


-- | Generates an HTML @\<hr\>@ element with the given attributes.
hr :: [Attribute] -> Html lng
hr = LeafNode "<hr"
{-# INLINE hr #-}
-- $doctest
-- >>> Html.hr []
-- <hr>


-- | Generates an HTML @\<html\>@ element with the given attributes and contents.
html :: [Attribute] -> [Html lng] -> Html lng
html = ParentNode "<html" "</html>"
{-# INLINE html #-}
-- $doctest
-- >>> Html.html [] [ Html.html [] [], Html.html [] [ "test" ] ]
-- <html><html></html><html>test</html></html>


-- | Generates an HTML @\<i\>@ element with the given attributes and contents.
i :: [Attribute] -> [Html lng] -> Html lng
i = ParentNode "<i" "</i>"
{-# INLINE i #-}
-- $doctest
-- >>> Html.i [] [ Html.i [] [], Html.i [] [ "test" ] ]
-- <i><i></i><i>test</i></i>


-- | Generates an HTML @\<iframe\>@ element with the given attributes and contents.
iframe :: [Attribute] -> [Html lng] -> Html lng
iframe = ParentNode "<iframe" "</iframe>"
{-# INLINE iframe #-}
-- $doctest
-- >>> Html.iframe [] [ Html.iframe [] [], Html.iframe [] [ "test" ] ]
-- <iframe><iframe></iframe><iframe>test</iframe></iframe>


-- | Generates an HTML @\<img\>@ element with the given attributes.
img :: [Attribute] -> Html lng
img = LeafNode "<img"
{-# INLINE img #-}
-- $doctest
-- >>> Html.img []
-- <img>


-- | Generates an HTML @\<input\>@ element with the given attributes.
input :: [Attribute] -> Html lng
input = LeafNode "<input"
{-# INLINE input #-}
-- $doctest
-- >>> Html.input []
-- <input>


-- | Generates an HTML @\<ins\>@ element with the given attributes and contents.
ins :: [Attribute] -> [Html lng] -> Html lng
ins = ParentNode "<ins" "</ins>"
{-# INLINE ins #-}
-- $doctest
-- >>> Html.ins [] [ Html.ins [] [], Html.ins [] [ "test" ] ]
-- <ins><ins></ins><ins>test</ins></ins>


-- | Generates an HTML @\<kbd\>@ element with the given attributes and contents.
kbd :: [Attribute] -> [Html lng] -> Html lng
kbd = ParentNode "<kbd" "</kbd>"
{-# INLINE kbd #-}
-- $doctest
-- >>> Html.kbd [] [ Html.kbd [] [], Html.kbd [] [ "test" ] ]
-- <kbd><kbd></kbd><kbd>test</kbd></kbd>


-- | Generates an HTML @\<label\>@ element with the given attributes and contents.
label :: [Attribute] -> [Html lng] -> Html lng
label = ParentNode "<label" "</label>"
{-# INLINE label #-}
-- $doctest
-- >>> Html.label [] [ Html.label [] [], Html.label [] [ "test" ] ]
-- <label><label></label><label>test</label></label>


-- | Generates an HTML @\<legend\>@ element with the given attributes and contents.
legend :: [Attribute] -> [Html lng] -> Html lng
legend = ParentNode "<legend" "</legend>"
{-# INLINE legend #-}
-- $doctest
-- >>> Html.legend [] [ Html.legend [] [], Html.legend [] [ "test" ] ]
-- <legend><legend></legend><legend>test</legend></legend>


-- | Generates an HTML @\<li\>@ element with the given attributes and contents.
li :: [Attribute] -> [Html lng] -> Html lng
li = ParentNode "<li" "</li>"
{-# INLINE li #-}
-- $doctest
-- >>> Html.li [] [ Html.li [] [], Html.li [] [ "test" ] ]
-- <li><li></li><li>test</li></li>


-- | Generates an HTML @\<link\>@ element with the given attributes.
link :: [Attribute] -> Html lng
link = LeafNode "<link"
{-# INLINE link #-}
-- $doctest
-- >>> Html.link []
-- <link>


-- | Generates an HTML @\<main\>@ element with the given attributes and contents.
main :: [Attribute] -> [Html lng] -> Html lng
main = ParentNode "<main" "</main>"
{-# INLINE main #-}
-- $doctest
-- >>> Html.main [] [ Html.main [] [], Html.main [] [ "test" ] ]
-- <main><main></main><main>test</main></main>


-- | Generates an HTML @\<map\>@ element with the given attributes and contents.
map :: [Attribute] -> [Html lng] -> Html lng
map = ParentNode "<map" "</map>"
{-# INLINE map #-}
-- $doctest
-- >>> Html.map [] [ Html.map [] [], Html.map [] [ "test" ] ]
-- <map><map></map><map>test</map></map>


-- | Generates an HTML @\<mark\>@ element with the given attributes and contents.
mark :: [Attribute] -> [Html lng] -> Html lng
mark = ParentNode "<mark" "</mark>"
{-# INLINE mark #-}
-- $doctest
-- >>> Html.mark [] [ Html.mark [] [], Html.mark [] [ "test" ] ]
-- <mark><mark></mark><mark>test</mark></mark>


-- | Generates an HTML @\<menu\>@ element with the given attributes and contents.
menu :: [Attribute] -> [Html lng] -> Html lng
menu = ParentNode "<menu" "</menu>"
{-# INLINE menu #-}
-- $doctest
-- >>> Html.menu [] [ Html.menu [] [], Html.menu [] [ "test" ] ]
-- <menu><menu></menu><menu>test</menu></menu>


-- | Generates an HTML @\<meta\>@ element with the given attributes.
meta :: [Attribute] -> Html lng
meta = LeafNode "<meta"
{-# INLINE meta #-}
-- $doctest
-- >>> Html.meta []
-- <meta>


-- | Generates an HTML @\<meter\>@ element with the given attributes and contents.
meter :: [Attribute] -> [Html lng] -> Html lng
meter = ParentNode "<meter" "</meter>"
{-# INLINE meter #-}
-- $doctest
-- >>> Html.meter [] [ Html.meter [] [], Html.meter [] [ "test" ] ]
-- <meter><meter></meter><meter>test</meter></meter>


-- | Generates an HTML @\<nav\>@ element with the given attributes and contents.
nav :: [Attribute] -> [Html lng] -> Html lng
nav = ParentNode "<nav" "</nav>"
{-# INLINE nav #-}
-- $doctest
-- >>> Html.nav [] [ Html.nav [] [], Html.nav [] [ "test" ] ]
-- <nav><nav></nav><nav>test</nav></nav>


-- | Generates an HTML @\<noscript\>@ element with the given attributes and contents.
noscript :: [Attribute] -> [Html lng] -> Html lng
noscript = ParentNode "<noscript" "</noscript>"
{-# INLINE noscript #-}
-- $doctest
-- >>> Html.noscript [] [ Html.noscript [] [], Html.noscript [] [ "test" ] ]
-- <noscript><noscript></noscript><noscript>test</noscript></noscript>


-- | Generates an HTML @\<object\>@ element with the given attributes and contents.
object :: [Attribute] -> [Html lng] -> Html lng
object = ParentNode "<object" "</object>"
{-# INLINE object #-}
-- $doctest
-- >>> Html.object [] [ Html.object [] [], Html.object [] [ "test" ] ]
-- <object><object></object><object>test</object></object>


-- | Generates an HTML @\<ol\>@ element with the given attributes and contents.
ol :: [Attribute] -> [Html lng] -> Html lng
ol = ParentNode "<ol" "</ol>"
{-# INLINE ol #-}
-- $doctest
-- >>> Html.ol [] [ Html.ol [] [], Html.ol [] [ "test" ] ]
-- <ol><ol></ol><ol>test</ol></ol>


-- | Generates an HTML @\<optgroup\>@ element with the given attributes and contents.
optgroup :: [Attribute] -> [Html lng] -> Html lng
optgroup = ParentNode "<optgroup" "</optgroup>"
{-# INLINE optgroup #-}
-- $doctest
-- >>> Html.optgroup [] [ Html.optgroup [] [], Html.optgroup [] [ "test" ] ]
-- <optgroup><optgroup></optgroup><optgroup>test</optgroup></optgroup>


-- | Generates an HTML @\<option\>@ element with the given attributes and contents.
option :: [Attribute] -> [Html lng] -> Html lng
option = ParentNode "<option" "</option>"
{-# INLINE option #-}
-- $doctest
-- >>> Html.option [] [ Html.option [] [], Html.option [] [ "test" ] ]
-- <option><option></option><option>test</option></option>


-- | Generates an HTML @\<output\>@ element with the given attributes and contents.
output :: [Attribute] -> [Html lng] -> Html lng
output = ParentNode "<output" "</output>"
{-# INLINE output #-}
-- $doctest
-- >>> Html.output [] [ Html.output [] [], Html.output [] [ "test" ] ]
-- <output><output></output><output>test</output></output>


-- | Generates an HTML @\<p\>@ element with the given attributes and contents.
p :: [Attribute] -> [Html lng] -> Html lng
p = ParentNode "<p" "</p>"
{-# INLINE p #-}
-- $doctest
-- >>> Html.p [] [ Html.p [] [], Html.p [] [ "test" ] ]
-- <p><p></p><p>test</p></p>


-- | Generates an HTML @\<picture\>@ element with the given attributes and contents.
picture :: [Attribute] -> [Html lng] -> Html lng
picture = ParentNode "<picture" "</picture>"
{-# INLINE picture #-}
-- $doctest
-- >>> Html.picture [] [ Html.picture [] [], Html.picture [] [ "test" ] ]
-- <picture><picture></picture><picture>test</picture></picture>


-- | Generates an HTML @\<pre\>@ element with the given attributes and contents.
pre :: [Attribute] -> [Html lng] -> Html lng
pre = ParentNode "<pre" "</pre>"
{-# INLINE pre #-}
-- $doctest
-- >>> Html.pre [] [ Html.pre [] [], Html.pre [] [ "test" ] ]
-- <pre><pre></pre><pre>test</pre></pre>


-- | Generates an HTML @\<progress\>@ element with the given attributes and contents.
progress :: [Attribute] -> [Html lng] -> Html lng
progress = ParentNode "<progress" "</progress>"
{-# INLINE progress #-}
-- $doctest
-- >>> Html.progress [] [ Html.progress [] [], Html.progress [] [ "test" ] ]
-- <progress><progress></progress><progress>test</progress></progress>


-- | Generates an HTML @\<q\>@ element with the given attributes and contents.
q :: [Attribute] -> [Html lng] -> Html lng
q = ParentNode "<q" "</q>"
{-# INLINE q #-}
-- $doctest
-- >>> Html.q [] [ Html.q [] [], Html.q [] [ "test" ] ]
-- <q><q></q><q>test</q></q>


-- | Generates an HTML @\<rp\>@ element with the given attributes and contents.
rp :: [Attribute] -> [Html lng] -> Html lng
rp = ParentNode "<rp" "</rp>"
{-# INLINE rp #-}
-- $doctest
-- >>> Html.rp [] [ Html.rp [] [], Html.rp [] [ "test" ] ]
-- <rp><rp></rp><rp>test</rp></rp>


-- | Generates an HTML @\<rt\>@ element with the given attributes and contents.
rt :: [Attribute] -> [Html lng] -> Html lng
rt = ParentNode "<rt" "</rt>"
{-# INLINE rt #-}
-- $doctest
-- >>> Html.rt [] [ Html.rt [] [], Html.rt [] [ "test" ] ]
-- <rt><rt></rt><rt>test</rt></rt>


-- | Generates an HTML @\<ruby\>@ element with the given attributes and contents.
ruby :: [Attribute] -> [Html lng] -> Html lng
ruby = ParentNode "<ruby" "</ruby>"
{-# INLINE ruby #-}
-- $doctest
-- >>> Html.ruby [] [ Html.ruby [] [], Html.ruby [] [ "test" ] ]
-- <ruby><ruby></ruby><ruby>test</ruby></ruby>


-- | Generates an HTML @\<s\>@ element with the given attributes and contents.
s :: [Attribute] -> [Html lng] -> Html lng
s = ParentNode "<s" "</s>"
{-# INLINE s #-}
-- $doctest
-- >>> Html.s [] [ Html.s [] [], Html.s [] [ "test" ] ]
-- <s><s></s><s>test</s></s>


-- | Generates an HTML @\<samp\>@ element with the given attributes and contents.
samp :: [Attribute] -> [Html lng] -> Html lng
samp = ParentNode "<samp" "</samp>"
{-# INLINE samp #-}
-- $doctest
-- >>> Html.samp [] [ Html.samp [] [], Html.samp [] [ "test" ] ]
-- <samp><samp></samp><samp>test</samp></samp>


-- | Generates an HTML @\<script\>@ element with the given attributes and contents.
script :: [Attribute] -> [Html lng] -> Html lng
script = ParentNode "<script" "</script>"
{-# INLINE script #-}
-- $doctest
-- >>> Html.script [] [ Html.script [] [], Html.script [] [ "test" ] ]
-- <script><script></script><script>test</script></script>


-- | Generates an HTML @\<section\>@ element with the given attributes and contents.
section :: [Attribute] -> [Html lng] -> Html lng
section = ParentNode "<section" "</section>"
{-# INLINE section #-}
-- $doctest
-- >>> Html.section [] [ Html.section [] [], Html.section [] [ "test" ] ]
-- <section><section></section><section>test</section></section>


-- | Generates an HTML @\<select\>@ element with the given attributes and contents.
select :: [Attribute] -> [Html lng] -> Html lng
select = ParentNode "<select" "</select>"
{-# INLINE select #-}
-- $doctest
-- >>> Html.select [] [ Html.select [] [], Html.select [] [ "test" ] ]
-- <select><select></select><select>test</select></select>


-- | Generates an HTML @\<slot\>@ element with the given attributes and contents.
slot :: [Attribute] -> [Html lng] -> Html lng
slot = ParentNode "<slot" "</slot>"
{-# INLINE slot #-}
-- $doctest
-- >>> Html.slot [] [ Html.slot [] [], Html.slot [] [ "test" ] ]
-- <slot><slot></slot><slot>test</slot></slot>


-- | Generates an HTML @\<small\>@ element with the given attributes and contents.
small :: [Attribute] -> [Html lng] -> Html lng
small = ParentNode "<small" "</small>"
{-# INLINE small #-}
-- $doctest
-- >>> Html.small [] [ Html.small [] [], Html.small [] [ "test" ] ]
-- <small><small></small><small>test</small></small>


-- | Generates an HTML @\<source\>@ element with the given attributes.
source :: [Attribute] -> Html lng
source = LeafNode "<source"
{-# INLINE source #-}
-- $doctest
-- >>> Html.source []
-- <source>


-- | Generates an HTML @\<span\>@ element with the given attributes and contents.
span :: [Attribute] -> [Html lng] -> Html lng
span = ParentNode "<span" "</span>"
{-# INLINE span #-}
-- $doctest
-- >>> Html.span [] [ Html.span [] [], Html.span [] [ "test" ] ]
-- <span><span></span><span>test</span></span>


-- | Generates an HTML @\<strong\>@ element with the given attributes and contents.
strong :: [Attribute] -> [Html lng] -> Html lng
strong = ParentNode "<strong" "</strong>"
{-# INLINE strong #-}
-- $doctest
-- >>> Html.strong [] [ Html.strong [] [], Html.strong [] [ "test" ] ]
-- <strong><strong></strong><strong>test</strong></strong>


-- | Generates an HTML @\<style\>@ element with the given attributes and contents.
style :: [Attribute] -> [Html lng] -> Html lng
style = ParentNode "<style" "</style>"
{-# INLINE style #-}
-- $doctest
-- >>> Html.style [] [ Html.style [] [], Html.style [] [ "test" ] ]
-- <style><style></style><style>test</style></style>


-- | Generates an HTML @\<sub\>@ element with the given attributes and contents.
sub :: [Attribute] -> [Html lng] -> Html lng
sub = ParentNode "<sub" "</sub>"
{-# INLINE sub #-}
-- $doctest
-- >>> Html.sub [] [ Html.sub [] [], Html.sub [] [ "test" ] ]
-- <sub><sub></sub><sub>test</sub></sub>


-- | Generates an HTML @\<summary\>@ element with the given attributes and contents.
summary :: [Attribute] -> [Html lng] -> Html lng
summary = ParentNode "<summary" "</summary>"
{-# INLINE summary #-}
-- $doctest
-- >>> Html.summary [] [ Html.summary [] [], Html.summary [] [ "test" ] ]
-- <summary><summary></summary><summary>test</summary></summary>


-- | Generates an HTML @\<sup\>@ element with the given attributes and contents.
sup :: [Attribute] -> [Html lng] -> Html lng
sup = ParentNode "<sup" "</sup>"
{-# INLINE sup #-}
-- $doctest
-- >>> Html.sup [] [ Html.sup [] [], Html.sup [] [ "test" ] ]
-- <sup><sup></sup><sup>test</sup></sup>


-- | Generates an HTML @\<table\>@ element with the given attributes and contents.
table :: [Attribute] -> [Html lng] -> Html lng
table = ParentNode "<table" "</table>"
{-# INLINE table #-}
-- $doctest
-- >>> Html.table [] [ Html.table [] [], Html.table [] [ "test" ] ]
-- <table><table></table><table>test</table></table>


-- | Generates an HTML @\<tbody\>@ element with the given attributes and contents.
tbody :: [Attribute] -> [Html lng] -> Html lng
tbody = ParentNode "<tbody" "</tbody>"
{-# INLINE tbody #-}
-- $doctest
-- >>> Html.tbody [] [ Html.tbody [] [], Html.tbody [] [ "test" ] ]
-- <tbody><tbody></tbody><tbody>test</tbody></tbody>


-- | Generates an HTML @\<td\>@ element with the given attributes and contents.
td :: [Attribute] -> [Html lng] -> Html lng
td = ParentNode "<td" "</td>"
{-# INLINE td #-}
-- $doctest
-- >>> Html.td [] [ Html.td [] [], Html.td [] [ "test" ] ]
-- <td><td></td><td>test</td></td>


-- | Generates an HTML @\<template\>@ element with the given attributes and contents.
template :: [Attribute] -> [Html lng] -> Html lng
template = ParentNode "<template" "</template>"
{-# INLINE template #-}
-- $doctest
-- >>> Html.template [] [ Html.template [] [], Html.template [] [ "test" ] ]
-- <template><template></template><template>test</template></template>


-- | Generates an HTML @\<textarea\>@ element with the given attributes and contents.
textarea :: [Attribute] -> [Html lng] -> Html lng
textarea = ParentNode "<textarea" "</textarea>"
{-# INLINE textarea #-}
-- $doctest
-- >>> Html.textarea [] [ Html.textarea [] [], Html.textarea [] [ "test" ] ]
-- <textarea><textarea></textarea><textarea>test</textarea></textarea>


-- | Generates an HTML @\<tfoot\>@ element with the given attributes and contents.
tfoot :: [Attribute] -> [Html lng] -> Html lng
tfoot = ParentNode "<tfoot" "</tfoot>"
{-# INLINE tfoot #-}
-- $doctest
-- >>> Html.tfoot [] [ Html.tfoot [] [], Html.tfoot [] [ "test" ] ]
-- <tfoot><tfoot></tfoot><tfoot>test</tfoot></tfoot>


-- | Generates an HTML @\<th\>@ element with the given attributes and contents.
th :: [Attribute] -> [Html lng] -> Html lng
th = ParentNode "<th" "</th>"
{-# INLINE th #-}
-- $doctest
-- >>> Html.th [] [ Html.th [] [], Html.th [] [ "test" ] ]
-- <th><th></th><th>test</th></th>


-- | Generates an HTML @\<thead\>@ element with the given attributes and contents.
thead :: [Attribute] -> [Html lng] -> Html lng
thead = ParentNode "<thead" "</thead>"
{-# INLINE thead #-}
-- $doctest
-- >>> Html.thead [] [ Html.thead [] [], Html.thead [] [ "test" ] ]
-- <thead><thead></thead><thead>test</thead></thead>


-- | Generates an HTML @\<time\>@ element with the given attributes and contents.
time :: [Attribute] -> [Html lng] -> Html lng
time = ParentNode "<time" "</time>"
{-# INLINE time #-}
-- $doctest
-- >>> Html.time [] [ Html.time [] [], Html.time [] [ "test" ] ]
-- <time><time></time><time>test</time></time>


-- | Generates an HTML @\<title\>@ element with the given attributes and contents.
title :: [Attribute] -> [Html lng] -> Html lng
title = ParentNode "<title" "</title>"
{-# INLINE title #-}
-- $doctest
-- >>> Html.title [] [ Html.title [] [], Html.title [] [ "test" ] ]
-- <title><title></title><title>test</title></title>


-- | Generates an HTML @\<tr\>@ element with the given attributes and contents.
tr :: [Attribute] -> [Html lng] -> Html lng
tr = ParentNode "<tr" "</tr>"
{-# INLINE tr #-}
-- $doctest
-- >>> Html.tr [] [ Html.tr [] [], Html.tr [] [ "test" ] ]
-- <tr><tr></tr><tr>test</tr></tr>


-- | Generates an HTML @\<track\>@ element with the given attributes.
track :: [Attribute] -> Html lng
track = LeafNode "<track"
{-# INLINE track #-}
-- $doctest
-- >>> Html.track []
-- <track>


-- | Generates an HTML @\<u\>@ element with the given attributes and contents.
u :: [Attribute] -> [Html lng] -> Html lng
u = ParentNode "<u" "</u>"
{-# INLINE u #-}
-- $doctest
-- >>> Html.u [] [ Html.u [] [], Html.u [] [ "test" ] ]
-- <u><u></u><u>test</u></u>


-- | Generates an HTML @\<ul\>@ element with the given attributes and contents.
ul :: [Attribute] -> [Html lng] -> Html lng
ul = ParentNode "<ul" "</ul>"
{-# INLINE ul #-}
-- $doctest
-- >>> Html.ul [] [ Html.ul [] [], Html.ul [] [ "test" ] ]
-- <ul><ul></ul><ul>test</ul></ul>


-- | Generates an HTML @\<var\>@ element with the given attributes and contents.
var :: [Attribute] -> [Html lng] -> Html lng
var = ParentNode "<var" "</var>"
{-# INLINE var #-}
-- $doctest
-- >>> Html.var [] [ Html.var [] [], Html.var [] [ "test" ] ]
-- <var><var></var><var>test</var></var>


-- | Generates an HTML @\<video\>@ element with the given attributes and contents.
video :: [Attribute] -> [Html lng] -> Html lng
video = ParentNode "<video" "</video>"
{-# INLINE video #-}
-- $doctest
-- >>> Html.video [] [ Html.video [] [], Html.video [] [ "test" ] ]
-- <video><video></video><video>test</video></video>


-- | Generates an HTML @\<wbr\>@ element with the given attributes.
wbr :: [Attribute] -> Html lng
wbr = LeafNode "<wbr"
{-# INLINE wbr #-}
-- $doctest
-- >>> Html.wbr []
-- <wbr>
