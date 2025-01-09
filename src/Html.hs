{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}


-- | Module    : Html
-- Copyright   : (c) Joshua Obritsch, 2021-2025
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
-- = Element Content Categories
--
-- == Metadata content#metadata#
-- 'Html.base'; 'Html.link'; 'Html.meta'; 'Html.noscript'; 'Html.script'; 'Html.style'; 'Html.template'; 'Html.title'
--
-- == Flow content#flow#
-- 'Html.a'; 'Html.abbr'; 'Html.address'; 'Html.article'; 'Html.aside'; 'Html.audio'; 'Html.b'; 'Html.bdi'; 'Html.bdo'; 'Html.blockquote'; 'Html.br'; 'Html.button'; 'Html.canvas'; 'Html.cite'; 'Html.code'; 'Html.data_'; 'Html.datalist'; 'Html.del'; 'Html.details'; 'Html.dfn'; 'Html.dialog'; 'Html.div'; 'Html.dl'; 'Html.em'; 'Html.embed'; 'Html.fieldset'; 'Html.figure'; 'Html.footer'; 'Html.form'; 'Html.h1'; 'Html.h2'; 'Html.h3'; 'Html.h4'; 'Html.h5'; 'Html.h6'; 'Html.header'; 'Html.hgroup'; 'Html.hr'; 'Html.i'; 'Html.iframe'; 'Html.img'; 'Html.input'; 'Html.ins'; 'Html.kbd'; 'Html.label'; 'Html.map'; 'Html.mark'; 'Html.Math.math'; 'Html.menu'; 'Html.meter'; 'Html.nav'; 'Html.noscript'; 'Html.object'; 'Html.ol'; 'Html.output'; 'Html.p'; 'Html.picture'; 'Html.pre'; 'Html.progress'; 'Html.q'; 'Html.ruby'; 'Html.s'; 'Html.samp'; 'Html.script'; 'Html.search'; 'Html.section'; 'Html.select'; 'Html.slot'; 'Html.small'; 'Html.span'; 'Html.strong'; 'Html.sub'; 'Html.sup'; 'Html.Svg.svg'; 'Html.table'; 'Html.template'; 'Html.textarea'; 'Html.time'; 'Html.u'; 'Html.ul'; 'Html.var'; 'Html.video'; 'Html.wbr'; autonomous custom elements; text
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


-- | Generates an HTML __@\<!DOCTYPE\>@__ declaration with the given contents.
--
-- [@Example@]:
--
-- >>> doctype []
-- <!DOCTYPE html>
-- <BLANKLINE>
doctype :: [Html] -> Html
doctype = RootNode "<!DOCTYPE html>\n"
{-# INLINE doctype #-}


-- ELEMENTS


-- | Generates an HTML __@\<a\>@__ element with the given attributes and contents.
--
-- [@Description@]: Hyperlink
-- [@Categories@]: /[flow](#flow)/; /[phrasing](#phrasing)/; /[interactive](#interactive)/; /[palpable](#palpable)/
-- [@Parents@]: /[phrasing](#phrasing)/
-- [@Children@]: transparent
-- [@Attributes@]: /[globals]("Html.Attributes#globals")/; 'Html.Attributes.href'; 'Html.Attributes.target'; 'Html.Attributes.download'; 'Html.Attributes.ping'; 'Html.Attributes.rel'; 'Html.Attributes.hreflang'; 'Html.Attributes.type_'; 'Html.Attributes.referrerpolicy'
-- [@Example@]:
--
-- >>> a [] []
-- <a></a>
a :: [Attribute] -> [Html] -> Html
a = ParentNode "<a" "</a>"
{-# INLINE a #-}


-- | Generates an HTML __@\<abbr\>@__ element with the given attributes and contents.
--
-- [@Description@]: Abbreviation
-- [@Categories@]: /[flow](#flow)/; /[phrasing](#phrasing)/; /[palpable](#palpable)/
-- [@Parents@]: /[phrasing](#phrasing)/
-- [@Children@]: /[phrasing](#phrasing)/
-- [@Attributes@]: /[globals]("Html.Attributes#globals")/
-- [@Example@]:
--
-- >>> abbr [] []
-- <abbr></abbr>
--
-- /Note: This element collides with the 'Html.Attributes.abbr' attribute./
abbr :: [Attribute] -> [Html] -> Html
abbr = ParentNode "<abbr" "</abbr>"
{-# INLINE abbr #-}


-- | Generates an HTML __@\<address\>@__ element with the given attributes and contents.
--
-- [@Description@]: Contract information for a page or 'Html.article' element
-- [@Categories@]: /[flow](#flow)/; /[palpable](#palpable)/
-- [@Parents@]: /[flow](#flow)/
-- [@Children@]: /[flow](#flow)/
-- [@Attributes@]: /[globals]("Html.Attributes#globals")/
-- [@Example@]:
--
-- >>> address [] []
-- <address></address>
address :: [Attribute] -> [Html] -> Html
address = ParentNode "<address" "</address>"
{-# INLINE address #-}


-- | Generates an HTML __@\<area\>@__ element with the given attributes.
--
-- [@Description@]: Hyperlink or dead area on an image map
-- [@Categories@]: /[flow](#flow)/; /[phrasing](#phrasing)/
-- [@Parents@]: /[phrasing](#phrasing)/
-- [@Children@]: empty
-- [@Attributes@]: /[globals]("Html.Attributes#globals")/; 'Html.Attributes.alt'; 'Html.Attributes.coords'; 'Html.Attributes.shape'; 'Html.Attributes.href'; 'Html.Attributes.target'; 'Html.Attributes.download'; 'Html.Attributes.ping'; 'Html.Attributes.rel'; 'Html.Attributes.referrerpolicy'
-- [@Example@]:
--
-- >>> area []
-- <area>
area :: [Attribute] -> Html
area = LeafNode "<area"
{-# INLINE area #-}


-- | Generates an HTML __@\<article\>@__ element with the given attributes and contents.
--
-- [@Description@]: Self-contained syndicatable or reusable composition
-- [@Categories@]: /[flow](#flow)/; /[sectioning](#sectioning)/; /[palpable](#palpable)/
-- [@Parents@]: /[flow](#flow)/
-- [@Children@]: /[flow](#flow)/
-- [@Attributes@]: /[globals]("Html.Attributes#globals")/
-- [@Example@]:
--
-- >>> article [] []
-- <article></article>
article :: [Attribute] -> [Html] -> Html
article = ParentNode "<article" "</article>"
{-# INLINE article #-}


-- | Generates an HTML __@\<aside\>@__ element with the given attributes and contents.
--
-- [@Description@]: Sidebar for tangentially related content
-- [@Categories@]: /[flow](#flow)/; /[sectioning](#sectioning)/; /[palpable](#palpable)/
-- [@Parents@]: /[flow](#flow)/
-- [@Children@]: /[flow](#flow)/
-- [@Attributes@]: /[globals]("Html.Attributes#globals")/
-- [@Example@]:
--
-- >>> aside [] []
-- <aside></aside>
aside :: [Attribute] -> [Html] -> Html
aside = ParentNode "<aside" "</aside>"
{-# INLINE aside #-}


-- | Generates an HTML __@\<audio\>@__ element with the given attributes and contents.
--
-- [@Description@]: Audio player
-- [@Categories@]: /[flow](#flow)/; /[phrasing](#phrasing)/; /[embedded](#embedded)/; /[interactive](#interactive)/; /[palpable](#palpable)/
-- [@Parents@]: /[phrasing](#phrasing)/
-- [@Children@]: 'Html.source'; 'Html.track'; transparent
-- [@Attributes@]: /[globals]("Html.Attributes#globals")/; 'Html.Attributes.src'; 'Html.Attributes.crossorigin'; 'Html.Attributes.preload'; 'Html.Attributes.autoplay'; 'Html.Attributes.loop'; 'Html.Attributes.muted'; 'Html.Attributes.controls'
-- [@Example@]:
--
-- >>> audio [] []
-- <audio></audio>
audio :: [Attribute] -> [Html] -> Html
audio = ParentNode "<audio" "</audio>"
{-# INLINE audio #-}


-- | Generates an HTML __@\<b\>@__ element with the given attributes and contents.
--
-- [@Description@]: Keywords
-- [@Categories@]: /[flow](#flow)/; /[phrasing](#phrasing)/; /[palpable](#palpable)/
-- [@Parents@]: /[phrasing](#phrasing)/
-- [@Children@]: /[phrasing](#phrasing)/
-- [@Attributes@]: /[globals]("Html.Attributes#globals")/
-- [@Example@]:
--
-- >>> b [] []
-- <b></b>
b :: [Attribute] -> [Html] -> Html
b = ParentNode "<b" "</b>"
{-# INLINE b #-}


-- | Generates an HTML __@\<base\>@__ element with the given attributes.
--
-- [@Description@]: Base URL and default target navigable for hyperlinks and forms
-- [@Categories@]: /[metadata](#metadata)/
-- [@Parents@]: 'Html.head'
-- [@Children@]: empty
-- [@Attributes@]: /[globals]("Html.Attributes#globals")/; 'Html.Attributes.href'; 'Html.Attributes.target'
-- [@Example@]:
--
-- >>> base []
-- <base>
base :: [Attribute] -> Html
base = LeafNode "<base"
{-# INLINE base #-}


-- | Generates an HTML __@\<bdi\>@__ element with the given attributes and contents.
--
-- [@Description@]: Text directionality isolation
-- [@Categories@]: /[flow](#flow)/; /[phrasing](#phrasing)/; /[palpable](#palpable)/
-- [@Parents@]: /[phrasing](#phrasing)/
-- [@Children@]: /[phrasing](#phrasing)/
-- [@Attributes@]: /[globals]("Html.Attributes#globals")/
-- [@Example@]:
--
-- >>> bdi [] []
-- <bdi></bdi>
bdi :: [Attribute] -> [Html] -> Html
bdi = ParentNode "<bdi" "</bdi>"
{-# INLINE bdi #-}


-- | Generates an HTML __@\<bdo\>@__ element with the given attributes and contents.
--
-- [@Description@]: Text directionality formatting
-- [@Categories@]: /[flow](#flow)/; /[phrasing](#phrasing)/; /[palpable](#palpable)/
-- [@Parents@]: /[phrasing](#phrasing)/
-- [@Children@]: /[phrasing](#phrasing)/
-- [@Attributes@]: /[globals]("Html.Attributes#globals")/
-- [@Example@]:
--
-- >>> bdo [] []
-- <bdo></bdo>
bdo :: [Attribute] -> [Html] -> Html
bdo = ParentNode "<bdo" "</bdo>"
{-# INLINE bdo #-}


-- | Generates an HTML __@\<blockquote\>@__ element with the given attributes and contents.
--
-- [@Description@]: A section quoted from another source
-- [@Categories@]: /[flow](#flow)/; /[palpable](#palpable)/
-- [@Parents@]: /[flow](#flow)/
-- [@Children@]: /[flow](#flow)/
-- [@Attributes@]: /[globals]("Html.Attributes#globals")/; 'Html.Attributes.cite'
-- [@Example@]:
--
-- >>> blockquote [] []
-- <blockquote></blockquote>
blockquote :: [Attribute] -> [Html] -> Html
blockquote = ParentNode "<blockquote" "</blockquote>"
{-# INLINE blockquote #-}


-- | Generates an HTML __@\<body\>@__ element with the given attributes and contents.
--
-- [@Description@]: Document body
-- [@Categories@]: none
-- [@Parents@]: 'Html.html'
-- [@Children@]: /[flow](#flow)/
-- [@Attributes@]: /[globals]("Html.Attributes#globals")/; 'Html.Events.onafterprint'; 'Html.Events.onbeforeprint'; 'Html.Events.onbeforeunload'; 'Html.Events.onhashchange'; 'Html.Events.onlanguagechange'; 'Html.Events.onmessage'; 'Html.Events.onmessageerror'; 'Html.Events.onoffline'; 'Html.Events.ononline'; 'Html.Events.onpageswap'; 'Html.Events.onpagehide'; 'Html.Events.onpagereveal'; 'Html.Events.onpageshow'; 'Html.Events.onpopstate'; 'Html.Events.onrejectionhandled'; 'Html.Events.onstorage'; 'Html.Events.onunhandledrejection'; 'Html.Events.onunload'
-- [@Example@]:
--
-- >>> body [] []
-- <body></body>
body :: [Attribute] -> [Html] -> Html
body = ParentNode "<body" "</body>"
{-# INLINE body #-}


-- | Generates an HTML __@\<br\>@__ element with the given attributes.
--
-- [@Description@]: Line break, e.g. in poem or postal address
-- [@Categories@]: /[flow](#flow)/; /[phrasing](#phrasing)/
-- [@Parents@]: /[phrasing](#phrasing)/
-- [@Children@]: empty
-- [@Attributes@]: /[globals]("Html.Attributes#globals")/
-- [@Example@]:
--
-- >>> br []
-- <br>
br :: [Attribute] -> Html
br = LeafNode "<br"
{-# INLINE br #-}


-- | Generates an HTML __@\<button\>@__ element with the given attributes and contents.
--
-- [@Description@]: Button control
-- [@Categories@]: /[flow](#flow)/; /[phrasing](#phrasing)/; /[interactive](#interactive)/; /[listed](#listed)/; /[labelable](#labelable)/; /[submittable](#submittable)/; /[form-associated](#form-associated)/; /[palpable](#palpable)/
-- [@Parents@]: /[phrasing](#phrasing)/
-- [@Children@]: /[phrasing](#phrasing)/
-- [@Attributes@]: /[globals]("Html.Attributes#globals")/; 'Html.Attributes.disabled'; 'Html.Attributes.form'; 'Html.Attributes.formaction'; 'Html.Attributes.formenctype'; 'Html.Attributes.formmethod'; 'Html.Attributes.formnovalidate'; 'Html.Attributes.formtarget'; 'Html.Attributes.name'; 'Html.Attributes.popovertarget'; 'Html.Attributes.popovertargetaction'; 'Html.Attributes.type_'; 'Html.Attributes.value'
-- [@Example@]:
--
-- >>> button [] []
-- <button></button>
button :: [Attribute] -> [Html] -> Html
button = ParentNode "<button" "</button>"
{-# INLINE button #-}


-- | Generates an HTML __@\<canvas\>@__ element with the given attributes and contents.
--
-- [@Description@]: Scriptable bitmap canvas
-- [@Categories@]: /[flow](#flow)/; /[phrasing](#phrasing)/; /[embedded](#embedded)/; /[palpable](#palpable)/
-- [@Parents@]: /[phrasing](#phrasing)/
-- [@Children@]: transparent
-- [@Attributes@]: /[globals]("Html.Attributes#globals")/; 'Html.Attributes.width'; 'Html.Attributes.height'
-- [@Example@]:
--
-- >>> canvas [] []
-- <canvas></canvas>
canvas :: [Attribute] -> [Html] -> Html
canvas = ParentNode "<canvas" "</canvas>"
{-# INLINE canvas #-}


-- | Generates an HTML __@\<caption\>@__ element with the given attributes and contents.
--
-- [@Description@]: Table caption
-- [@Categories@]: none
-- [@Parents@]: 'Html.table'
-- [@Children@]: /[flow](#flow)/
-- [@Attributes@]: /[globals]("Html.Attributes#globals")/
-- [@Example@]:
--
-- >>> caption [] []
-- <caption></caption>
caption :: [Attribute] -> [Html] -> Html
caption = ParentNode "<caption" "</caption>"
{-# INLINE caption #-}


-- | Generates an HTML __@\<cite\>@__ element with the given attributes and contents.
--
-- [@Description@]: Title of a work
-- [@Categories@]: /[flow](#flow)/; /[phrasing](#phrasing)/; /[palpable](#palpable)/
-- [@Parents@]: /[phrasing](#phrasing)/
-- [@Children@]: /[phrasing](#phrasing)/
-- [@Attributes@]: /[globals]("Html.Attributes#globals")/
-- [@Example@]:
--
-- >>> cite [] []
-- <cite></cite>
--
-- /Note: This element collides with the 'Html.Attributes.cite' attribute./
cite :: [Attribute] -> [Html] -> Html
cite = ParentNode "<cite" "</cite>"
{-# INLINE cite #-}


-- | Generates an HTML __@\<code\>@__ element with the given attributes and contents.
--
-- [@Description@]: Computer code
-- [@Categories@]: /[flow](#flow)/; /[phrasing](#phrasing)/; /[palpable](#palpable)/
-- [@Parents@]: /[phrasing](#phrasing)/
-- [@Children@]: /[phrasing](#phrasing)/
-- [@Attributes@]: /[globals]("Html.Attributes#globals")/
-- [@Example@]:
--
-- >>> code [] []
-- <code></code>
code :: [Attribute] -> [Html] -> Html
code = ParentNode "<code" "</code>"
{-# INLINE code #-}


-- | Generates an HTML __@\<col\>@__ element with the given attributes.
--
-- [@Description@]: Table column
-- [@Categories@]: none
-- [@Parents@]: 'Html.colgroup'
-- [@Children@]: empty
-- [@Attributes@]: /[globals]("Html.Attributes#globals")/; 'Html.Attributes.span'
-- [@Example@]:
--
-- >>> col []
-- <col>
col :: [Attribute] -> Html
col = LeafNode "<col"
{-# INLINE col #-}


-- | Generates an HTML __@\<colgroup\>@__ element with the given attributes and contents.
--
-- [@Description@]: Group of columns in a table
-- [@Categories@]: none
-- [@Parents@]: 'Html.table'
-- [@Children@]: 'Html.col'; 'Html.template'
-- [@Attributes@]: /[globals]("Html.Attributes#globals")/; 'Html.Attributes.span'
-- [@Example@]:
--
-- >>> colgroup [] []
-- <colgroup></colgroup>
colgroup :: [Attribute] -> [Html] -> Html
colgroup = ParentNode "<colgroup" "</colgroup>"
{-# INLINE colgroup #-}


-- | Generates an HTML __@\<data\>@__ element with the given attributes and contents.
--
-- [@Description@]: Machine-readable equivalent
-- [@Categories@]: /[flow](#flow)/; /[phrasing](#phrasing)/; /[palpable](#palpable)/
-- [@Parents@]: /[phrasing](#phrasing)/
-- [@Children@]: /[phrasing](#phrasing)/
-- [@Attributes@]: /[globals]("Html.Attributes#globals")/; 'Html.Attributes.value'
-- [@Example@]:
--
-- >>> data_ [] []
-- <data></data>
--
-- /Note: This element collides with the 'Html.Attributes.data_' attribute./
data_ :: [Attribute] -> [Html] -> Html
data_ = ParentNode "<data" "</data>"
{-# INLINE data_ #-}


-- | Generates an HTML __@\<datalist\>@__ element with the given attributes and contents.
--
-- [@Description@]: Container for options for combo box control
-- [@Categories@]: /[flow](#flow)/; /[phrasing](#phrasing)/
-- [@Parents@]: /[phrasing](#phrasing)/
-- [@Children@]: /[phrasing](#phrasing)/; 'Html.option'; /[script-supporting](#script-supporting)/
-- [@Attributes@]: /[globals]("Html.Attributes#globals")/
-- [@Example@]:
--
-- >>> datalist [] []
-- <datalist></datalist>
datalist :: [Attribute] -> [Html] -> Html
datalist = ParentNode "<datalist" "</datalist>"
{-# INLINE datalist #-}


-- | Generates an HTML __@\<dd\>@__ element with the given attributes and contents.
--
-- [@Description@]: Content for corresponding 'Html.dt' element(s)
-- [@Categories@]: none
-- [@Parents@]: 'Html.dl'; 'Html.div'
-- [@Children@]: /[flow](#flow)/
-- [@Attributes@]: /[globals]("Html.Attributes#globals")/
-- [@Example@]:
--
-- >>> dd [] []
-- <dd></dd>
dd :: [Attribute] -> [Html] -> Html
dd = ParentNode "<dd" "</dd>"
{-# INLINE dd #-}


-- | Generates an HTML __@\<del\>@__ element with the given attributes and contents.
--
-- [@Description@]: A removal from the document
-- [@Categories@]: /[flow](#flow)/; /[phrasing](#phrasing)/; /[palpable](#palpable)/
-- [@Parents@]: /[phrasing](#phrasing)/
-- [@Children@]: transparent
-- [@Attributes@]: /[globals]("Html.Attributes#globals")/; 'Html.Attributes.cite'; 'Html.Attributes.datetime'
-- [@Example@]:
--
-- >>> del [] []
-- <del></del>
del :: [Attribute] -> [Html] -> Html
del = ParentNode "<del" "</del>"
{-# INLINE del #-}


-- | Generates an HTML __@\<details\>@__ element with the given attributes and contents.
--
-- [@Description@]: Disclosure control for hiding details
-- [@Categories@]: /[flow](#flow)/; /[interactive](#interactive)/; /[palpable](#palpable)/
-- [@Parents@]: /[flow](#flow)/
-- [@Children@]: 'Html.summary'; /[flow](#flow)/
-- [@Attributes@]: /[globals]("Html.Attributes#globals")/; 'Html.Attributes.name'; 'Html.Attributes.open'
-- [@Example@]:
--
-- >>> details [] []
-- <details></details>
details :: [Attribute] -> [Html] -> Html
details = ParentNode "<details" "</details>"
{-# INLINE details #-}


-- | Generates an HTML __@\<dfn\>@__ element with the given attributes and contents.
--
-- [@Description@]: Defining instance
-- [@Categories@]: /[flow](#flow)/; /[phrasing](#phrasing)/; /[palpable](#palpable)/
-- [@Parents@]: /[phrasing](#phrasing)/
-- [@Children@]: /[phrasing](#phrasing)/
-- [@Attributes@]: /[globals]("Html.Attributes#globals")/
-- [@Example@]:
--
-- >>> dfn [] []
-- <dfn></dfn>
dfn :: [Attribute] -> [Html] -> Html
dfn = ParentNode "<dfn" "</dfn>"
{-# INLINE dfn #-}


-- | Generates an HTML __@\<dialog\>@__ element with the given attributes and contents.
--
-- [@Description@]: Dialog box or window
-- [@Categories@]: /[flow](#flow)/
-- [@Parents@]: /[flow](#flow)/
-- [@Children@]: /[flow](#flow)/
-- [@Attributes@]: /[globals]("Html.Attributes#globals")/; 'Html.Attributes.open'
-- [@Example@]:
--
-- >>> dialog [] []
-- <dialog></dialog>
dialog :: [Attribute] -> [Html] -> Html
dialog = ParentNode "<dialog" "</dialog>"
{-# INLINE dialog #-}


-- | Generates an HTML __@\<div\>@__ element with the given attributes and contents.
--
-- [@Description@]: Generic flow container, or container for name-value groups in 'Html.dl' elements
-- [@Categories@]: /[flow](#flow)/; /[palpable](#palpable)/
-- [@Parents@]: /[flow](#flow)/; 'Html.dl'
-- [@Children@]: /[flow](#flow)/
-- [@Attributes@]: /[globals]("Html.Attributes#globals")/
-- [@Example@]:
--
-- >>> div [] []
-- <div></div>
div :: [Attribute] -> [Html] -> Html
div = ParentNode "<div" "</div>"
{-# INLINE div #-}


-- | Generates an HTML __@\<dl\>@__ element with the given attributes and contents.
--
-- [@Description@]: Association list consisting of zero or more name-value groups
-- [@Categories@]: /[flow](#flow)/; /[palpable](#palpable)/
-- [@Parents@]: /[flow](#flow)/
-- [@Children@]: 'Html.dt'; 'Html.dd'; 'Html.div'; /[script-supporting](#script-supporting)/
-- [@Attributes@]: /[globals]("Html.Attributes#globals")/
-- [@Example@]:
--
-- >>> dl [] []
-- <dl></dl>
dl :: [Attribute] -> [Html] -> Html
dl = ParentNode "<dl" "</dl>"
{-# INLINE dl #-}


-- | Generates an HTML __@\<dt\>@__ element with the given attributes and contents.
--
-- [@Description@]: Legend for corresponding 'Html.dd' element(s)
-- [@Categories@]: none
-- [@Parents@]: 'Html.dl'; 'Html.div'
-- [@Children@]: /[flow](#flow)/
-- [@Attributes@]: /[globals]("Html.Attributes#globals")/
-- [@Example@]:
--
-- >>> dt [] []
-- <dt></dt>
dt :: [Attribute] -> [Html] -> Html
dt = ParentNode "<dt" "</dt>"
{-# INLINE dt #-}


-- | Generates an HTML __@\<em\>@__ element with the given attributes and contents.
--
-- [@Description@]: Stress emphasis
-- [@Categories@]: /[flow](#flow)/; /[phrasing](#phrasing)/; /[palpable](#palpable)/
-- [@Parents@]: /[phrasing](#phrasing)/
-- [@Children@]: /[phrasing](#phrasing)/
-- [@Attributes@]: /[globals]("Html.Attributes#globals")/
-- [@Example@]:
--
-- >>> em [] []
-- <em></em>
em :: [Attribute] -> [Html] -> Html
em = ParentNode "<em" "</em>"
{-# INLINE em #-}


-- | Generates an HTML __@\<embed\>@__ element with the given attributes.
--
-- [@Description@]: Plugin
-- [@Categories@]: /[flow](#flow)/; /[phrasing](#phrasing)/; /[embedded](#embedded)/; /[interactive](#interactive)/; /[palpable](#palpable)/
-- [@Parents@]: /[phrasing](#phrasing)/
-- [@Children@]: none
-- [@Attributes@]: /[globals]("Html.Attributes#globals")/; 'Html.Attributes.src'; 'Html.Attributes.type_'; 'Html.Attributes.width'; 'Html.Attributes.height'; any
-- [@Example@]:
--
-- >>> embed []
-- <embed>
embed :: [Attribute] -> Html
embed = LeafNode "<embed"
{-# INLINE embed #-}


-- | Generates an HTML __@\<fieldset\>@__ element with the given attributes and contents.
--
-- [@Description@]: Group of form controls
-- [@Categories@]: /[flow](#flow)/; /[listed](#listed)/; /[form-associated](#form-associated)/; /[palpable](#palpable)/
-- [@Parents@]: /[flow](#flow)/
-- [@Children@]: 'Html.legend'; /[flow](#flow)/
-- [@Attributes@]: /[globals]("Html.Attributes#globals")/; 'Html.Attributes.disabled'; 'Html.Attributes.form'; 'Html.Attributes.name'
-- [@Example@]:
--
-- >>> fieldset [] []
-- <fieldset></fieldset>
fieldset :: [Attribute] -> [Html] -> Html
fieldset = ParentNode "<fieldset" "</fieldset>"
{-# INLINE fieldset #-}


-- | Generates an HTML __@\<figcaption\>@__ element with the given attributes and contents.
--
-- [@Description@]: Caption for 'Html.figure'
-- [@Categories@]: none
-- [@Parents@]: 'Html.figure'
-- [@Children@]: /[flow](#flow)/
-- [@Attributes@]: /[globals]("Html.Attributes#globals")/
-- [@Example@]:
--
-- >>> figcaption [] []
-- <figcaption></figcaption>
figcaption :: [Attribute] -> [Html] -> Html
figcaption = ParentNode "<figcaption" "</figcaption>"
{-# INLINE figcaption #-}


-- | Generates an HTML __@\<figure\>@__ element with the given attributes and contents.
--
-- [@Description@]: Figure with optional caption
-- [@Categories@]: /[flow](#flow)/; /[palpable](#palpable)/
-- [@Parents@]: /[flow](#flow)/
-- [@Children@]: 'Html.figcaption'; /[flow](#flow)/
-- [@Attributes@]: /[globals]("Html.Attributes#globals")/
-- [@Example@]:
--
-- >>> figure [] []
-- <figure></figure>
figure :: [Attribute] -> [Html] -> Html
figure = ParentNode "<figure" "</figure>"
{-# INLINE figure #-}


-- | Generates an HTML __@\<footer\>@__ element with the given attributes and contents.
--
-- [@Description@]: Footer for a page or section
-- [@Categories@]: /[flow](#flow)/; /[palpable](#palpable)/
-- [@Parents@]: /[flow](#flow)/
-- [@Children@]: /[flow](#flow)/
-- [@Attributes@]: /[globals]("Html.Attributes#globals")/
-- [@Example@]:
--
-- >>> footer [] []
-- <footer></footer>
footer :: [Attribute] -> [Html] -> Html
footer = ParentNode "<footer" "</footer>"
{-# INLINE footer #-}


-- | Generates an HTML __@\<form\>@__ element with the given attributes and contents.
--
-- [@Description@]: User-submittable form
-- [@Categories@]: /[flow](#flow)/; /[palpable](#palpable)/
-- [@Parents@]: /[flow](#flow)/
-- [@Children@]: /[flow](#flow)/
-- [@Attributes@]: /[globals]("Html.Attributes#globals")/; 'Html.Attributes.acceptCharset'; 'Html.Attributes.action'; 'Html.Attributes.autocomplete'; 'Html.Attributes.enctype'; 'Html.Attributes.method'; 'Html.Attributes.name'; 'Html.Attributes.novalidate'; 'Html.Attributes.rel'; 'Html.Attributes.target'
-- [@Example@]:
--
-- >>> form [] []
-- <form></form>
--
-- /Note: This element collides with the 'Html.Attributes.form' attribute./
form :: [Attribute] -> [Html] -> Html
form = ParentNode "<form" "</form>"
{-# INLINE form #-}


-- | Generates an HTML __@\<h1\>@__ element with the given attributes and contents.
--
-- [@Description@]: Heading
-- [@Categories@]: /[flow](#flow)/; /[heading](#heading)/; /[palpable](#palpable)/
-- [@Parents@]: 'Html.legend'; 'Html.summary'; /[flow](#flow)/
-- [@Children@]: /[phrasing](#phrasing)/
-- [@Attributes@]: /[globals]("Html.Attributes#globals")/
-- [@Example@]:
--
-- >>> h1 [] []
-- <h1></h1>
h1 :: [Attribute] -> [Html] -> Html
h1 = ParentNode "<h1" "</h1>"
{-# INLINE h1 #-}


-- | Generates an HTML __@\<h2\>@__ element with the given attributes and contents.
--
-- [@Description@]: Heading
-- [@Categories@]: /[flow](#flow)/; /[heading](#heading)/; /[palpable](#palpable)/
-- [@Parents@]: 'Html.legend'; 'Html.summary'; /[flow](#flow)/
-- [@Children@]: /[phrasing](#phrasing)/
-- [@Attributes@]: /[globals]("Html.Attributes#globals")/
-- [@Example@]:
--
-- >>> h2 [] []
-- <h2></h2>
h2 :: [Attribute] -> [Html] -> Html
h2 = ParentNode "<h2" "</h2>"
{-# INLINE h2 #-}


-- | Generates an HTML __@\<h3\>@__ element with the given attributes and contents.
--
-- [@Description@]: Heading
-- [@Categories@]: /[flow](#flow)/; /[heading](#heading)/; /[palpable](#palpable)/
-- [@Parents@]: 'Html.legend'; 'Html.summary'; /[flow](#flow)/
-- [@Children@]: /[phrasing](#phrasing)/
-- [@Attributes@]: /[globals]("Html.Attributes#globals")/
-- [@Example@]:
--
-- >>> h3 [] []
-- <h3></h3>
h3 :: [Attribute] -> [Html] -> Html
h3 = ParentNode "<h3" "</h3>"
{-# INLINE h3 #-}


-- | Generates an HTML __@\<h4\>@__ element with the given attributes and contents.
--
-- [@Description@]: Heading
-- [@Categories@]: /[flow](#flow)/; /[heading](#heading)/; /[palpable](#palpable)/
-- [@Parents@]: 'Html.legend'; 'Html.summary'; /[flow](#flow)/
-- [@Children@]: /[phrasing](#phrasing)/
-- [@Attributes@]: /[globals]("Html.Attributes#globals")/
-- [@Example@]:
--
-- >>> h4 [] []
-- <h4></h4>
h4 :: [Attribute] -> [Html] -> Html
h4 = ParentNode "<h4" "</h4>"
{-# INLINE h4 #-}


-- | Generates an HTML __@\<h5\>@__ element with the given attributes and contents.
--
-- [@Description@]: Heading
-- [@Categories@]: /[flow](#flow)/; /[heading](#heading)/; /[palpable](#palpable)/
-- [@Parents@]: 'Html.legend'; 'Html.summary'; /[flow](#flow)/
-- [@Children@]: /[phrasing](#phrasing)/
-- [@Attributes@]: /[globals]("Html.Attributes#globals")/
-- [@Example@]:
--
-- >>> h5 [] []
-- <h5></h5>
h5 :: [Attribute] -> [Html] -> Html
h5 = ParentNode "<h5" "</h5>"
{-# INLINE h5 #-}


-- | Generates an HTML __@\<h6\>@__ element with the given attributes and contents.
--
-- [@Description@]: Heading
-- [@Categories@]: /[flow](#flow)/; /[heading](#heading)/; /[palpable](#palpable)/
-- [@Parents@]: 'Html.legend'; 'Html.summary'; /[flow](#flow)/
-- [@Children@]: /[phrasing](#phrasing)/
-- [@Attributes@]: /[globals]("Html.Attributes#globals")/
-- [@Example@]:
--
-- >>> h6 [] []
-- <h6></h6>
h6 :: [Attribute] -> [Html] -> Html
h6 = ParentNode "<h6" "</h6>"
{-# INLINE h6 #-}


-- | Generates an HTML __@\<head\>@__ element with the given attributes and contents.
--
-- [@Description@]: Container for document metadata
-- [@Categories@]: none
-- [@Parents@]: 'Html.html'
-- [@Children@]: /[metadata](#metadata)/
-- [@Attributes@]: /[globals]("Html.Attributes#globals")/
-- [@Example@]:
--
-- >>> head [] []
-- <head></head>
head :: [Attribute] -> [Html] -> Html
head = ParentNode "<head" "</head>"
{-# INLINE head #-}


-- | Generates an HTML __@\<header\>@__ element with the given attributes and contents.
--
-- [@Description@]: Introductory or navigational aids for a page or section
-- [@Categories@]: /[flow](#flow)/; /[palpable](#palpable)/
-- [@Parents@]: /[flow](#flow)/
-- [@Children@]: /[flow](#flow)/
-- [@Attributes@]: /[globals]("Html.Attributes#globals")/
-- [@Example@]:
--
-- >>> header [] []
-- <header></header>
header :: [Attribute] -> [Html] -> Html
header = ParentNode "<header" "</header>"
{-# INLINE header #-}


-- | Generates an HTML __@\<hgroup\>@__ element with the given attributes and contents.
--
-- [@Description@]: Heading container
-- [@Categories@]: /[flow](#flow)/; /[palpable](#palpable)/
-- [@Parents@]: 'Html.legend'; 'Html.summary'; /[flow](#flow)/
-- [@Children@]: 'Html.h1'; 'Html.h2'; 'Html.h3'; 'Html.h4'; 'Html.h5'; 'Html.h6'; /[script-supporting](#script-supporting)/
-- [@Attributes@]: /[globals]("Html.Attributes#globals")/
-- [@Example@]:
--
-- >>> hgroup [] []
-- <hgroup></hgroup>
hgroup :: [Attribute] -> [Html] -> Html
hgroup = ParentNode "<hgroup" "</hgroup>"
{-# INLINE hgroup #-}


-- | Generates an HTML __@\<hr\>@__ element with the given attributes.
--
-- [@Description@]: Thematic break
-- [@Categories@]: /[flow](#flow)/
-- [@Parents@]: /[flow](#flow)/
-- [@Children@]: empty
-- [@Attributes@]: /[globals]("Html.Attributes#globals")/
-- [@Example@]:
--
-- >>> hr []
-- <hr>
hr :: [Attribute] -> Html
hr = LeafNode "<hr"
{-# INLINE hr #-}


-- | Generates an HTML __@\<html\>@__ element with the given attributes and contents.
--
-- [@Description@]: Root element
-- [@Categories@]: none
-- [@Parents@]: none
-- [@Children@]: 'Html.head'; 'Html.body'
-- [@Attributes@]: /[globals]("Html.Attributes#globals")/
-- [@Example@]:
--
-- >>> html [] []
-- <html></html>
html :: [Attribute] -> [Html] -> Html
html = ParentNode "<html" "</html>"
{-# INLINE html #-}


-- | Generates an HTML __@\<i\>@__ element with the given attributes and contents.
--
-- [@Description@]: Alternate voice
-- [@Categories@]: /[flow](#flow)/; /[phrasing](#phrasing)/; /[palpable](#palpable)/
-- [@Parents@]: /[phrasing](#phrasing)/
-- [@Children@]: /[phrasing](#phrasing)/
-- [@Attributes@]: /[globals]("Html.Attributes#globals")/
-- [@Example@]:
--
-- >>> i [] []
-- <i></i>
i :: [Attribute] -> [Html] -> Html
i = ParentNode "<i" "</i>"
{-# INLINE i #-}


-- | Generates an HTML __@\<iframe\>@__ element with the given attributes and contents.
--
-- [@Description@]: Child navigable
-- [@Categories@]: /[flow](#flow)/; /[phrasing](#phrasing)/; /[embedded](#embedded)/; /[interactive](#interactive)/; /[palpable](#palpable)/
-- [@Parents@]: /[phrasing](#phrasing)/
-- [@Children@]: empty
-- [@Attributes@]: /[globals]("Html.Attributes#globals")/; 'Html.Attributes.src'; 'Html.Attributes.srcdoc'; 'Html.Attributes.name'; 'Html.Attributes.sandbox'; 'Html.Attributes.allow'; 'Html.Attributes.allowfullscreen'; 'Html.Attributes.width'; 'Html.Attributes.height'; 'Html.Attributes.referrerpolicy'; 'Html.Attributes.loading'
-- [@Example@]:
--
-- >>> iframe [] []
-- <iframe></iframe>
iframe :: [Attribute] -> [Html] -> Html
iframe = ParentNode "<iframe" "</iframe>"
{-# INLINE iframe #-}


-- | Generates an HTML __@\<img\>@__ element with the given attributes.
--
-- [@Description@]: Image
-- [@Categories@]: /[flow](#flow)/; /[phrasing](#phrasing)/; /[embedded](#embedded)/; /[interactive](#interactive)/; /[form-associated](#form-associated)/; /[palpable](#palpable)/
-- [@Parents@]: /[phrasing](#phrasing)/; 'Html.picture'
-- [@Children@]: empty
-- [@Attributes@]: /[globals]("Html.Attributes#globals")/; 'Html.Attributes.alt'; 'Html.Attributes.src'; 'Html.Attributes.srcset'; 'Html.Attributes.sizes'; 'Html.Attributes.crossorigin'; 'Html.Attributes.usemap'; 'Html.Attributes.ismap'; 'Html.Attributes.width'; 'Html.Attributes.height'; 'Html.Attributes.referrerpolicy'; 'Html.Attributes.decoding'; 'Html.Attributes.loading'; 'Html.Attributes.fetchpriority'
-- [@Example@]:
--
-- >>> img []
-- <img>
img :: [Attribute] -> Html
img = LeafNode "<img"
{-# INLINE img #-}


-- | Generates an HTML __@\<input\>@__ element with the given attributes.
--
-- [@Description@]: Form control
-- [@Categories@]: /[flow](#flow)/; /[phrasing](#phrasing)/; /[interactive](#interactive)/; /[listed](#listed)/; /[labelable](#labelable)/; /[submittable](#submittable)/; /[resettable](#resettable)/; /[form-associated](#form-associated)/; /[palpable](#palpable)/
-- [@Parents@]: /[phrasing](#phrasing)/
-- [@Children@]: empty
-- [@Attributes@]: /[globals]("Html.Attributes#globals")/; 'Html.Attributes.accept'; 'Html.Attributes.alpha'; 'Html.Attributes.alt'; 'Html.Attributes.autocomplete'; 'Html.Attributes.checked'; 'Html.Attributes.colorspace'; 'Html.Attributes.dirname'; 'Html.Attributes.disabled'; 'Html.Attributes.form'; 'Html.Attributes.formaction'; 'Html.Attributes.formenctype'; 'Html.Attributes.formmethod'; 'Html.Attributes.formnovalidate'; 'Html.Attributes.formtarget'; 'Html.Attributes.height'; 'Html.Attributes.list'; 'Html.Attributes.max'; 'Html.Attributes.maxlength'; 'Html.Attributes.min'; 'Html.Attributes.minlength'; 'Html.Attributes.multiple'; 'Html.Attributes.name'; 'Html.Attributes.pattern'; 'Html.Attributes.placeholder'; 'Html.Attributes.popovertarget'; 'Html.Attributes.popovertargetaction'; 'Html.Attributes.readonly'; 'Html.Attributes.required'; 'Html.Attributes.size'; 'Html.Attributes.src'; 'Html.Attributes.step'; 'Html.Attributes.type_'; 'Html.Attributes.value'; 'Html.Attributes.width'
-- [@Example@]:
--
-- >>> input []
-- <input>
input :: [Attribute] -> Html
input = LeafNode "<input"
{-# INLINE input #-}


-- | Generates an HTML __@\<ins\>@__ element with the given attributes and contents.
--
-- [@Description@]: An addition to the document
-- [@Categories@]: /[flow](#flow)/; /[phrasing](#phrasing)/; /[palpable](#palpable)/
-- [@Parents@]: /[phrasing](#phrasing)/
-- [@Children@]: transparent
-- [@Attributes@]: /[globals]("Html.Attributes#globals")/; 'Html.Attributes.cite'; 'Html.Attributes.datetime'
-- [@Example@]:
--
-- >>> ins [] []
-- <ins></ins>
ins :: [Attribute] -> [Html] -> Html
ins = ParentNode "<ins" "</ins>"
{-# INLINE ins #-}


-- | Generates an HTML __@\<kbd\>@__ element with the given attributes and contents.
--
-- [@Description@]: User input
-- [@Categories@]: /[flow](#flow)/; /[phrasing](#phrasing)/; /[palpable](#palpable)/
-- [@Parents@]: /[phrasing](#phrasing)/
-- [@Children@]: /[phrasing](#phrasing)/
-- [@Attributes@]: /[globals]("Html.Attributes#globals")/
-- [@Example@]:
--
-- >>> kbd [] []
-- <kbd></kbd>
kbd :: [Attribute] -> [Html] -> Html
kbd = ParentNode "<kbd" "</kbd>"
{-# INLINE kbd #-}


-- | Generates an HTML __@\<label\>@__ element with the given attributes and contents.
--
-- [@Description@]: Caption for a form control
-- [@Categories@]: /[flow](#flow)/; /[phrasing](#phrasing)/; /[interactive](#interactive)/; /[palpable](#palpable)/
-- [@Parents@]: /[phrasing](#phrasing)/
-- [@Children@]: /[phrasing](#phrasing)/
-- [@Attributes@]: /[globals]("Html.Attributes#globals")/; 'Html.Attributes.for'
-- [@Example@]:
--
-- >>> label [] []
-- <label></label>
--
-- /Note: This element collides with the 'Html.Attributes.label' attribute./
label :: [Attribute] -> [Html] -> Html
label = ParentNode "<label" "</label>"
{-# INLINE label #-}


-- | Generates an HTML __@\<legend\>@__ element with the given attributes and contents.
--
-- [@Description@]: Caption for 'Html.fieldset'
-- [@Categories@]: none
-- [@Parents@]: 'Html.fieldset'
-- [@Children@]: /[phrasing](#phrasing)/; /[heading](#heading)/
-- [@Attributes@]: /[globals]("Html.Attributes#globals")/
-- [@Example@]:
--
-- >>> legend [] []
-- <legend></legend>
legend :: [Attribute] -> [Html] -> Html
legend = ParentNode "<legend" "</legend>"
{-# INLINE legend #-}


-- | Generates an HTML __@\<li\>@__ element with the given attributes and contents.
--
-- [@Description@]: List item
-- [@Categories@]: none
-- [@Parents@]: 'Html.ol'; 'Html.ul'; 'Html.menu'
-- [@Children@]: /[flow](#flow)/
-- [@Attributes@]: /[globals]("Html.Attributes#globals")/; 'Html.Attributes.value'
-- [@Example@]:
--
-- >>> li [] []
-- <li></li>
li :: [Attribute] -> [Html] -> Html
li = ParentNode "<li" "</li>"
{-# INLINE li #-}


-- | Generates an HTML __@\<link\>@__ element with the given attributes.
--
-- [@Description@]: Link metadata
-- [@Categories@]: /[metadata](#metadata)/; /[flow](#flow)/; /[phrasing](#phrasing)/
-- [@Parents@]: 'Html.head'; 'Html.noscript'; /[phrasing](#phrasing)/
-- [@Children@]: empty
-- [@Attributes@]: /[globals]("Html.Attributes#globals")/; 'Html.Attributes.href'; 'Html.Attributes.crossorigin'; 'Html.Attributes.rel'; 'Html.Attributes.as'; 'Html.Attributes.media'; 'Html.Attributes.hreflang'; 'Html.Attributes.type_'; 'Html.Attributes.sizes'; 'Html.Attributes.imagesrcset'; 'Html.Attributes.imagesizes'; 'Html.Attributes.referrerpolicy'; 'Html.Attributes.integrity'; 'Html.Attributes.blocking'; 'Html.Attributes.color'; 'Html.Attributes.disabled'; 'Html.Attributes.fetchpriority'
-- [@Example@]:
--
-- >>> link []
-- <link>
link :: [Attribute] -> Html
link = LeafNode "<link"
{-# INLINE link #-}


-- | Generates an HTML __@\<main\>@__ element with the given attributes and contents.
--
-- [@Description@]: Container for the dominant contents of the document
-- [@Categories@]: /[flow](#flow)/; /[palpable](#palpable)/
-- [@Parents@]: /[flow](#flow)/
-- [@Children@]: /[flow](#flow)/
-- [@Attributes@]: /[globals]("Html.Attributes#globals")/
-- [@Example@]:
--
-- >>> main [] []
-- <main></main>
main :: [Attribute] -> [Html] -> Html
main = ParentNode "<main" "</main>"
{-# INLINE main #-}


-- | Generates an HTML __@\<map\>@__ element with the given attributes and contents.
--
-- [@Description@]: Image map
-- [@Categories@]: /[flow](#flow)/; /[phrasing](#phrasing)/; /[palpable](#palpable)/
-- [@Parents@]: /[phrasing](#phrasing)/
-- [@Children@]: transparent; 'Html.area'
-- [@Attributes@]: /[globals]("Html.Attributes#globals")/; 'Html.Attributes.name'
-- [@Example@]:
--
-- >>> map [] []
-- <map></map>
map :: [Attribute] -> [Html] -> Html
map = ParentNode "<map" "</map>"
{-# INLINE map #-}


-- | Generates an HTML __@\<mark\>@__ element with the given attributes and contents.
--
-- [@Description@]: Highlight
-- [@Categories@]: /[flow](#flow)/; /[phrasing](#phrasing)/; /[palpable](#palpable)/
-- [@Parents@]: /[phrasing](#phrasing)/
-- [@Children@]: /[phrasing](#phrasing)/
-- [@Attributes@]: /[globals]("Html.Attributes#globals")/
-- [@Example@]:
--
-- >>> mark [] []
-- <mark></mark>
mark :: [Attribute] -> [Html] -> Html
mark = ParentNode "<mark" "</mark>"
{-# INLINE mark #-}


-- | Generates an HTML __@\<menu\>@__ element with the given attributes and contents.
--
-- [@Description@]: Menu of commands
-- [@Categories@]: /[flow](#flow)/; /[palpable](#palpable)/
-- [@Parents@]: /[flow](#flow)/
-- [@Children@]: 'Html.li'; /[script-supporting](#script-supporting)/
-- [@Attributes@]: /[globals]("Html.Attributes#globals")/
-- [@Example@]:
--
-- >>> menu [] []
-- <menu></menu>
menu :: [Attribute] -> [Html] -> Html
menu = ParentNode "<menu" "</menu>"
{-# INLINE menu #-}


-- | Generates an HTML __@\<meta\>@__ element with the given attributes.
--
-- [@Description@]: Text metadata
-- [@Categories@]: /[metadata](#metadata)/; /[flow](#flow)/; /[phrasing](#phrasing)/
-- [@Parents@]: 'Html.head'; 'Html.noscript'; /[phrasing](#phrasing)/
-- [@Children@]: empty
-- [@Attributes@]: /[globals]("Html.Attributes#globals")/; 'Html.Attributes.name'; 'Html.Attributes.httpEquiv'; 'Html.Attributes.content'; 'Html.Attributes.charset'; 'Html.Attributes.media'
-- [@Example@]:
--
-- >>> meta []
-- <meta>
meta :: [Attribute] -> Html
meta = LeafNode "<meta"
{-# INLINE meta #-}


-- | Generates an HTML __@\<meter\>@__ element with the given attributes and contents.
--
-- [@Description@]: Gauge
-- [@Categories@]: /[flow](#flow)/; /[phrasing](#phrasing)/; /[labelable](#labelable)/; /[palpable](#palpable)/
-- [@Parents@]: /[phrasing](#phrasing)/
-- [@Children@]: /[phrasing](#phrasing)/
-- [@Attributes@]: /[globals]("Html.Attributes#globals")/; 'Html.Attributes.value'; 'Html.Attributes.min'; 'Html.Attributes.max'; 'Html.Attributes.low'; 'Html.Attributes.high'; 'Html.Attributes.optimum'
-- [@Example@]:
--
-- >>> meter [] []
-- <meter></meter>
meter :: [Attribute] -> [Html] -> Html
meter = ParentNode "<meter" "</meter>"
{-# INLINE meter #-}


-- | Generates an HTML __@\<nav\>@__ element with the given attributes and contents.
--
-- [@Description@]: Section with navigational links
-- [@Categories@]: /[flow](#flow)/; /[sectioning](#sectioning)/; /[palpable](#palpable)/
-- [@Parents@]: /[flow](#flow)/
-- [@Children@]: /[flow](#flow)/
-- [@Attributes@]: /[globals]("Html.Attributes#globals")/
-- [@Example@]:
--
-- >>> nav [] []
-- <nav></nav>
nav :: [Attribute] -> [Html] -> Html
nav = ParentNode "<nav" "</nav>"
{-# INLINE nav #-}


-- | Generates an HTML __@\<noscript\>@__ element with the given attributes and contents.
--
-- [@Description@]: Fallback content for script
-- [@Categories@]: /[metadata](#metadata)/; /[flow](#flow)/; /[phrasing](#phrasing)/
-- [@Parents@]: 'Html.head'; /[phrasing](#phrasing)/
-- [@Children@]: varies
-- [@Attributes@]: /[globals]("Html.Attributes#globals")/
-- [@Example@]:
--
-- >>> noscript [] []
-- <noscript></noscript>
noscript :: [Attribute] -> [Html] -> Html
noscript = ParentNode "<noscript" "</noscript>"
{-# INLINE noscript #-}


-- | Generates an HTML __@\<object\>@__ element with the given attributes and contents.
--
-- [@Description@]: Image, child navigable, or plugin
-- [@Categories@]: /[flow](#flow)/; /[phrasing](#phrasing)/; /[embedded](#embedded)/; /[interactive](#interactive)/; /[listed](#listed)/; /[form-associated](#form-associated)/; /[palpable](#palpable)/
-- [@Parents@]: /[phrasing](#phrasing)/
-- [@Children@]: transparent
-- [@Attributes@]: /[globals]("Html.Attributes#globals")/; 'Html.Attributes.data_'; 'Html.Attributes.type_'; 'Html.Attributes.name'; 'Html.Attributes.form'; 'Html.Attributes.width'; 'Html.Attributes.height'
-- [@Example@]:
--
-- >>> object [] []
-- <object></object>
object :: [Attribute] -> [Html] -> Html
object = ParentNode "<object" "</object>"
{-# INLINE object #-}


-- | Generates an HTML __@\<ol\>@__ element with the given attributes and contents.
--
-- [@Description@]: Ordered list
-- [@Categories@]: /[flow](#flow)/; /[palpable](#palpable)/
-- [@Parents@]: /[flow](#flow)/
-- [@Children@]: 'Html.li'; /[script-supporting](#script-supporting)/
-- [@Attributes@]: /[globals]("Html.Attributes#globals")/; 'Html.Attributes.reversed'; 'Html.Attributes.start'; 'Html.Attributes.type_'
-- [@Example@]:
--
-- >>> ol [] []
-- <ol></ol>
ol :: [Attribute] -> [Html] -> Html
ol = ParentNode "<ol" "</ol>"
{-# INLINE ol #-}


-- | Generates an HTML __@\<optgroup\>@__ element with the given attributes and contents.
--
-- [@Description@]: Group of options in a list box
-- [@Categories@]: none
-- [@Parents@]: 'Html.select'
-- [@Children@]: 'Html.option'; /[script-supporting](#script-supporting)/
-- [@Attributes@]: /[globals]("Html.Attributes#globals")/; 'Html.Attributes.disabled'; 'Html.Attributes.label'
-- [@Example@]:
--
-- >>> optgroup [] []
-- <optgroup></optgroup>
optgroup :: [Attribute] -> [Html] -> Html
optgroup = ParentNode "<optgroup" "</optgroup>"
{-# INLINE optgroup #-}


-- | Generates an HTML __@\<option\>@__ element with the given attributes and contents.
--
-- [@Description@]: Option in a list box or combo box control
-- [@Categories@]: none
-- [@Parents@]: 'Html.select'; 'Html.datalist'; 'Html.optgroup'
-- [@Children@]: text
-- [@Attributes@]: /[globals]("Html.Attributes#globals")/; 'Html.Attributes.disabled'; 'Html.Attributes.label'; 'Html.Attributes.selected'; 'Html.Attributes.value'
-- [@Example@]:
--
-- >>> option [] []
-- <option></option>
option :: [Attribute] -> [Html] -> Html
option = ParentNode "<option" "</option>"
{-# INLINE option #-}


-- | Generates an HTML __@\<output\>@__ element with the given attributes and contents.
--
-- [@Description@]: Calculated output value
-- [@Categories@]: /[flow](#flow)/; /[phrasing](#phrasing)/; /[listed](#listed)/; /[labelable](#labelable)/; /[resettable](#resettable)/; /[form-associated](#form-associated)/; /[palpable](#palpable)/
-- [@Parents@]: /[phrasing](#phrasing)/
-- [@Children@]: /[phrasing](#phrasing)/
-- [@Attributes@]: /[globals]("Html.Attributes#globals")/; 'Html.Attributes.for'; 'Html.Attributes.form'; 'Html.Attributes.name'
-- [@Example@]:
--
-- >>> output [] []
-- <output></output>
output :: [Attribute] -> [Html] -> Html
output = ParentNode "<output" "</output>"
{-# INLINE output #-}


-- | Generates an HTML __@\<p\>@__ element with the given attributes and contents.
--
-- [@Description@]: Paragraph
-- [@Categories@]: /[flow](#flow)/; /[palpable](#palpable)/
-- [@Parents@]: /[flow](#flow)/
-- [@Children@]: /[phrasing](#phrasing)/
-- [@Attributes@]: /[globals]("Html.Attributes#globals")/
-- [@Example@]:
--
-- >>> p [] []
-- <p></p>
p :: [Attribute] -> [Html] -> Html
p = ParentNode "<p" "</p>"
{-# INLINE p #-}


-- | Generates an HTML __@\<picture\>@__ element with the given attributes and contents.
--
-- [@Description@]: Image
-- [@Categories@]: /[flow](#flow)/; /[phrasing](#phrasing)/; /[embedded](#embedded)/; /[palpable](#palpable)/
-- [@Parents@]: /[phrasing](#phrasing)/
-- [@Children@]: 'Html.source'; one 'Html.img'; /[script-supporting](#script-supporting)/
-- [@Attributes@]: /[globals]("Html.Attributes#globals")/
-- [@Example@]:
--
-- >>> picture [] []
-- <picture></picture>
picture :: [Attribute] -> [Html] -> Html
picture = ParentNode "<picture" "</picture>"
{-# INLINE picture #-}


-- | Generates an HTML __@\<pre\>@__ element with the given attributes and contents.
--
-- [@Description@]: Block of preformatted text
-- [@Categories@]: /[flow](#flow)/; /[palpable](#palpable)/
-- [@Parents@]: /[flow](#flow)/
-- [@Children@]: /[phrasing](#phrasing)/
-- [@Attributes@]: /[globals]("Html.Attributes#globals")/
-- [@Example@]:
--
-- >>> pre [] []
-- <pre></pre>
pre :: [Attribute] -> [Html] -> Html
pre = ParentNode "<pre" "</pre>"
{-# INLINE pre #-}


-- | Generates an HTML __@\<progress\>@__ element with the given attributes and contents.
--
-- [@Description@]: Progress bar
-- [@Categories@]: /[flow](#flow)/; /[phrasing](#phrasing)/; /[labelable](#labelable)/; /[palpable](#palpable)/
-- [@Parents@]: /[phrasing](#phrasing)/
-- [@Children@]: /[phrasing](#phrasing)/
-- [@Attributes@]: /[globals]("Html.Attributes#globals")/; 'Html.Attributes.value'; 'Html.Attributes.max'
-- [@Example@]:
--
-- >>> progress [] []
-- <progress></progress>
progress :: [Attribute] -> [Html] -> Html
progress = ParentNode "<progress" "</progress>"
{-# INLINE progress #-}


-- | Generates an HTML __@\<q\>@__ element with the given attributes and contents.
--
-- [@Description@]: Quotation
-- [@Categories@]: /[flow](#flow)/; /[phrasing](#phrasing)/; /[palpable](#palpable)/
-- [@Parents@]: /[phrasing](#phrasing)/
-- [@Children@]: /[phrasing](#phrasing)/
-- [@Attributes@]: /[globals]("Html.Attributes#globals")/; 'Html.Attributes.cite'
-- [@Example@]:
--
-- >>> q [] []
-- <q></q>
q :: [Attribute] -> [Html] -> Html
q = ParentNode "<q" "</q>"
{-# INLINE q #-}


-- | Generates an HTML __@\<rp\>@__ element with the given attributes and contents.
--
-- [@Description@]: Parenthesis for ruby annotation text
-- [@Categories@]: none
-- [@Parents@]: 'Html.ruby'
-- [@Children@]: text
-- [@Attributes@]: /[globals]("Html.Attributes#globals")/
-- [@Example@]:
--
-- >>> rp [] []
-- <rp></rp>
rp :: [Attribute] -> [Html] -> Html
rp = ParentNode "<rp" "</rp>"
{-# INLINE rp #-}


-- | Generates an HTML __@\<rt\>@__ element with the given attributes and contents.
--
-- [@Description@]: Ruby annotation text
-- [@Categories@]: none
-- [@Parents@]: 'Html.ruby'
-- [@Children@]: /[phrasing](#phrasing)/
-- [@Attributes@]: /[globals]("Html.Attributes#globals")/
-- [@Example@]:
--
-- >>> rt [] []
-- <rt></rt>
rt :: [Attribute] -> [Html] -> Html
rt = ParentNode "<rt" "</rt>"
{-# INLINE rt #-}


-- | Generates an HTML __@\<ruby\>@__ element with the given attributes and contents.
--
-- [@Description@]: Ruby annotation(s)
-- [@Categories@]: /[flow](#flow)/; /[phrasing](#phrasing)/; /[palpable](#palpable)/
-- [@Parents@]: /[phrasing](#phrasing)/
-- [@Children@]: /[phrasing](#phrasing)/; 'Html.rt'; 'Html.rp'
-- [@Attributes@]: /[globals]("Html.Attributes#globals")/
-- [@Example@]:
--
-- >>> ruby [] []
-- <ruby></ruby>
ruby :: [Attribute] -> [Html] -> Html
ruby = ParentNode "<ruby" "</ruby>"
{-# INLINE ruby #-}


-- | Generates an HTML __@\<s\>@__ element with the given attributes and contents.
--
-- [@Description@]: Inaccurate text
-- [@Categories@]: /[flow](#flow)/; /[phrasing](#phrasing)/; /[palpable](#palpable)/
-- [@Parents@]: /[phrasing](#phrasing)/
-- [@Children@]: /[phrasing](#phrasing)/
-- [@Attributes@]: /[globals]("Html.Attributes#globals")/
-- [@Example@]:
--
-- >>> s [] []
-- <s></s>
s :: [Attribute] -> [Html] -> Html
s = ParentNode "<s" "</s>"
{-# INLINE s #-}


-- | Generates an HTML __@\<samp\>@__ element with the given attributes and contents.
--
-- [@Description@]: Computer output
-- [@Categories@]: /[flow](#flow)/; /[phrasing](#phrasing)/; /[palpable](#palpable)/
-- [@Parents@]: /[phrasing](#phrasing)/
-- [@Children@]: /[phrasing](#phrasing)/
-- [@Attributes@]: /[globals]("Html.Attributes#globals")/
-- [@Example@]:
--
-- >>> samp [] []
-- <samp></samp>
samp :: [Attribute] -> [Html] -> Html
samp = ParentNode "<samp" "</samp>"
{-# INLINE samp #-}


-- | Generates an HTML __@\<script\>@__ element with the given attributes and contents.
--
-- [@Description@]: Embedded script
-- [@Categories@]: /[metadata](#metadata)/; /[flow](#flow)/; /[phrasing](#phrasing)/; /[script-supporting](#script-supporting)/
-- [@Parents@]: 'Html.head'; /[phrasing](#phrasing)/; /[script-supporting](#script-supporting)/
-- [@Children@]: script, data, or script documentation
-- [@Attributes@]: /[globals]("Html.Attributes#globals")/; 'Html.Attributes.src'; 'Html.Attributes.type_'; 'Html.Attributes.nomodule'; 'Html.Attributes.async'; 'Html.Attributes.defer'; 'Html.Attributes.crossorigin'; 'Html.Attributes.integrity'; 'Html.Attributes.referrerpolicy'; 'Html.Attributes.blocking'; 'Html.Attributes.fetchpriority'
-- [@Example@]:
--
-- >>> script [] []
-- <script></script>
script :: [Attribute] -> [Html] -> Html
script = ParentNode "<script" "</script>"
{-# INLINE script #-}


-- | Generates an HTML __@\<search\>@__ element with the given attributes and contents.
--
-- [@Description@]: Container for search controls
-- [@Categories@]: /[flow](#flow)/; /[palpable](#palpable)/
-- [@Parents@]: /[flow](#flow)/
-- [@Children@]: /[flow](#flow)/
-- [@Attributes@]: /[globals]("Html.Attributes#globals")/
-- [@Example@]:
--
-- >>> search [] []
-- <search></search>
search :: [Attribute] -> [Html] -> Html
search = ParentNode "<search" "</search>"
{-# INLINE search #-}


-- | Generates an HTML __@\<section\>@__ element with the given attributes and contents.
--
-- [@Description@]: Generic document or application section
-- [@Categories@]: /[flow](#flow)/; /[sectioning](#sectioning)/; /[palpable](#palpable)/
-- [@Parents@]: /[flow](#flow)/
-- [@Children@]: /[flow](#flow)/
-- [@Attributes@]: /[globals]("Html.Attributes#globals")/
-- [@Example@]:
--
-- >>> section [] []
-- <section></section>
section :: [Attribute] -> [Html] -> Html
section = ParentNode "<section" "</section>"
{-# INLINE section #-}


-- | Generates an HTML __@\<select\>@__ element with the given attributes and contents.
--
-- [@Description@]: List box control
-- [@Categories@]: /[flow](#flow)/; /[phrasing](#phrasing)/; /[interactive](#interactive)/; /[listed](#listed)/; /[labelable](#labelable)/; /[submittable](#submittable)/; /[resettable](#resettable)/; /[form-associated](#form-associated)/; /[palpable](#palpable)/
-- [@Parents@]: /[phrasing](#phrasing)/
-- [@Children@]: 'Html.option'; 'Html.optgroup'; /[script-supporting](#script-supporting)/
-- [@Attributes@]: /[globals]("Html.Attributes#globals")/; 'Html.Attributes.autocomplete'; 'Html.Attributes.disabled'; 'Html.Attributes.form'; 'Html.Attributes.multiple'; 'Html.Attributes.name'; 'Html.Attributes.required'; 'Html.Attributes.size'
-- [@Example@]:
--
-- >>> select [] []
-- <select></select>
select :: [Attribute] -> [Html] -> Html
select = ParentNode "<select" "</select>"
{-# INLINE select #-}


-- | Generates an HTML __@\<slot\>@__ element with the given attributes and contents.
--
-- [@Description@]: Shadow tree slot
-- [@Categories@]: /[flow](#flow)/; /[phrasing](#phrasing)/
-- [@Parents@]: /[phrasing](#phrasing)/
-- [@Children@]: transparent
-- [@Attributes@]: /[globals]("Html.Attributes#globals")/; 'Html.Attributes.name'
-- [@Example@]:
--
-- >>> slot [] []
-- <slot></slot>
--
-- /Note: This element collides with the 'Html.Attributes.slot' attribute./
slot :: [Attribute] -> [Html] -> Html
slot = ParentNode "<slot" "</slot>"
{-# INLINE slot #-}


-- | Generates an HTML __@\<small\>@__ element with the given attributes and contents.
--
-- [@Description@]: Side comment
-- [@Categories@]: /[flow](#flow)/; /[phrasing](#phrasing)/; /[palpable](#palpable)/
-- [@Parents@]: /[phrasing](#phrasing)/
-- [@Children@]: /[phrasing](#phrasing)/
-- [@Attributes@]: /[globals]("Html.Attributes#globals")/
-- [@Example@]:
--
-- >>> small [] []
-- <small></small>
small :: [Attribute] -> [Html] -> Html
small = ParentNode "<small" "</small>"
{-# INLINE small #-}


-- | Generates an HTML __@\<source\>@__ element with the given attributes.
--
-- [@Description@]: Image source for 'Html.img' or media source for 'Html.video' or 'Html.audio'
-- [@Categories@]: none
-- [@Parents@]: 'Html.picture'; 'Html.video'; 'Html.audio'
-- [@Children@]: empty
-- [@Attributes@]: /[globals]("Html.Attributes#globals")/; 'Html.Attributes.type_'; 'Html.Attributes.media'; 'Html.Attributes.src'; 'Html.Attributes.srcset'; 'Html.Attributes.sizes'; 'Html.Attributes.width'; 'Html.Attributes.height'
-- [@Example@]:
--
-- >>> source []
-- <source>
source :: [Attribute] -> Html
source = LeafNode "<source"
{-# INLINE source #-}


-- | Generates an HTML __@\<span\>@__ element with the given attributes and contents.
--
-- [@Description@]: Generic phrasing container
-- [@Categories@]: /[flow](#flow)/; /[phrasing](#phrasing)/; /[palpable](#palpable)/
-- [@Parents@]: /[phrasing](#phrasing)/
-- [@Children@]: /[phrasing](#phrasing)/
-- [@Attributes@]: /[globals]("Html.Attributes#globals")/
-- [@Example@]:
--
-- >>> span [] []
-- <span></span>
--
-- /Note: This element collides with the 'Html.Attributes.span' attribute./
span :: [Attribute] -> [Html] -> Html
span = ParentNode "<span" "</span>"
{-# INLINE span #-}


-- | Generates an HTML __@\<strong\>@__ element with the given attributes and contents.
--
-- [@Description@]: Importance
-- [@Categories@]: /[flow](#flow)/; /[phrasing](#phrasing)/; /[palpable](#palpable)/
-- [@Parents@]: /[phrasing](#phrasing)/
-- [@Children@]: /[phrasing](#phrasing)/
-- [@Attributes@]: /[globals]("Html.Attributes#globals")/
-- [@Example@]:
--
-- >>> strong [] []
-- <strong></strong>
strong :: [Attribute] -> [Html] -> Html
strong = ParentNode "<strong" "</strong>"
{-# INLINE strong #-}


-- | Generates an HTML __@\<style\>@__ element with the given attributes and contents.
--
-- [@Description@]: Embedded styling information
-- [@Categories@]: /[metadata](#metadata)/
-- [@Parents@]: 'Html.head'; 'Html.noscript'
-- [@Children@]: text
-- [@Attributes@]: /[globals]("Html.Attributes#globals")/; 'Html.Attributes.media'; 'Html.Attributes.blocking'
-- [@Example@]:
--
-- >>> style [] []
-- <style></style>
--
-- /Note: This element collides with the 'Html.Attributes.style' attribute./
style :: [Attribute] -> [Html] -> Html
style = ParentNode "<style" "</style>"
{-# INLINE style #-}


-- | Generates an HTML __@\<sub\>@__ element with the given attributes and contents.
--
-- [@Description@]: Subscript
-- [@Categories@]: /[flow](#flow)/; /[phrasing](#phrasing)/; /[palpable](#palpable)/
-- [@Parents@]: /[phrasing](#phrasing)/
-- [@Children@]: /[phrasing](#phrasing)/
-- [@Attributes@]: /[globals]("Html.Attributes#globals")/
-- [@Example@]:
--
-- >>> sub [] []
-- <sub></sub>
sub :: [Attribute] -> [Html] -> Html
sub = ParentNode "<sub" "</sub>"
{-# INLINE sub #-}


-- | Generates an HTML __@\<summary\>@__ element with the given attributes and contents.
--
-- [@Description@]: Caption for 'Html.details'
-- [@Categories@]: none
-- [@Parents@]: 'Html.details'
-- [@Children@]: /[phrasing](#phrasing)/; /[heading](#heading)/
-- [@Attributes@]: /[globals]("Html.Attributes#globals")/
-- [@Example@]:
--
-- >>> summary [] []
-- <summary></summary>
summary :: [Attribute] -> [Html] -> Html
summary = ParentNode "<summary" "</summary>"
{-# INLINE summary #-}


-- | Generates an HTML __@\<sup\>@__ element with the given attributes and contents.
--
-- [@Description@]: Superscript
-- [@Categories@]: /[flow](#flow)/; /[phrasing](#phrasing)/; /[palpable](#palpable)/
-- [@Parents@]: /[phrasing](#phrasing)/
-- [@Children@]: /[phrasing](#phrasing)/
-- [@Attributes@]: /[globals]("Html.Attributes#globals")/
-- [@Example@]:
--
-- >>> sup [] []
-- <sup></sup>
sup :: [Attribute] -> [Html] -> Html
sup = ParentNode "<sup" "</sup>"
{-# INLINE sup #-}


-- | Generates an HTML __@\<table\>@__ element with the given attributes and contents.
--
-- [@Description@]: Table
-- [@Categories@]: /[flow](#flow)/; /[palpable](#palpable)/
-- [@Parents@]: /[flow](#flow)/
-- [@Children@]: 'Html.caption'; 'Html.colgroup'; 'Html.thead'; 'Html.tbody'; 'Html.tfoot'; 'Html.tr'; /[script-supporting](#script-supporting)/
-- [@Attributes@]: /[globals]("Html.Attributes#globals")/
-- [@Example@]:
--
-- >>> table [] []
-- <table></table>
table :: [Attribute] -> [Html] -> Html
table = ParentNode "<table" "</table>"
{-# INLINE table #-}


-- | Generates an HTML __@\<tbody\>@__ element with the given attributes and contents.
--
-- [@Description@]: Group of rows in a table
-- [@Categories@]: none
-- [@Parents@]: 'Html.table'
-- [@Children@]: 'Html.tr'; /[script-supporting](#script-supporting)/
-- [@Attributes@]: /[globals]("Html.Attributes#globals")/
-- [@Example@]:
--
-- >>> tbody [] []
-- <tbody></tbody>
tbody :: [Attribute] -> [Html] -> Html
tbody = ParentNode "<tbody" "</tbody>"
{-# INLINE tbody #-}


-- | Generates an HTML __@\<td\>@__ element with the given attributes and contents.
--
-- [@Description@]: Table cell
-- [@Categories@]: none
-- [@Parents@]: 'Html.tr'
-- [@Children@]: /[flow](#flow)/
-- [@Attributes@]: /[globals]("Html.Attributes#globals")/; 'Html.Attributes.colspan'; 'Html.Attributes.rowspan'; 'Html.Attributes.headers'
-- [@Example@]:
--
-- >>> td [] []
-- <td></td>
td :: [Attribute] -> [Html] -> Html
td = ParentNode "<td" "</td>"
{-# INLINE td #-}


-- | Generates an HTML __@\<template\>@__ element with the given attributes and contents.
--
-- [@Description@]: Template
-- [@Categories@]: /[metadata](#metadata)/; /[flow](#flow)/; /[phrasing](#phrasing)/; /[script-supporting](#script-supporting)/
-- [@Parents@]: /[metadata](#metadata)/; /[phrasing](#phrasing)/; /[script-supporting](#script-supporting)/; 'Html.colgroup'
-- [@Children@]: none
-- [@Attributes@]: /[globals]("Html.Attributes#globals")/; 'Html.Attributes.shadowrootmode'; 'Html.Attributes.shadowrootdelegatesfocus'; 'Html.Attributes.shadowrootclonable'; 'Html.Attributes.shadowrootserializable'
-- [@Example@]:
--
-- >>> template [] []
-- <template></template>
template :: [Attribute] -> [Html] -> Html
template = ParentNode "<template" "</template>"
{-# INLINE template #-}


-- | Generates an HTML __@\<textarea\>@__ element with the given attributes and contents.
--
-- [@Description@]: Multiline text controls
-- [@Categories@]: /[flow](#flow)/; /[phrasing](#phrasing)/; /[interactive](#interactive)/; /[listed](#listed)/; /[labelable](#labelable)/; /[submittable](#submittable)/; /[resettable](#resettable)/; /[form-associated](#form-associated)/; /[palpable](#palpable)/
-- [@Parents@]: /[phrasing](#phrasing)/
-- [@Children@]: text
-- [@Attributes@]: /[globals]("Html.Attributes#globals")/; 'Html.Attributes.autocomplete'; 'Html.Attributes.cols'; 'Html.Attributes.dirname'; 'Html.Attributes.disabled'; 'Html.Attributes.form'; 'Html.Attributes.maxlength'; 'Html.Attributes.minlength'; 'Html.Attributes.name'; 'Html.Attributes.placeholder'; 'Html.Attributes.readonly'; 'Html.Attributes.required'; 'Html.Attributes.rows'; 'Html.Attributes.wrap'
-- [@Example@]:
--
-- >>> textarea [] []
-- <textarea></textarea>
textarea :: [Attribute] -> [Html] -> Html
textarea = ParentNode "<textarea" "</textarea>"
{-# INLINE textarea #-}


-- | Generates an HTML __@\<tfoot\>@__ element with the given attributes and contents.
--
-- [@Description@]: Group of footer rows in a table
-- [@Categories@]: none
-- [@Parents@]: 'Html.table'
-- [@Children@]: 'Html.tr'; /[script-supporting](#script-supporting)/
-- [@Attributes@]: /[globals]("Html.Attributes#globals")/
-- [@Example@]:
--
-- >>> tfoot [] []
-- <tfoot></tfoot>
tfoot :: [Attribute] -> [Html] -> Html
tfoot = ParentNode "<tfoot" "</tfoot>"
{-# INLINE tfoot #-}


-- | Generates an HTML __@\<th\>@__ element with the given attributes and contents.
--
-- [@Description@]: Table header cell
-- [@Categories@]: /[interactive](#interactive)/
-- [@Parents@]: 'Html.tr'
-- [@Children@]: /[flow](#flow)/
-- [@Attributes@]: /[globals]("Html.Attributes#globals")/; 'Html.Attributes.colspan'; 'Html.Attributes.rowspan'; 'Html.Attributes.headers'; 'Html.Attributes.scope'; 'Html.Attributes.abbr'
-- [@Example@]:
--
-- >>> th [] []
-- <th></th>
th :: [Attribute] -> [Html] -> Html
th = ParentNode "<th" "</th>"
{-# INLINE th #-}


-- | Generates an HTML __@\<thead\>@__ element with the given attributes and contents.
--
-- [@Description@]: Group of heading rows in a table
-- [@Categories@]: none
-- [@Parents@]: 'Html.table'
-- [@Children@]: 'Html.tr'; /[script-supporting](#script-supporting)/
-- [@Attributes@]: /[globals]("Html.Attributes#globals")/
-- [@Example@]:
--
-- >>> thead [] []
-- <thead></thead>
thead :: [Attribute] -> [Html] -> Html
thead = ParentNode "<thead" "</thead>"
{-# INLINE thead #-}


-- | Generates an HTML __@\<time\>@__ element with the given attributes and contents.
--
-- [@Description@]: Machine-r/eadable equivalent of/ date- or time-related data
-- [@Categories@]: /[flow](#flow)/; /[phrasing](#phrasing)/; /[palpable](#palpable)/
-- [@Parents@]: /[phrasing](#phrasing)/
-- [@Children@]: /[phrasing](#phrasing)/
-- [@Attributes@]: /[globals]("Html.Attributes#globals")/; 'Html.Attributes.datetime'
-- [@Example@]:
--
-- >>> time [] []
-- <time></time>
time :: [Attribute] -> [Html] -> Html
time = ParentNode "<time" "</time>"
{-# INLINE time #-}


-- | Generates an HTML __@\<title\>@__ element with the given attributes and contents.
--
-- [@Description@]: Document title
-- [@Categories@]: /[metadata](#metadata)/
-- [@Parents@]: 'Html.head'
-- [@Children@]: text
-- [@Attributes@]: /[globals]("Html.Attributes#globals")/
-- [@Example@]:
--
-- >>> title [] []
-- <title></title>
--
-- /Note: This element collides with the 'Html.Attributes.title' attribute./
title :: [Attribute] -> [Html] -> Html
title = ParentNode "<title" "</title>"
{-# INLINE title #-}


-- | Generates an HTML __@\<tr\>@__ element with the given attributes and contents.
--
-- [@Description@]: Table row
-- [@Categories@]: none
-- [@Parents@]: 'Html.table'; 'Html.thead'; 'Html.tbody'; 'Html.tfoot'
-- [@Children@]: 'Html.th'; 'Html.td'; /[script-supporting](#script-supporting)/
-- [@Attributes@]: /[globals]("Html.Attributes#globals")/
-- [@Example@]:
--
-- >>> tr [] []
-- <tr></tr>
tr :: [Attribute] -> [Html] -> Html
tr = ParentNode "<tr" "</tr>"
{-# INLINE tr #-}


-- | Generates an HTML __@\<track\>@__ element with the given attributes.
--
-- [@Description@]: Timed text track
-- [@Categories@]: none
-- [@Parents@]: 'Html.audio'; 'Html.video'
-- [@Children@]: empty
-- [@Attributes@]: /[globals]("Html.Attributes#globals")/; 'Html.Attributes.default_'; 'Html.Attributes.kind'; 'Html.Attributes.label'; 'Html.Attributes.src'; 'Html.Attributes.srclang'
-- [@Example@]:
--
-- >>> track []
-- <track>
track :: [Attribute] -> Html
track = LeafNode "<track"
{-# INLINE track #-}


-- | Generates an HTML __@\<u\>@__ element with the given attributes and contents.
--
-- [@Description@]: Unarticulated annotation
-- [@Categories@]: /[flow](#flow)/; /[phrasing](#phrasing)/; /[palpable](#palpable)/
-- [@Parents@]: /[phrasing](#phrasing)/
-- [@Children@] /[phrasing](#phrasing)/
-- [@Attributes@]: /[globals]("Html.Attributes#globals")/
-- [@Example@]:
--
-- >>> u [] []
-- <u></u>
u :: [Attribute] -> [Html] -> Html
u = ParentNode "<u" "</u>"
{-# INLINE u #-}


-- | Generates an HTML __@\<ul\>@__ element with the given attributes and contents.
--
-- [@Description@]: List
-- [@Categories@]: /[flow](#flow)/; /[palpable](#palpable)/
-- [@Parents@]: /[flow](#flow)/
-- [@Children@]: 'Html.li'; /[script-supporting](#script-supporting)/
-- [@Attributes@]: /[globals]("Html.Attributes#globals")/
-- [@Example@]:
--
-- >>> ul [] []
-- <ul></ul>
ul :: [Attribute] -> [Html] -> Html
ul = ParentNode "<ul" "</ul>"
{-# INLINE ul #-}


-- | Generates an HTML __@\<var\>@__ element with the given attributes and contents.
--
-- [@Description@]: Variable
-- [@Categories@]: /[flow](#flow)/; /[phrasing](#phrasing)/; /[palpable](#palpable)/
-- [@Parents@]: /[phrasing](#phrasing)/
-- [@Children@]: /[phrasing](#phrasing)/
-- [@Attributes@]: /[globals]("Html.Attributes#globals")/
-- [@Example@]:
--
-- >>> var [] []
-- <var></var>
var :: [Attribute] -> [Html] -> Html
var = ParentNode "<var" "</var>"
{-# INLINE var #-}


-- | Generates an HTML __@\<video\>@__ element with the given attributes and contents.
--
-- [@Description@]: Video player
-- [@Categories@]: /[flow](#flow)/; /[phrasing](#phrasing)/; /[embedded](#embedded)/; /[interactive](#interactive)/; /[palpable](#palpable)/
-- [@Parents@]: /[phrasing](#phrasing)/
-- [@Children@]: 'Html.source'; 'Html.track'; transparent
-- [@Attributes@]: /[globals]("Html.Attributes#globals")/; 'Html.Attributes.src'; 'Html.Attributes.crossorigin'; 'Html.Attributes.poster'; 'Html.Attributes.preload'; 'Html.Attributes.autoplay'; 'Html.Attributes.playsinline'; 'Html.Attributes.loop'; 'Html.Attributes.muted'; 'Html.Attributes.controls'; 'Html.Attributes.width'; 'Html.Attributes.height'
-- [@Example@]:
--
-- >>> video [] []
-- <video></video>
video :: [Attribute] -> [Html] -> Html
video = ParentNode "<video" "</video>"
{-# INLINE video #-}


-- | Generates an HTML __@\<wbr\>@__ element with the given attributes.
--
-- [@Description@]: Line breaking opportunity
-- [@Categories@]: /[flow](#flow)/; /[phrasing](#phrasing)/
-- [@Parents@]: /[phrasing](#phrasing)/
-- [@Children@]: empty
-- [@Attributes@]: /[globals]("Html.Attributes#globals")/
-- [@Example@]:
--
-- >>> wbr []
-- <wbr>
wbr :: [Attribute] -> Html
wbr = LeafNode "<wbr"
{-#INLINE wbr #-}
