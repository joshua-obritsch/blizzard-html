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
-- The 'Html.Math.math' and 'Html.Svg.svg' elements and their related elements and attributes have been moved into the "Html.Math" and
-- "Html.Svg" modules respectively.
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
data Html ctx

    -- | Constructs an HTML parent node.
    = ParentNode Builder Builder [Attribute ctx] [Html ctx]

    -- | Constructs an HTML leaf node.
    | LeafNode Builder [Attribute ctx]

    -- | Constructs an HTML root node.
    | RootNode Builder [Html ctx]

    -- | Constructs an HTML text node.
    | TextNode Builder


instance IsString (Html ctx) where
    fromString = TextNode . fromString


instance Show (Html ctx) where
    show = unpack . toLazyText . build


instance Buildable (Html ctx) where
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


instance {-# OVERLAPPING #-} Show [Html ctx] where
    show = unpack . toLazyText . build


instance Buildable [Html ctx] where
    build = foldr ((<>) . build) mempty


-- | Represents an HTML attribute.
data Attribute ctx

    -- | Constructs a boolean HTML attribute.
    = BoolAttribute Builder Bool

    -- | Constructs a textual HTML attribute.
    | TextAttribute Builder Builder


instance Show (Attribute ctx) where
    show = unpack . toLazyText . build


instance Buildable (Attribute ctx) where
    build attribute = case attribute of
        BoolAttribute _   False -> mempty
        BoolAttribute key True  -> key
        TextAttribute key value -> key <> value <> singleton '"'


instance {-# OVERLAPPING #-} Show [Attribute ctx] where
    show = unpack . toLazyText . build


instance Buildable [Attribute ctx] where
    build = foldr ((<>) . build) mempty


-- CLASSES


-- | Enables conversion to 'Data.Text.Lazy.Builder.Builder'.
class Buildable a where

    -- | Converts to 'Data.Text.Lazy.Builder.Builder'.
    build :: a -> Builder


-- DECLARATIONS


-- | Generates an HTML @__\<!DOCTYPE\>__@ declaration with the given contents.
doctype :: [Html ctx] -> Html ctx
doctype = RootNode "<!DOCTYPE html>\n"
{-# INLINE doctype #-}


-- ELEMENTS


-- | Generates an HTML @__\<a\>__@ element with the given attributes and contents.
a :: [Attribute ctx] -> [Html ctx] -> Html ctx
a = ParentNode "<a" "</a>"
{-# INLINE a #-}


-- | Generates an HTML @__\<abbr\>__@ element with the given attributes and contents.
abbr :: [Attribute ctx] -> [Html ctx] -> Html ctx
abbr = ParentNode "<abbr" "</abbr>"
{-# INLINE abbr #-}


-- | Generates an HTML @__\<address\>__@ element with the given attributes and contents.
address :: [Attribute ctx] -> [Html ctx] -> Html ctx
address = ParentNode "<address" "</address>"
{-# INLINE address #-}


-- | Generates an HTML @__\<area\>__@ element with the given attributes.
area :: [Attribute ctx] -> Html ctx
area = LeafNode "<area"
{-# INLINE area #-}


-- | Generates an HTML @__\<article\>__@ element with the given attributes and contents.
article :: [Attribute ctx] -> [Html ctx] -> Html ctx
article = ParentNode "<article" "</article>"
{-# INLINE article #-}


-- | Generates an HTML @__\<aside\>__@ element with the given attributes and contents.
aside :: [Attribute ctx] -> [Html ctx] -> Html ctx
aside = ParentNode "<aside" "</aside>"
{-# INLINE aside #-}


-- | Generates an HTML @__\<audio\>__@ element with the given attributes and contents.
audio :: [Attribute ctx] -> [Html ctx] -> Html ctx
audio = ParentNode "<audio" "</audio>"
{-# INLINE audio #-}


-- | Generates an HTML @__\<b\>__@ element with the given attributes and contents.
b :: [Attribute ctx] -> [Html ctx] -> Html ctx
b = ParentNode "<b" "</b>"
{-# INLINE b #-}


-- | Generates an HTML @__\<base\>__@ element with the given attributes.
base :: [Attribute ctx] -> Html ctx
base = LeafNode "<base"
{-# INLINE base #-}


-- | Generates an HTML @__\<bdi\>__@ element with the given attributes and contents.
bdi :: [Attribute ctx] -> [Html ctx] -> Html ctx
bdi = ParentNode "<bdi" "</bdi>"
{-# INLINE bdi #-}


-- | Generates an HTML @__\<bdo\>__@ element with the given attributes and contents.
bdo :: [Attribute ctx] -> [Html ctx] -> Html ctx
bdo = ParentNode "<bdo" "</bdo>"
{-# INLINE bdo #-}


-- | Generates an HTML @__\<blockquote\>__@ element with the given attributes and contents.
blockquote :: [Attribute ctx] -> [Html ctx] -> Html ctx
blockquote = ParentNode "<blockquote" "</blockquote>"
{-# INLINE blockquote #-}


-- | Generates an HTML @__\<body\>__@ element with the given attributes and contents.
body :: [Attribute ctx] -> [Html ctx] -> Html ctx
body = ParentNode "<body" "</body>"
{-# INLINE body #-}


-- | Generates an HTML @__\<br\>__@ element with the given attributes.
br :: [Attribute ctx] -> Html ctx
br = LeafNode "<br"
{-# INLINE br #-}


-- | Generates an HTML @__\<button\>__@ element with the given attributes and contents.
button :: [Attribute ctx] -> [Html ctx] -> Html ctx
button = ParentNode "<button" "</button>"
{-# INLINE button #-}


-- | Generates an HTML @__\<canvas\>__@ element with the given attributes and contents.
canvas :: [Attribute ctx] -> [Html ctx] -> Html ctx
canvas = ParentNode "<canvas" "</canvas>"
{-# INLINE canvas #-}


-- | Generates an HTML @__\<caption\>__@ element with the given attributes and contents.
caption :: [Attribute ctx] -> [Html ctx] -> Html ctx
caption = ParentNode "<caption" "</caption>"
{-# INLINE caption #-}


-- | Generates an HTML @__\<cite\>__@ element with the given attributes and contents.
cite :: [Attribute ctx] -> [Html ctx] -> Html ctx
cite = ParentNode "<cite" "</cite>"
{-# INLINE cite #-}


-- | Generates an HTML @__\<code\>__@ element with the given attributes and contents.
code :: [Attribute ctx] -> [Html ctx] -> Html ctx
code = ParentNode "<code" "</code>"
{-# INLINE code #-}


-- | Generates an HTML @__\<col\>__@ element with the given attributes.
col :: [Attribute ctx] -> Html ctx
col = LeafNode "<col"
{-# INLINE col #-}


-- | Generates an HTML @__\<colgroup\>__@ element with the given attributes and contents.
colgroup :: [Attribute ctx] -> [Html ctx] -> Html ctx
colgroup = ParentNode "<colgroup" "</colgroup>"
{-# INLINE colgroup #-}


-- | Generates an HTML @__\<data\>__@ element with the given attributes and contents.
data_ :: [Attribute ctx] -> [Html ctx] -> Html ctx
data_ = ParentNode "<data" "</data>"
{-# INLINE data_ #-}


-- | Generates an HTML @__\<datalist\>__@ element with the given attributes and contents.
datalist :: [Attribute ctx] -> [Html ctx] -> Html ctx
datalist = ParentNode "<datalist" "</datalist>"
{-# INLINE datalist #-}


-- | Generates an HTML @__\<dd\>__@ element with the given attributes and contents.
dd :: [Attribute ctx] -> [Html ctx] -> Html ctx
dd = ParentNode "<dd" "</dd>"
{-# INLINE dd #-}


-- | Generates an HTML @__\<del\>__@ element with the given attributes and contents.
del :: [Attribute ctx] -> [Html ctx] -> Html ctx
del = ParentNode "<del" "</del>"
{-# INLINE del #-}


-- | Generates an HTML @__\<details\>__@ element with the given attributes and contents.
details :: [Attribute ctx] -> [Html ctx] -> Html ctx
details = ParentNode "<details" "</details>"
{-# INLINE details #-}


-- | Generates an HTML @__\<dfn\>__@ element with the given attributes and contents.
dfn :: [Attribute ctx] -> [Html ctx] -> Html ctx
dfn = ParentNode "<dfn" "</dfn>"
{-# INLINE dfn #-}


-- | Generates an HTML @__\<dialog\>__@ element with the given attributes and contents.
dialog :: [Attribute ctx] -> [Html ctx] -> Html ctx
dialog = ParentNode "<dialog" "</dialog>"
{-# INLINE dialog #-}


-- | Generates an HTML @__\<div\>__@ element with the given attributes and contents.
div :: [Attribute ctx] -> [Html ctx] -> Html ctx
div = ParentNode "<div" "</div>"
{-# INLINE div #-}


-- | Generates an HTML @__\<dl\>__@ element with the given attributes and contents.
dl :: [Attribute ctx] -> [Html ctx] -> Html ctx
dl = ParentNode "<dl" "</dl>"
{-# INLINE dl #-}


-- | Generates an HTML @__\<dt\>__@ element with the given attributes and contents.
dt :: [Attribute ctx] -> [Html ctx] -> Html ctx
dt = ParentNode "<dt" "</dt>"
{-# INLINE dt #-}


-- | Generates an HTML @__\<em\>__@ element with the given attributes and contents.
em :: [Attribute ctx] -> [Html ctx] -> Html ctx
em = ParentNode "<em" "</em>"
{-# INLINE em #-}


-- | Generates an HTML @__\<embed\>__@ element with the given attributes.
embed :: [Attribute ctx] -> Html ctx
embed = LeafNode "<embed"
{-# INLINE embed #-}


-- | Generates an HTML @__\<fieldset\>__@ element with the given attributes and contents.
fieldset :: [Attribute ctx] -> [Html ctx] -> Html ctx
fieldset = ParentNode "<fieldset" "</fieldset>"
{-# INLINE fieldset #-}


-- | Generates an HTML @__\<figcaption\>__@ element with the given attributes and contents.
figcaption :: [Attribute ctx] -> [Html ctx] -> Html ctx
figcaption = ParentNode "<figcaption" "</figcaption>"
{-# INLINE figcaption #-}


-- | Generates an HTML @__\<figure\>__@ element with the given attributes and contents.
figure :: [Attribute ctx] -> [Html ctx] -> Html ctx
figure = ParentNode "<figure" "</figure>"
{-# INLINE figure #-}


-- | Generates an HTML @__\<footer\>__@ element with the given attributes and contents.
footer :: [Attribute ctx] -> [Html ctx] -> Html ctx
footer = ParentNode "<footer" "</footer>"
{-# INLINE footer #-}


-- | Generates an HTML @__\<form\>__@ element with the given attributes and contents.
form :: [Attribute ctx] -> [Html ctx] -> Html ctx
form = ParentNode "<form" "</form>"
{-# INLINE form #-}


-- | Generates an HTML @__\<h1\>__@ element with the given attributes and contents.
h1 :: [Attribute ctx] -> [Html ctx] -> Html ctx
h1 = ParentNode "<h1" "</h1>"
{-# INLINE h1 #-}


-- | Generates an HTML @__\<h2\>__@ element with the given attributes and contents.
h2 :: [Attribute ctx] -> [Html ctx] -> Html ctx
h2 = ParentNode "<h2" "</h2>"
{-# INLINE h2 #-}


-- | Generates an HTML @__\<h3\>__@ element with the given attributes and contents.
h3 :: [Attribute ctx] -> [Html ctx] -> Html ctx
h3 = ParentNode "<h3" "</h3>"
{-# INLINE h3 #-}


-- | Generates an HTML @__\<h4\>__@ element with the given attributes and contents.
h4 :: [Attribute ctx] -> [Html ctx] -> Html ctx
h4 = ParentNode "<h4" "</h4>"
{-# INLINE h4 #-}


-- | Generates an HTML @__\<h5\>__@ element with the given attributes and contents.
h5 :: [Attribute ctx] -> [Html ctx] -> Html ctx
h5 = ParentNode "<h5" "</h5>"
{-# INLINE h5 #-}


-- | Generates an HTML @__\<h6\>__@ element with the given attributes and contents.
h6 :: [Attribute ctx] -> [Html ctx] -> Html ctx
h6 = ParentNode "<h6" "</h6>"
{-# INLINE h6 #-}


-- | Generates an HTML @__\<head\>__@ element with the given attributes and contents.
head :: [Attribute ctx] -> [Html ctx] -> Html ctx
head = ParentNode "<head" "</head>"
{-# INLINE head #-}


-- | Generates an HTML @__\<header\>__@ element with the given attributes and contents.
header :: [Attribute ctx] -> [Html ctx] -> Html ctx
header = ParentNode "<header" "</header>"
{-# INLINE header #-}


-- | Generates an HTML @__\<hgroup\>__@ element with the given attributes and contents.
hgroup :: [Attribute ctx] -> [Html ctx] -> Html ctx
hgroup = ParentNode "<hgroup" "</hgroup>"
{-# INLINE hgroup #-}


-- | Generates an HTML @__\<hr\>__@ element with the given attributes.
hr :: [Attribute ctx] -> Html ctx
hr = LeafNode "<hr"
{-# INLINE hr #-}


-- | Generates an HTML @__\<html\>__@ element with the given attributes and contents.
html :: [Attribute ctx] -> [Html ctx] -> Html ctx
html = ParentNode "<html" "</html>"
{-# INLINE html #-}


-- | Generates an HTML @__\<i\>__@ element with the given attributes and contents.
i :: [Attribute ctx] -> [Html ctx] -> Html ctx
i = ParentNode "<i" "</i>"
{-# INLINE i #-}


-- | Generates an HTML @__\<iframe\>__@ element with the given attributes and contents.
iframe :: [Attribute ctx] -> [Html ctx] -> Html ctx
iframe = ParentNode "<iframe" "</iframe>"
{-# INLINE iframe #-}


-- | Generates an HTML @__\<img\>__@ element with the given attributes.
img :: [Attribute ctx] -> Html ctx
img = LeafNode "<img"
{-# INLINE img #-}


-- | Generates an HTML @__\<input\>__@ element with the given attributes.
input :: [Attribute ctx] -> Html ctx
input = LeafNode "<input"
{-# INLINE input #-}


-- | Generates an HTML @__\<ins\>__@ element with the given attributes and contents.
ins :: [Attribute ctx] -> [Html ctx] -> Html ctx
ins = ParentNode "<ins" "</ins>"
{-# INLINE ins #-}


-- | Generates an HTML @__\<kbd\>__@ element with the given attributes and contents.
kbd :: [Attribute ctx] -> [Html ctx] -> Html ctx
kbd = ParentNode "<kbd" "</kbd>"
{-# INLINE kbd #-}


-- | Generates an HTML @__\<label\>__@ element with the given attributes and contents.
label :: [Attribute ctx] -> [Html ctx] -> Html ctx
label = ParentNode "<label" "</label>"
{-# INLINE label #-}


-- | Generates an HTML @__\<legend\>__@ element with the given attributes and contents.
legend :: [Attribute ctx] -> [Html ctx] -> Html ctx
legend = ParentNode "<legend" "</legend>"
{-# INLINE legend #-}


-- | Generates an HTML @__\<li\>__@ element with the given attributes and contents.
li :: [Attribute ctx] -> [Html ctx] -> Html ctx
li = ParentNode "<li" "</li>"
{-# INLINE li #-}


-- | Generates an HTML @__\<link\>__@ element with the given attributes.
link :: [Attribute ctx] -> Html ctx
link = LeafNode "<link"
{-# INLINE link #-}


-- | Generates an HTML @__\<main\>__@ element with the given attributes and contents.
main :: [Attribute ctx] -> [Html ctx] -> Html ctx
main = ParentNode "<main" "</main>"
{-# INLINE main #-}


-- | Generates an HTML @__\<map\>__@ element with the given attributes and contents.
map :: [Attribute ctx] -> [Html ctx] -> Html ctx
map = ParentNode "<map" "</map>"
{-# INLINE map #-}


-- | Generates an HTML @__\<mark\>__@ element with the given attributes and contents.
mark :: [Attribute ctx] -> [Html ctx] -> Html ctx
mark = ParentNode "<mark" "</mark>"
{-# INLINE mark #-}


-- | Generates an HTML @__\<menu\>__@ element with the given attributes and contents.
menu :: [Attribute ctx] -> [Html ctx] -> Html ctx
menu = ParentNode "<menu" "</menu>"
{-# INLINE menu #-}


-- | Generates an HTML @__\<meta\>__@ element with the given attributes.
meta :: [Attribute ctx] -> Html ctx
meta = LeafNode "<meta"
{-# INLINE meta #-}


-- | Generates an HTML @__\<meter\>__@ element with the given attributes and contents.
meter :: [Attribute ctx] -> [Html ctx] -> Html ctx
meter = ParentNode "<meter" "</meter>"
{-# INLINE meter #-}


-- | Generates an HTML @__\<nav\>__@ element with the given attributes and contents.
nav :: [Attribute ctx] -> [Html ctx] -> Html ctx
nav = ParentNode "<nav" "</nav>"
{-# INLINE nav #-}


-- | Generates an HTML @__\<noscript\>__@ element with the given attributes and contents.
noscript :: [Attribute ctx] -> [Html ctx] -> Html ctx
noscript = ParentNode "<noscript" "</noscript>"
{-# INLINE noscript #-}


-- | Generates an HTML @__\<object\>__@ element with the given attributes and contents.
object :: [Attribute ctx] -> [Html ctx] -> Html ctx
object = ParentNode "<object" "</object>"
{-# INLINE object #-}


-- | Generates an HTML @__\<ol\>__@ element with the given attributes and contents.
ol :: [Attribute ctx] -> [Html ctx] -> Html ctx
ol = ParentNode "<ol" "</ol>"
{-# INLINE ol #-}


-- | Generates an HTML @__\<optgroup\>__@ element with the given attributes and contents.
optgroup :: [Attribute ctx] -> [Html ctx] -> Html ctx
optgroup = ParentNode "<optgroup" "</optgroup>"
{-# INLINE optgroup #-}


-- | Generates an HTML @__\<option\>__@ element with the given attributes and contents.
option :: [Attribute ctx] -> [Html ctx] -> Html ctx
option = ParentNode "<option" "</option>"
{-# INLINE option #-}


-- | Generates an HTML @__\<output\>__@ element with the given attributes and contents.
output :: [Attribute ctx] -> [Html ctx] -> Html ctx
output = ParentNode "<output" "</output>"
{-# INLINE output #-}


-- | Generates an HTML @__\<p\>__@ element with the given attributes and contents.
p :: [Attribute ctx] -> [Html ctx] -> Html ctx
p = ParentNode "<p" "</p>"
{-# INLINE p #-}


-- | Generates an HTML @__\<picture\>__@ element with the given attributes and contents.
picture :: [Attribute ctx] -> [Html ctx] -> Html ctx
picture = ParentNode "<picture" "</picture>"
{-# INLINE picture #-}


-- | Generates an HTML @__\<pre\>__@ element with the given attributes and contents.
pre :: [Attribute ctx] -> [Html ctx] -> Html ctx
pre = ParentNode "<pre" "</pre>"
{-# INLINE pre #-}


-- | Generates an HTML @__\<progress\>__@ element with the given attributes and contents.
progress :: [Attribute ctx] -> [Html ctx] -> Html ctx
progress = ParentNode "<progress" "</progress>"
{-# INLINE progress #-}


-- | Generates an HTML @__\<q\>__@ element with the given attributes and contents.
q :: [Attribute ctx] -> [Html ctx] -> Html ctx
q = ParentNode "<q" "</q>"
{-# INLINE q #-}


-- | Generates an HTML @__\<rp\>__@ element with the given attributes and contents.
rp :: [Attribute ctx] -> [Html ctx] -> Html ctx
rp = ParentNode "<rp" "</rp>"
{-# INLINE rp #-}


-- | Generates an HTML @__\<rt\>__@ element with the given attributes and contents.
rt :: [Attribute ctx] -> [Html ctx] -> Html ctx
rt = ParentNode "<rt" "</rt>"
{-# INLINE rt #-}


-- | Generates an HTML @__\<ruby\>__@ element with the given attributes and contents.
ruby :: [Attribute ctx] -> [Html ctx] -> Html ctx
ruby = ParentNode "<ruby" "</ruby>"
{-# INLINE ruby #-}


-- | Generates an HTML @__\<s\>__@ element with the given attributes and contents.
s :: [Attribute ctx] -> [Html ctx] -> Html ctx
s = ParentNode "<s" "</s>"
{-# INLINE s #-}


-- | Generates an HTML @__\<samp\>__@ element with the given attributes and contents.
samp :: [Attribute ctx] -> [Html ctx] -> Html ctx
samp = ParentNode "<samp" "</samp>"
{-# INLINE samp #-}


-- | Generates an HTML @__\<script\>__@ element with the given attributes and contents.
script :: [Attribute ctx] -> [Html ctx] -> Html ctx
script = ParentNode "<script" "</script>"
{-# INLINE script #-}


-- | Generates an HTML @__\<search\>__@ element with the given attributes and contents.
search :: [Attribute ctx] -> [Html ctx] -> Html ctx
search = ParentNode "<search" "</search>"
{-# INLINE search #-}


-- | Generates an HTML @__\<section\>__@ element with the given attributes and contents.
section :: [Attribute ctx] -> [Html ctx] -> Html ctx
section = ParentNode "<section" "</section>"
{-# INLINE section #-}


-- | Generates an HTML @__\<select\>__@ element with the given attributes and contents.
select :: [Attribute ctx] -> [Html ctx] -> Html ctx
select = ParentNode "<select" "</select>"
{-# INLINE select #-}


-- | Generates an HTML @__\<slot\>__@ element with the given attributes and contents.
slot :: [Attribute ctx] -> [Html ctx] -> Html ctx
slot = ParentNode "<slot" "</slot>"
{-# INLINE slot #-}


-- | Generates an HTML @__\<small\>__@ element with the given attributes and contents.
small :: [Attribute ctx] -> [Html ctx] -> Html ctx
small = ParentNode "<small" "</small>"
{-# INLINE small #-}


-- | Generates an HTML @__\<source\>__@ element with the given attributes.
source :: [Attribute ctx] -> Html ctx
source = LeafNode "<source"
{-# INLINE source #-}


-- | Generates an HTML @__\<span\>__@ element with the given attributes and contents.
span :: [Attribute ctx] -> [Html ctx] -> Html ctx
span = ParentNode "<span" "</span>"
{-# INLINE span #-}


-- | Generates an HTML @__\<strong\>__@ element with the given attributes and contents.
strong :: [Attribute ctx] -> [Html ctx] -> Html ctx
strong = ParentNode "<strong" "</strong>"
{-# INLINE strong #-}


-- | Generates an HTML @__\<style\>__@ element with the given attributes and contents.
style :: [Attribute ctx] -> [Html ctx] -> Html ctx
style = ParentNode "<style" "</style>"
{-# INLINE style #-}


-- | Generates an HTML @__\<sub\>__@ element with the given attributes and contents.
sub :: [Attribute ctx] -> [Html ctx] -> Html ctx
sub = ParentNode "<sub" "</sub>"
{-# INLINE sub #-}


-- | Generates an HTML @__\<summary\>__@ element with the given attributes and contents.
summary :: [Attribute ctx] -> [Html ctx] -> Html ctx
summary = ParentNode "<summary" "</summary>"
{-# INLINE summary #-}


-- | Generates an HTML @__\<sup\>__@ element with the given attributes and contents.
sup :: [Attribute ctx] -> [Html ctx] -> Html ctx
sup = ParentNode "<sup" "</sup>"
{-# INLINE sup #-}


-- | Generates an HTML @__\<table\>__@ element with the given attributes and contents.
table :: [Attribute ctx] -> [Html ctx] -> Html ctx
table = ParentNode "<table" "</table>"
{-# INLINE table #-}


-- | Generates an HTML @__\<tbody\>__@ element with the given attributes and contents.
tbody :: [Attribute ctx] -> [Html ctx] -> Html ctx
tbody = ParentNode "<tbody" "</tbody>"
{-# INLINE tbody #-}


-- | Generates an HTML @__\<td\>__@ element with the given attributes and contents.
td :: [Attribute ctx] -> [Html ctx] -> Html ctx
td = ParentNode "<td" "</td>"
{-# INLINE td #-}


-- | Generates an HTML @__\<template\>__@ element with the given attributes and contents.
template :: [Attribute ctx] -> [Html ctx] -> Html ctx
template = ParentNode "<template" "</template>"
{-# INLINE template #-}


-- | Generates an HTML @__\<textarea\>__@ element with the given attributes and contents.
textarea :: [Attribute ctx] -> [Html ctx] -> Html ctx
textarea = ParentNode "<textarea" "</textarea>"
{-# INLINE textarea #-}


-- | Generates an HTML @__\<tfoot\>__@ element with the given attributes and contents.
tfoot :: [Attribute ctx] -> [Html ctx] -> Html ctx
tfoot = ParentNode "<tfoot" "</tfoot>"
{-# INLINE tfoot #-}


-- | Generates an HTML @__\<th\>__@ element with the given attributes and contents.
th :: [Attribute ctx] -> [Html ctx] -> Html ctx
th = ParentNode "<th" "</th>"
{-# INLINE th #-}


-- | Generates an HTML @__\<thead\>__@ element with the given attributes and contents.
thead :: [Attribute ctx] -> [Html ctx] -> Html ctx
thead = ParentNode "<thead" "</thead>"
{-# INLINE thead #-}


-- | Generates an HTML @__\<time\>__@ element with the given attributes and contents.
time :: [Attribute ctx] -> [Html ctx] -> Html ctx
time = ParentNode "<time" "</time>"
{-# INLINE time #-}


-- | Generates an HTML @__\<title\>__@ element with the given attributes and contents.
title :: [Attribute ctx] -> [Html ctx] -> Html ctx
title = ParentNode "<title" "</title>"
{-# INLINE title #-}


-- | Generates an HTML @__\<tr\>__@ element with the given attributes and contents.
tr :: [Attribute ctx] -> [Html ctx] -> Html ctx
tr = ParentNode "<tr" "</tr>"
{-# INLINE tr #-}


-- | Generates an HTML @__\<track\>__@ element with the given attributes.
track :: [Attribute ctx] -> Html ctx
track = LeafNode "<track"
{-# INLINE track #-}


-- | Generates an HTML @__\<u\>__@ element with the given attributes and contents.
u :: [Attribute ctx] -> [Html ctx] -> Html ctx
u = ParentNode "<u" "</u>"
{-# INLINE u #-}


-- | Generates an HTML @__\<ul\>__@ element with the given attributes and contents.
ul :: [Attribute ctx] -> [Html ctx] -> Html ctx
ul = ParentNode "<ul" "</ul>"
{-# INLINE ul #-}


-- | Generates an HTML @__\<var\>__@ element with the given attributes and contents.
var :: [Attribute ctx] -> [Html ctx] -> Html ctx
var = ParentNode "<var" "</var>"
{-# INLINE var #-}


-- | Generates an HTML @__\<video\>__@ element with the given attributes and contents.
video :: [Attribute ctx] -> [Html ctx] -> Html ctx
video = ParentNode "<video" "</video>"
{-# INLINE video #-}


-- | Generates an HTML @__\<wbr\>__@ element with the given attributes.
wbr :: [Attribute ctx] -> Html ctx
wbr = LeafNode "<wbr"
{-#INLINE wbr #-}
