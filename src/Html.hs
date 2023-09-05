{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The "Html" module provides a set of types, classes and functions for generating HTML elements.
--
-- These elements along with their attributes and event handlers, found in the "Html.Attributes" and "Html.Events" modules respectively, can
-- be used to dynamically compose HTML documents natively in Haskell, without relying on templating engines or other techniques that can be
-- error-prone and difficult to maintain.
--
-- Additionally, the functions provided in the "Html.Intl" module can be used to facilitate internationalization.
--
-- === Example
--
-- __Input:__
--
-- >Html.doctype
-- >    [ Html.html []
-- >        [ Html.head []
-- >            [ Html.meta
-- >                [ Attr.charset "UTF-8" ]
-- >            , Html.meta
-- >                [ Attr.name "viewport"
-- >                , Attr.content "width=device-width, initial-scale=1.0"
-- >                ]
-- >            , Html.title []
-- >                [ "Example Blog" ]
-- >            , Html.meta
-- >                [ Attr.rel "stylesheet"
-- >                , Attr.href "styles.css"
-- >                ]
-- >            ]
-- >        , Html.body []
-- >            [ Html.header []
-- >                [ Html.h1 []
-- >                    [ "Example Blog" ]
-- >                , Html.nav []
-- >                    [ Html.ul []
-- >                        [ Html.li []
-- >                            [ Html.a
-- >                                [ Attr.href "/" ]
-- >                                [ "Home" ]
-- >                            ]
-- >                        , Html.li []
-- >                            [ Html.a
-- >                                [ Attr.href "/about" ]
-- >                                [ "About" ]
-- >                            ]
-- >                        , Html.li []
-- >                            [ Html.a
-- >                                [ Attr.href "/posts" ]
-- >                                [ "Posts" ]
-- >                            ]
-- >                        , Html.li []
-- >                            [ Html.a
-- >                                [ Attr.href "/contact" ]
-- >                                [ "Contact" ]
-- >                            ]
-- >                        ]
-- >                    ]
-- >                ]
-- >            , Html.main []
-- >                [ Html.article []
-- >                    [ Html.header []
-- >                        [ Html.h2 []
-- >                            [ "Exploring the Great Outdoors" ]
-- >                        , Html.p []
-- >                            [ "Published on "
-- >                            , Html.time
-- >                                [ Attr.datetime "2023-09-05" ]
-- >                                [ "September 5, 2023" ]
-- >                            , " by John Doe"
-- >                            ]
-- >                        ]
-- >                    , Html.p []
-- >                        [ "Yesterday, I embarked on a thrilling hiking adventure in the heart of the Rocky Mountains..." ]
-- >                    ]
-- >                , Html.article []
-- >                    [ Html.header []
-- >                        [ Html.h2 []
-- >                            [ "A Culinary Journey Through Italy" ]
-- >                        , Html.p []
-- >                            [ "Published on "
-- >                            , Html.time
-- >                                [ Attr.datetime "2023-09-10" ]
-- >                                [ "September 10, 2023" ]
-- >                            , " by John Doe"
-- >                            ]
-- >                        ]
-- >                    , Html.p []
-- >                        [ "I recently had the chance to explore Italy's culinary delights..." ]
-- >                    ]
-- >                ]
-- >            , Html.footer []
-- >                [ Html.p []
-- >                    [ "&copy; 2023 Example Blog. All rights reserved." ]
-- >                ]
-- >            ]
-- >        ]
-- >    ]
--
-- __Output:__
--
-- ><!DOCTYPE html>
-- ><html>
-- >    <head>
-- >        <meta charset="UTF-8">
-- >        <meta name="viewport" content="width=device-width, initial-scale=1.0">
-- >        <title>Example Blog</title>
-- >        <meta rel="stylesheet" href="styles.css">
-- >    </head>
-- >    <body>
-- >        <header>
-- >            <h1>Example Blog</h1>
-- >            <nav>
-- >                <ul>
-- >                    <li><a href="/">Home</a></li>
-- >                    <li><a href="/about">About</a></li>
-- >                    <li><a href="/posts">Posts</a></li>
-- >                    <li><a href="/contact">Contact</a></li>
-- >                </ul>
-- >            </nav>
-- >        </header>
-- >        <main>
-- >            <article>
-- >                <header>
-- >                    <h2>Exploring the Great Outdoors</h2>
-- >                    <p>Published on <time datetime="2023-09-05">September 5, 2023</time> by John Doe</p>
-- >                </header>
-- >                <p>Yesterday, I embarked on a thrilling hiking adventure in the heart of the Rocky Mountains...</p>
-- >            </article>
-- >            <article>
-- >                <header>
-- >                    <h2>A Culinary Journey Through Italy</h2>
-- >                    <p>Published on <time datetime="2023-09-10">September 10, 2023</time> by John Doe</p>
-- >                </header>
-- >                <p>I recently had the chance to explore Italy's culinary delights...</p>
-- >            </article>
-- >        </main>
-- >        <footer>
-- >            <p>&copy; 2023 Example Blog. All rights reserved.</p>
-- >        </footer>
-- >    </body>
-- ></html>
--
-- /Note: All examples in this module assume the following imports:/
--
-- @
-- import Html (Html(..))
--
-- import qualified Html
-- import qualified Html.Attributes as Attr
-- import qualified Html.Events as Evnt
-- @
--
-- /Note: All example results in this module are formatted neatly for readability but are condensed in practice./
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


import Data.Bool (Bool(..))
import Data.Foldable (foldr)
import Data.Function (($), (.))
import Data.Monoid ((<>), mempty)
import Data.Semigroup (Semigroup(..))
import Data.Text.Lazy (unpack)
import Data.Text.Lazy.Builder (Builder, singleton, toLazyText)
import Text.Show (Show(..), showString)

import Data.String (IsString(..))


data Intl = Intl
    { de :: Builder
    , en :: Builder
    }


instance Translatable Intl where
    defaultLanguage = en


-- TYPES


-- | Represents an HTML element.
--
-- This data type can be used to generate HTML elements programmatically with the functions provided in the "Html" module.
--
-- The type variable /lng/ stands for /language/ and is intended to facilitate internationalization.
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
--
-- This data type can be used to generate HTML attributes programmatically with the functions provided in the "Html.Attributes" module.
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
--
-- A default language must be set to ensure proper conversion of 'Html.Html' to 'Data.Text.Lazy.Builder.Builder' and 'Data.String.String'
-- when generated without the use of 'Html.Intl.translate'.
class Translatable a where

    -- | Sets the default language to use for internationalization with 'Html.Html'.
    defaultLanguage :: a -> Builder


-- DECLARATIONS


-- | Generates an HTML /\<!DOCTYPE\>/ declaration with the given contents.
--
-- The /\<!DOCTYPE\>/ declaration defines the document type and version of the HTML being used. It ensures proper rendering by browsers and
-- sets the standard for the document's structure.
--
-- ==== __Example__
--
-- __Input:__
--
-- >Html.doctype
-- >    [ Html.html
-- >        [ Attr.lang "en" ]
-- >        [ Html.head []
-- >            [ Html.meta
-- >                [ Attr.charset "UTF-8" ]
-- >            , Html.meta
-- >                [ Attr.name "viewport"
-- >                , Attr.content "width=device-width, initial-scale=1.0"
-- >                ]
-- >            , Html.title []
-- >                [ "Example Company" ]
-- >            , Html.meta
-- >                [ Attr.rel "stylesheet"
-- >                , Attr.href "styles.css"
-- >                ]
-- >            ]
-- >        , Html.body []
-- >            [ Html.header []
-- >                [ Html.h1 []
-- >                    [ "Example Company" ]
-- >                , Html.nav []
-- >                    [ Html.ul []
-- >                        [ Html.li []
-- >                            [ Html.a
-- >                                [ Attr.href "/" ]
-- >                                [ "Home" ]
-- >                            ]
-- >                        , Html.li []
-- >                            [ Html.a
-- >                                [ Attr.href "/about" ]
-- >                                [ "About" ]
-- >                            ]
-- >                        , Html.li []
-- >                            [ Html.a
-- >                                [ Attr.href "/shop" ]
-- >                                [ "Shop" ]
-- >                            ]
-- >                        , Html.li []
-- >                            [ Html.a
-- >                                [ Attr.href "/contact" ]
-- >                                [ "Contact" ]
-- >                            ]
-- >                        ]
-- >                    ]
-- >                ]
-- >            , Html.main []
-- >                [ Html.section
-- >                    [ Attr.class_ "featured-products" ]
-- >                    [ Html.h2 []
-- >                        [ "Featured Products" ]
-- >                    , Html.div
-- >                        [ Attr.class_ "product" ]
-- >                        [ Html.img
-- >                            [ Attr.src "product1.jpg"
-- >                            , Attr.alt "Handcrafted Silver Necklace"
-- >                            ]
-- >                        , Html.h3 []
-- >                            [ "Silver Leaf Necklace" ]
-- >                        , Html.p
-- >                            [ Attr.class_ "price" ]
-- >                            [ "$75.00" ]
-- >                        , Html.a
-- >                            [ Attr.href "/shop/product1" ]
-- >                            [ "View Details" ]
-- >                        ]
-- >                    , Html.div
-- >                        [ Attr.class_ "product" ]
-- >                        [ Html.img
-- >                            [ Attr.src "product2.jpg"
-- >                            , Attr.alt "Gemstone Bracelet"
-- >                            ]
-- >                        , Html.h3 []
-- >                            [ "Gemstone Beaded Bracelet" ]
-- >                        , Html.p
-- >                            [ Attr.class_ "price" ]
-- >                            [ "$55.00" ]
-- >                        , Html.a
-- >                            [ Attr.href "/shop/product2" ]
-- >                            [ "View Details" ]
-- >                        ]
-- >                    ]
-- >                , Html.section
-- >                    [ Attr.class_ "about-us" ]
-- >                    [ Html.h2 []
-- >                        [ "About Us" ]
-- >                    , Html.p []
-- >                        [ "We are passionate about creating unique, handcrafted jewelry pieces..." ]
-- >                    ]
-- >                ]
-- >            , Html.footer []
-- >                [ Html.p []
-- >                    [ "&copy; 2023 Example Company. All rights reserved." ]
-- >                ]
-- >            ]
-- >        ]
-- >    ]
--
-- __Output:__
--
-- ><!DOCTYPE html>
-- ><html lang="en">
-- >    <head>
-- >        <meta charset="UTF-8">
-- >        <meta name="viewport" content="width=device-width, initial-scale=1.0">
-- >        <title>Example Company</title>
-- >        <link rel="stylesheet" href="styles.css">
-- >    </head>
-- >    <body>
-- >        <header>
-- >            <h1>Example Company</h1>
-- >            <nav>
-- >                <ul>
-- >                    <li><a href="/">Home</a></li>
-- >                    <li><a href="/about">About</a></li>
-- >                    <li><a href="/shop">Shop</a></li>
-- >                    <li><a href="/contact">Contact</a></li>
-- >                </ul>
-- >            </nav>
-- >        </header>
-- >        <main>
-- >            <section class="featured-products">
-- >                <h2>Featured Products</h2>
-- >                <div class="product">
-- >                    <img src="product1.jpg" alt="Handcrafted Silver Necklace">
-- >                    <h3>Silver Leaf Necklace</h3>
-- >                    <p class="price">$75.00</p>
-- >                    <a href="/shop/product1">View Details</a>
-- >                </div>
-- >                <div class="product">
-- >                    <img src="product2.jpg" alt="Gemstone Bracelet">
-- >                    <h3>Gemstone Beaded Bracelet</h3>
-- >                    <p class="price">$55.00</p>
-- >                    <a href="/shop/product2">View Details</a>
-- >                </div>
-- >            </section>
-- >            <section class="about-us">
-- >                <h2>About Us</h2>
-- >                <p>We are passionate about creating unique, handcrafted jewelry pieces...</p>
-- >            </section>
-- >        </main>
-- >        <footer>
-- >            <p>&copy; 2023 Example Company. All rights reserved.</p>
-- >        </footer>
-- >    </body>
-- ></html>
doctype :: [Html lng] -> Html lng
doctype = RootNode "<!DOCTYPE html>\n"
{-# INLINE doctype #-}


-- ELEMENTS


-- | Generates an HTML /\<a\>/ element with the given attributes and contents.
--
-- The /\<a\>/ element, or anchor element, is used to create hyperlinks that link to other web pages or resources. It defines the clickable
-- content that, when clicked, navigates to the URL specified by the /href/ attribute.
--
-- ==== __Example__
--
-- __Input:__
--
-- >Html.nav []
-- >    [ Html.ul []
-- >        [ Html.li []
-- >            [ Html.a
-- >                [ Attr.href "/" ]
-- >                [ "Home" ]
-- >            ]
-- >        , Html.li []
-- >            [ Html.a
-- >                [ Attr.href "/about.html" ]
-- >                [ "About" ]
-- >            ]
-- >        , Html.li []
-- >            [ Html.a
-- >                [ Attr.href "/contact.html" ]
-- >                [ "Contact" ]
-- >            ]
-- >        ]
-- >    ]
--
-- __Output:__
--
-- ><nav>
-- >    <ul>
-- >        <li><a href="/">Home</a></li>
-- >        <li><a href="/about.html">About</a></li>
-- >        <li><a href="/contact.html">Contact</a></li>
-- >    </ul>
-- ></nav>
a :: [Attribute] -> [Html lng] -> Html lng
a = ParentNode "<a" "</a>"
{-# INLINE a #-}


-- | Generates an HTML /\<abbr\>/ element with the given attributes and contents.
--
-- The /\<abbr\>/ element, or abbreviation element, is used to mark up an abbreviation or acronym in the text. It can include a /title/
-- attribute to provide the full or expanded form of the abbreviation when hovered over.
--
-- ==== __Example__
--
-- __Input:__
--
-- >Html.p []
-- >    [ "The "
-- >    , Html.abbr
-- >        [ Attr.title "Hypertext Markup Language" ]
-- >        [ "HTML" ]
-- >    , " standard revolutionized web development."
-- >    ]
--
-- __Output:__
--
-- ><p>The <abbr title="Hypertext Markup Language">HTML</abbr> standard revolutionized web development.</p>
abbr :: [Attribute] -> [Html lng] -> Html lng
abbr = ParentNode "<abbr" "</abbr>"
{-# INLINE abbr #-}


-- | Generates an HTML /\<address\>/ element with the given attributes and contents.
--
-- The /\<address\>/ element is used to provide contact information or author details for the nearest /\<article\>/ or /\<body\>/ ancestor.
-- It typically includes information such as names, addresses, emails, or phone numbers.
--
-- ==== __Example__
--
-- __Input:__
--
-- >Html.footer []
-- >    [ Html.address []
-- >        [ "For inquiries, please contact "
-- >        , Html.a
-- >            [ Attr.href "mailto:info@example.com" ]
-- >            [ "info@example.com" ]
-- >        , "."
-- >        ]
-- >    , Html.small []
-- >        [ "&copy; 2023 Example Company. All rights reserved." ]
-- >    ]
--
-- __Output:__
--
-- ><footer>
-- >    <address>
-- >        For inquiries, please contact <a href="mailto:contact@example.com">info@example.com</a>.
-- >    </address>
-- >    <p><small>&copy; 2023 Example Company. All rights reserved.</small></p>
-- ></footer>
address :: [Attribute] -> [Html lng] -> Html lng
address = ParentNode "<address" "</address>"
{-# INLINE address #-}


-- | Generates an HTML /\<area\>/ element with the given attributes.
--
-- The /\<area\>/ element is used within a /\<map\>/ element to define clickable areas within an image map. Each /\<area\>/ defines a
-- clickable region that links to a specific URL or performs an action when clicked.
--
-- ==== __Example__
--
-- __Input:__
--
-- >Html.p []
-- >    [ "Select one of the following planets to learn more:"
-- >    , Html.img
-- >        [ Attr.src "planets.jpg"
-- >        , Attr.alt "Planets"
-- >        , Attr.usemap "#planetmap"
-- >        ]
-- >    , Html.map
-- >        [ Attr.name "planetmap" ]
-- >        [ Html.area
-- >            [ Attr.shape "circle"
-- >            , Attr.coords "100,100,50"
-- >            , Attr.alt "Mercury"
-- >            , Attr.href "mercury.html"
-- >            ]
-- >        , Html.area
-- >            [ Attr.shape "circle"
-- >            , Attr.coords "200,200,50"
-- >            , Attr.alt "Venus"
-- >            , Attr.href "venus.html"
-- >            ]
-- >        , Html.area
-- >            [ Attr.shape "circle"
-- >            , Attr.coords "300,300,50"
-- >            , Attr.alt "Earth"
-- >            , Attr.href "earth.html"
-- >            ]
-- >        ]
-- >    ]
--
-- __Output:__
--
-- ><p>
-- >    Select one of the following planets to learn more:
-- >    <img src="planets.jpg" alt="Planets" usemap="#planetmap">
-- >    <map name="planetmap">
-- >        <area shape="circle" coords="100,100,50" alt="Mercury" href="mercury.html">
-- >        <area shape="circle" coords="200,200,50" alt="Venus" href="venus.html">
-- >        <area shape="circle" coords="300,300,50" alt="Earth" href="earth.html">
-- >    </map>
-- ></p>
area :: [Attribute] -> Html lng
area = LeafNode "<area"
{-# INLINE area #-}


-- | Generates an HTML /\<article\>/ element with the given attributes and contents.
--
-- The /\<article\>/ element represents a self-contained composition within a document. It encapsulates content that can be distributed or
-- reused independently, such as news articles, blog posts, or forum entries.
--
-- ==== __Example__
--
-- __Input:__
--
-- >Html.body []
-- >    [ Html.header []
-- >        [ Html.h1 []
-- >            [ "Example Blog" ]
-- >        ]
-- >    , Html.article []
-- >        [ Html.header []
-- >            [ Html.h1 []
-- >                [ "How to Start Your Own Garden" ]
-- >            , Html.p []
-- >                [ "Published on "
-- >                , Html.time
-- >                    [ Attr.datetime "2023-09-10" ]
-- >                    [ "September 10, 2023" ]
-- >                , " by Jane Doe"
-- >                ]
-- >            ]
-- >        , Html.p []
-- >            [ "If you've ever dreamed of having your own garden, follow these simple steps to get started." ]
-- >        , Html.ol []
-- >            [ Html.li []
-- >                [ "Choose a sunny spot in your backyard." ]
-- >            , Html.li []
-- >                [ "Prepare the soil." ]
-- >            , Html.li []
-- >                [ "Plant your favorite flowers and vegetables." ]
-- >            , Html.li []
-- >                [ "Water them regularly." ]
-- >            , Html.li []
-- >                [ "Watch your garden thrive!" ]
-- >            ]
-- >        , Html.footer []
-- >            [ Html.p []
-- >                [ "&copy; 2023 Example Company. All rights reserved." ]
-- >            ]
-- >        ]
-- >    , Html.footer []
-- >        [ Html.p []
-- >            [ "&copy; 2023 Example Blog. All rights reserved." ]
-- >        ]
-- >    ]
--
-- __Output:__
--
-- ><body>
-- >    <header>
-- >        <h1>Example Blog</h1>
-- >    </header>
-- >    <article>
-- >        <header>
-- >            <h2>How to Start Your Own Garden</h2>
-- >            <p>Published on <time datetime="2023-09-10">September 10, 2023</time> by Jane Doe</p>
-- >        </header>
-- >        <p>If you've ever dreamed of having your own garden, follow these simple steps to get started.</p>
-- >        <ol>
-- >            <li>Choose a sunny spot in your backyard.</li>
-- >            <li>Prepare the soil.</li>
-- >            <li>Plant your favorite flowers and vegetables.</li>
-- >            <li>Water them regularly.</li>
-- >            <li>Watch your garden thrive!</li>
-- >        </ol>
-- >        <footer>
-- >            <p>&copy; 2023 Example Company. All rights reserved.</p>
-- >        </footer>
-- >    </article>
-- >    <footer>
-- >        <p>&copy; 2023 Example Blog. All rights reserved.</p>
-- >    </footer>
-- ></body>
article :: [Attribute] -> [Html lng] -> Html lng
article = ParentNode "<article" "</article>"
{-# INLINE article #-}


-- | Generates an HTML /\<aside\>/ element with the given attributes and contents.
--
-- The /\<aside\>/ element represents content that is tangentially related to the main content of a page. It is often used for sidebars,
-- pull quotes, or advertisements.
--
-- ==== __Example__
--
-- __Input:__
--
-- >Html.body []
-- >    [ Html.header []
-- >        [ Html.h1 []
-- >            [ "Example Blog" ]
-- >        ]
-- >    , Html.article []
-- >        [ Html.header []
-- >            [ Html.h2 []
-- >                [ "10 Tips for Healthy Eating" ]
-- >            , Html.p []
-- >                [ "Published on "
-- >                , Html.time
-- >                    [ Attr.datetime "2023-09-15" ]
-- >                    [ "September 15, 2023" ]
-- >                , " by Jane Doe"
-- >                ]
-- >            ]
-- >        , Html.p []
-- >            [ "Healthy eating is essential for a balanced lifestyle. Here are ten tips to help you make nutritious choices:" ]
-- >        , Html.ol []
-- >            [ Html.li []
-- >                [ "Include a variety of fruits and vegetables in your diet." ]
-- >            , Html.li []
-- >                [ "Limit your intake of processed foods and sugary drinks." ]
-- >            , Html.li []
-- >                [ "Drink plenty of water throughout the day." ]
-- >            ]
-- >        , Html.footer []
-- >            [ Html.p []
-- >                [ "&copy; 2023 Example Company. All rights reserved." ]
-- >            ]
-- >        ]
-- >    , Html.aside []
-- >        [ Html.h3 []
-- >            [ "Advertisement" ]
-- >        , Html.p []
-- >            [ "Looking for healthy meal delivery options? Check out "
-- >            , Html.a
-- >                [ Attr.href "delivery@example.com" ]
-- >                [ "Example Delivery Service" ]
-- >            , "."
-- >            ]
-- >        ]
-- >    , Html.footer []
-- >        [ Html.p []
-- >            [ "&copy; 2023 Example Blog. All rights reserved." ]
-- >        ]
-- >    ]
--
-- __Output:__
--
-- ><body>
-- >    <header>
-- >        <h1>Example Blog</h1>
-- >    </header>
-- >    <article>
-- >        <header>
-- >            <h2>10 Tips for Healthy Eating</h2>
-- >            <p>Published on <time datetime="2023-09-15">September 15, 2023</time> by Jane Doe</p>
-- >        </header>
-- >        <p>Healthy eating is essential for a balanced lifestyle. Here are ten tips to help you make nutritious choices:</p>
-- >        <ol>
-- >            <li>Include a variety of fruits and vegetables in your diet.</li>
-- >            <li>Limit your intake of processed foods and sugary drinks.</li>
-- >            <li>Drink plenty of water throughout the day.</li>
-- >        </ol>
-- >        <footer>
-- >            <p>&copy; 2023 Example Company. All rights reserved.</p>
-- >        </footer>
-- >    </article>
-- >    <aside>
-- >        <h3>Advertisement</h3>
-- >        <p>Looking for healthy meal delivery options? Check out <a href="delivery@example.com">Example Delivery Service</a>.</p>
-- >    </aside>
-- >    <footer>
-- >        <p>&copy; 2023 Example Blog. All rights reserved.</p>
-- >    </footer>
-- ></body>
aside :: [Attribute] -> [Html lng] -> Html lng
aside = ParentNode "<aside" "</aside>"
{-# INLINE aside #-}


-- | Generates an HTML /\<audio\>/ element with the given attributes and contents.
--
-- The /\<audio\>/ element embeds audio content in a web page. It allows you to include audio files like music, podcasts, or sound effects
-- that users can play directly in their browsers.
--
-- ==== __Example__
--
-- __Input:__
--
-- >Html.audio
-- >    [ Attr.controls True ]
-- >    [ Html.source
-- >        [ Attr.src "ocean-waves.mp3"
-- >        , Attr.type_ "audio/mpeg"
-- >        ]
-- >    , "Your browser does not support the audio element."
-- >    ]
--
-- __Output:__
--
-- ><audio controls>
-- >    <source src="ocean-waves.mp3" type="audio/mpeg">
-- >    Your browser does not support the audio element.
-- ></audio>
audio :: [Attribute] -> [Html lng] -> Html lng
audio = ParentNode "<audio" "</audio>"
{-# INLINE audio #-}


-- | Generates an HTML /\<b\>/ element with the given attributes and contents.
--
-- The /\<b\>/ element, or bold element, is used to apply bold formatting to the enclosed text, indicating that the content should be
-- presented in a stronger or more prominent manner, without implying any specific semantic importance.
--
-- ==== __Example__
--
-- __Input:__
--
-- >Html.p []
-- >    [ Html.b []
-- >        [ "Chocolate" ]
-- >    , " and "
-- >    , Html.b []
-- >        [ "vanilla" ]
-- >    , " ice cream flavors are always a hit at our summer parties."
-- >    ]
--
-- __Output:__
--
-- ><p><b>Chocolate</b> and <b>vanilla</b> ice cream flavors are always a hit at our summer parties.</p>
b :: [Attribute] -> [Html lng] -> Html lng
b = ParentNode "<b" "</b>"
{-# INLINE b #-}


-- | Generates an HTML /\<base\>/ element with the given attributes.
--
-- The /\<base\>/ element specifies a base URL for relative URLs in a document. It helps browsers resolve relative URLs for assets like
-- images, stylesheets, and scripts.
--
-- ==== __Example__
--
-- __Input:__
--
-- >Html.doctype
-- >    [ Html.html []
-- >        [ Html.head []
-- >            [ Html.title []
-- >                [ "Example Newsletter" ]
-- >            , Html.base
-- >                [ Attr.href "https://www.example.com/newsletter/index.html" ]
-- >            ]
-- >        , Html.body []
-- >            [ Html.p []
-- >                [ "Browse the "
-- >                , Html.a
-- >                    [ Attr.href "archives.html" ]
-- >                    [ "archives" ]
-- >                , " for older issues."
-- >                ]
-- >            ]
-- >        ]
-- >    ]
--
-- __Output:__
--
-- ><!DOCTYPE html>
-- ><html>
-- >    <head>
-- >        <title>Example Newsletter</title>
-- >        <base href="https://www.example.com/newsletter/index.html">
-- >    </head>
-- >    <body>
-- >        <p>Browse the <a href="archives.html">archives</a> for older issues.</p>
-- >    </body>
-- ></html>
base :: [Attribute] -> Html lng
base = LeafNode "<base"
{-# INLINE base #-}


-- | Generates an HTML /\<bdi\>/ element with the given attributes and contents.
--
-- The /\<bdi\>/ element, or bidirectional isolation element, isolates a span of text that needs to be formatted in a specific direction for
-- languages that are written right-to-left, like Arabic or Hebrew, within a predominantly left-to-right text.
--
-- ==== __Example__
--
-- __Input:__
--
-- >Html.ul []
-- >    [ Html.li []
-- >        [ Html.bdi []
-- >            [ "Alice" ]
-- >        , ": 30 reviews."
-- >        ]
-- >    , Html.li []
-- >        [ Html.bdi []
-- >            [ "David" ]
-- >        , ": 15 reviews."
-- >        ]
-- >    , Html.li []
-- >        [ Html.bdi []
-- >            [ "محمد" ]
-- >        , ": 8 reviews."
-- >        ]
-- >    ]
--
-- __Output:__
--
-- ><ul>
-- >    <li><bdi>Alice</bdi>: 30 reviews.</li>
-- >    <li><bdi>David</bdi>: 15 reviews.</li>
-- >    <li><bdi>محمد</bdi>: 8 reviews.</li>
-- ></ul>
bdi :: [Attribute] -> [Html lng] -> Html lng
bdi = ParentNode "<bdi" "</bdi>"
{-# INLINE bdi #-}


-- | Generates an HTML /\<bdo\>/ element with the given attributes and contents.
--
-- The /\<bdo\>/ element, or bidirectional override element, overrides the bidirectional algorithm setting of the surrounding text. It is
-- used to explicitly define the text direction, either left-to-right or right-to-left, for languages with different writing directions.
--
-- ==== __Example__
--
-- __Input:__
--
-- >Html.section []
-- >    [ Html.h3 []
-- >        [ "Comments" ]
-- >    , Html.div
-- >        [ Attr.class_ "comment" ]
-- >        [ Html.p []
-- >            [ Html.bdo
-- >                [ Attr.dir "rtl" ]
-- >                [ "مشكورين على هذا الموقع الرائع!" ]
-- >            ]
-- >        , Html.p []
-- >            [ "Thank you for this wonderful website!" ]
-- >        ]
-- >    , Html.div
-- >        [ Attr.class_ "comment" ]
-- >        [ Html.p []
-- >            [ Html.bdo
-- >                [ Attr.dir "rtl" ]
-- >                [ "أنا ممتن جداً للمعلومات القيمة التي وفرتموها." ]
-- >            ]
-- >        , Html.p []
-- >            [ "I'm very grateful for the valuable information you've provided." ]
-- >        ]
-- >    ]
--
-- __Output:__
--
-- ><section>
-- >    <h3>Comments</h3>
-- >    <div class="comment">
-- >        <p><bdo dir="rtl">مشكورين على هذا الموقع الرائع!</bdo></p>
-- >        <p>Thank you for this wonderful website!</p>
-- >    </div>
-- >    <div class="comment">
-- >        <p><bdo dir="rtl">أنا ممتن جداً للمعلومات القيمة التي وفرتموها.</bdo></p>
-- >        <p>I'm very grateful for the valuable information you've provided.</p>
-- >    </div>
-- ></section>
bdo :: [Attribute] -> [Html lng] -> Html lng
bdo = ParentNode "<bdo" "</bdo>"
{-# INLINE bdo #-}


-- | Generates an HTML /\<blockquote\>/ element with the given attributes and contents.
--
-- The /\<blockquote\>/ element is used to indicate a block of quoted text from another source. It is often used to provide attributions
-- for cited content.
--
-- ==== __Example__
--
-- __Input:__
--
-- >Html.figure []
-- >    [ Html.blockquote []
-- >        [ Html.p []
-- >            [ "To be or not to be, that is the question." ]
-- >        ]
-- >    , Html.figcaption []
-- >        [ "William Shakespeare" ]
-- >    ]
--
-- __Output:__
--
-- ><figure>
-- >    <blockquote>
-- >        <p>To be or not to be, that is the question.</p>
-- >    </blockquote>
-- >    <figcaption>William Shakespeare</figcaption>
-- ></figure>
blockquote :: [Attribute] -> [Html lng] -> Html lng
blockquote = ParentNode "<blockquote" "</blockquote>"
{-# INLINE blockquote #-}


-- | Generates an HTML /\<body\>/ element with the given attributes and contents.
--
-- The /\<body\>/ element represents the main content of an HTML document. It contains the visible content that is displayed in the browser
-- window.
--
-- ==== __Example__
--
-- __Input:__
--
-- >Html.doctype
-- >    [ Html.html []
-- >        [ Html.head []
-- >            [ Html.title []
-- >                [ "Example Blog" ]
-- >            ]
-- >        , Html.body []
-- >            [ Html.header []
-- >                [ Html.h1 []
-- >                    [ "Macro Photography Tips" ]
-- >                ]
-- >            , Html.main []
-- >                [ Html.article []
-- >                    [ Html.h2 []
-- >                        [ "Introduction to Macro Photography" ]
-- >                    , Html.p []
-- >                        [ "Unlock the world of close-up photography with these valuable tips." ]
-- >                    ]
-- >                , Html.article []
-- >                    [ Html.h2 []
-- >                        [ "Choosing the Right Equipment" ]
-- >                    , Html.p []
-- >                        [ "Invest in a good macro lens and use a tripod for stability." ]
-- >                    ]
-- >                , Html.article []
-- >                    [ Html.h2 []
-- >                        [ "Lighting Techniques" ]
-- >                    , Html.p []
-- >                        [ "Experiment with natural light, diffusers, and reflectors." ]
-- >                    ]
-- >                , Html.article []
-- >                    [ Html.h2 []
-- >                        [ "Composition and Framing" ]
-- >                    , Html.p []
-- >                        [ "Learn how to compose your shots effectively." ]
-- >                    ]
-- >                , Html.article []
-- >                    [ Html.h2 []
-- >                        [ "Post-Processing Tips" ]
-- >                    , Html.p []
-- >                        [ "Enhance your macro photos using post-processing software." ]
-- >                    ]
-- >                ]
-- >            , Html.footer []
-- >                [ Html.p []
-- >                    [ "&copy; 2023 Example Blog. All rights reserved." ]
-- >                ]
-- >            ]
-- >        ]
-- >    ]
--
-- __Output:__
--
-- ><!DOCTYPE html>
-- ><html>
-- >    <head>
-- >        <title>Example Blog</title>
-- >    </head>
-- >    <body>
-- >        <header>
-- >            <h1>Macro Photography Tips</h1>
-- >        </header>
-- >        <main>
-- >            <article>
-- >                <h2>Introduction to Macro Photography</h2>
-- >                <p>Unlock the world of close-up photography with these valuable tips.</p>
-- >            </article>
-- >            <article>
-- >                <h2>Choosing the Right Equipment</h2>
-- >                <p>Invest in a good macro lens and use a tripod for stability.</p>
-- >            </article>
-- >            <article>
-- >                <h2>Lighting Techniques</h2>
-- >                <p>Experiment with natural light, diffusers, and reflectors.</p>
-- >            </article>
-- >            <article>
-- >                <h2>Composition and Framing</h2>
-- >                <p>Learn how to compose your shots effectively.</p>
-- >            </article>
-- >            <article>
-- >                <h2>Post-Processing Tips</h2>
-- >                <p>Enhance your macro photos using post-processing software.</p>
-- >            </article>
-- >        </main>
-- >        <footer>
-- >            <p>&copy; 2023 Example Blog. All rights reserved.</p>
-- >        </footer>
-- >    </body>
-- ></html>
body :: [Attribute] -> [Html lng] -> Html lng
body = ParentNode "<body" "</body>"
{-# INLINE body #-}


-- | Generates an HTML /\<br\>/ element with the given attributes.
--
-- The /\<br\>/ element, or break element, represents a line break, indicating that the content following it should appear on the next line.
-- It is used to create single-line breaks within text.
--
-- ==== __Example__
--
-- __Input:__
--
-- >Html.p []
-- >    [ "We're a cozy coffee shop in the heart of the city."
-- >    , Html.br []
-- >    , "Serving great coffee and homemade treats since 2005."
-- >    ]
--
-- __Output:__
--
-- ><p>
-- >    We're a cozy coffee shop in the heart of the city.<br>
-- >    Serving great coffee and homemade treats since 2005.
-- ></p>
br :: [Attribute] -> Html lng
br = LeafNode "<br"
{-# INLINE br #-}


-- | Generates an HTML /\<button\>/ element with the given attributes and contents.
--
-- The /\<button\>/ element represents a clickable button that can trigger actions or events when clicked by the user. It can be used for
-- various interactive purposes on a web page.
--
-- ==== __Example__
--
-- __Input:__
--
-- >Html.form
-- >    [ Attr.action "contact.php"
-- >    , Attr.method "post"
-- >    ]
-- >    [ Html.label
-- >        [ Attr.for "name" ]
-- >        [ "Name:" ]
-- >    , Html.input
-- >        [ Attr.type_ "text"
-- >        , Attr.id "name"
-- >        , Attr.name "name"
-- >        , Attr.required True
-- >        ]
-- >    , Html.label
-- >        [ Attr.for "email" ]
-- >        [ "Email:" ]
-- >    , Html.input
-- >        [ Attr.type_ "email"
-- >        , Attr.id "email"
-- >        , Attr.name "email"
-- >        , Attr.required True
-- >        ]
-- >    , Html.label
-- >        [ Attr.for "message" ]
-- >        [ "Message:" ]
-- >    , Html.input
-- >        [ Attr.type_ "text"
-- >        , Attr.id "message"
-- >        , Attr.name "message"
-- >        , Attr.required True
-- >        ]
-- >    , Html.button
-- >        [ Attr.type_ "submit" ]
-- >        [ "Submit" ]
-- >    ]
--
-- __Output:__
--
-- ><form action="contact.php" method="post">
-- >    <label for="name">Name:</label>
-- >    <input type="text" id="name" name="name" required>
-- >    <label for="email">Email:</label>
-- >    <input type="email" id="email" name="email" required>
-- >    <label for="message">Message:</label>
-- >    <input type="text" id="message" name="message" required>
-- >    <button type="submit">Submit</button>
-- ></form>
button :: [Attribute] -> [Html lng] -> Html lng
button = ParentNode "<button" "</button>"
{-# INLINE button #-}


-- | Generates an HTML /\<canvas\>/ element with the given attributes and contents.
--
-- The /\<canvas\>/ element provides a space within which graphics, animations, and drawings can be rendered using JavaScript. It offers a
-- versatile way to create dynamic visual content on a web page.
--
-- ==== __Example__
--
-- __Input:__
--
-- >Html.canvas
-- >    [ Attr.id "myCanvas"
-- >    , Attr.width  "200"
-- >    , Attr.height "100"
-- >    ]
-- >    []
--
-- __Output:__
--
-- ><canvas id="myCanvas" width="200" height="100"></canvas>
canvas :: [Attribute] -> [Html lng] -> Html lng
canvas = ParentNode "<canvas" "</canvas>"
{-# INLINE canvas #-}


-- | Generates an HTML /\<caption\>/ element with the given attributes and contents.
--
-- The /\<caption\>/ element is used to provide a title or description for a /\<table\>/ element. It is typically placed as the first child
-- within the /\<table\>/ element to provide context for the table\'s content.
--
-- ==== __Example__
--
-- __Input:__
--
-- >Html.table []
-- >    [ Html.caption []
-- >        [ "Monthly Sales Report" ]
-- >    , Html.tr []
-- >        [ Html.th []
-- >            [ "Month" ]
-- >        , Html.th []
-- >            [ "Sales" ]
-- >        ]
-- >    , Html.tr []
-- >        [ Html.td []
-- >            [ "January" ]
-- >        , Html.td []
-- >            [ "$10,000" ]
-- >        ]
-- >    ]
--
-- __Output:__
--
-- ><table>
-- >    <caption>Monthly Sales Report</caption>
-- >    <tr>
-- >        <th>Month</th>
-- >        <th>Sales</th>
-- >    </tr>
-- >    <tr>
-- >        <td>January</td>
-- >        <td>$10,000</td>
-- >    </tr>
-- ></table>
caption :: [Attribute] -> [Html lng] -> Html lng
caption = ParentNode "<caption" "</caption>"
{-# INLINE caption #-}


-- | Generates an HTML /\<cite\>/ element with the given attributes and contents.
--
-- The /\<cite\>/ element is used to mark a reference to a creative work, such as a book, movie, or song, within a text. It indicates the
-- title of the work and can be used for citations.
--
-- ==== __Example__
--
-- __Input:__
--
-- >Html.p []
-- >    [ Html.cite []
-- >        [ "The Great Gatsby" ]
-- >    , " by F. Scott Fitzgerald is a classic novel."
-- >    ]
--
-- __Output:__
--
-- ><p><cite>The Great Gatsby</cite> by F. Scott Fitzgerald is a classic novel.</p>
cite :: [Attribute] -> [Html lng] -> Html lng
cite = ParentNode "<cite" "</cite>"
{-# INLINE cite #-}


-- | Generates an HTML /\<code\>/ element with the given attributes and contents.
--
-- The /\<code\>/ element is used to represent a fragment of computer code within a document. It is typically used to display code examples,
-- snippets, or programming instructions.
--
-- ==== __Example__
--
-- __Input:__
--
-- >Html.p []
-- >    [ "To display a message in Python, use the "
-- >    , Html.code []
-- >        [ "print()" ]
-- >    , " function."
-- >    ]
--
-- __Output:__
--
-- ><p>To display a message in Python, use the <code>print()</code> function.</p>
code :: [Attribute] -> [Html lng] -> Html lng
code = ParentNode "<code" "</code>"
{-# INLINE code #-}


-- | Generates an HTML /\<col\>/ element with the given attributes.
--
-- The /\<col\>/ element is used to define properties for a group of columns within a /\<table\>/ element. It allows you to apply styling or
-- attributes to multiple columns at once.
--
-- ==== __Example__
--
-- __Input:__
--
-- >Html.table []
-- >    [ Html.colgroup []
-- >        [ Html.col
-- >            [ Attr.style "background-color: red;" ]
-- >        , Html.col
-- >            [ Attr.style "background-color: blue;" ]
-- >        ]
-- >    , Html.tr []
-- >        [ Html.th []
-- >            [ "Column 1" ]
-- >        , Html.th []
-- >            [ "Column 2" ]
-- >        ]
-- >    ]
--
-- __Output:__
--
-- ><table>
-- >    <colgroup>
-- >        <col style="background-color: lightblue;">
-- >        <col style="background-color: lightgreen;">
-- >    </colgroup>
-- >    <tr>
-- >        <td>Column 1</td>
-- >        <td>Column 2</td>
-- >    </tr>
-- ></table>
col :: [Attribute] -> Html lng
col = LeafNode "<col"
{-# INLINE col #-}


-- | Generates an HTML /\<colgroup\>/ element with the given attributes and contents.
--
-- The /\<colgroup\>/ element is used to group and define properties for one or more columns within a /\<table\>/ element. It is typically
-- used in conjunction with the /\<col\>/ element to apply styling or attributes to columns collectively.
--
-- ==== __Example__
--
-- __Input:__
--
-- >Html.table []
-- >    [ Html.colgroup []
-- >        [ Html.col
-- >            [ Attr.style "background-color: red;" ]
-- >        , Html.col
-- >            [ Attr.style "background-color: blue;" ]
-- >        ]
-- >    , Html.tr []
-- >        [ Html.th []
-- >            [ "Column 1" ]
-- >        , Html.th []
-- >            [ "Column 2" ]
-- >        ]
-- >    ]
--
-- __Output:__
--
-- ><table>
-- >    <colgroup>
-- >        <col style="background-color: lightblue;">
-- >        <col style="background-color: lightgreen;">
-- >    </colgroup>
-- >    <tr>
-- >        <td>Column 1</td>
-- >        <td>Column 2</td>
-- >    </tr>
-- ></table>
colgroup :: [Attribute] -> [Html lng] -> Html lng
colgroup = ParentNode "<colgroup" "</colgroup>"
{-# INLINE colgroup #-}


-- | Generates an HTML /\<data\>/ element with the given attributes and contents.
--
-- The /\<data\>/ element represents machine-readable data within the content, typically used to provide additional information that is not
-- meant for display but can be processed by scripts or applications.
--
-- ==== __Example__
--
-- __Input:__
--
-- >Html.p []
-- >    [ "The temperature is "
-- >    , Html.data_
-- >        [ Attr.value "25" ]
-- >        [ "25°C" ]
-- >    , " today."
-- >    ]
--
-- __Output:__
--
-- ><p>The temperature is <data value="25">25°C</data> today.</p>
data_ :: [Attribute] -> [Html lng] -> Html lng
data_ = ParentNode "<data" "</data>"
{-# INLINE data_ #-}


-- | Generates an HTML /\<datalist\>/ element with the given attributes and contents.
--
-- The /\<datalist\>/ element provides a predefined list of options that can be associated with an /\<input\>/ element\'s autocomplete
-- feature, making it easier for users to enter data.
--
-- ==== __Example__
--
-- __Input:__
--
-- >[ Html.label
-- >    [ Attr.for "browser" ]
-- >    [ "Choose a browser:" ]
-- >, Html.input
-- >    [ Attr.list "browsers"
-- >    , Attr.id "browser"
-- >    , Attr.name "browser"
-- >    ]
-- >, Html.datalist
-- >    [ Attr.id "browsers" ]
-- >    [ Html.option
-- >        [ Attr.value "Chrome" ] []
-- >    , Html.option
-- >        [ Attr.value "Firefox" ] []
-- >    , Html.option
-- >        [ Attr.value "Edge" ] []
-- >    , Html.option
-- >        [ Attr.value "Safari" ] []
-- >    ]
-- >]
--
-- __Output:__
--
-- ><label for="browser">Choose a browser:</label>
-- ><input list="browsers" id="browser" name="browser">
-- ><datalist id="browsers">
-- >    <option value="Chrome"></option>
-- >    <option value="Firefox"></option>
-- >    <option value="Edge"></option>
-- >    <option value="Safari"></option>
-- ></datalist>
datalist :: [Attribute] -> [Html lng] -> Html lng
datalist = ParentNode "<datalist" "</datalist>"
{-# INLINE datalist #-}


-- | Generates an HTML /\<dd\>/ element with the given attributes and contents.
--
-- The /\<dd\>/ element is used within a definition list (/\<dl\>/) to provide the description of definition of a term (/\<dt\>/). It is
-- commonly used to pair terms with their corresponding explanations.
--
-- ==== __Example__
--
-- __Input:__
--
-- >Html.dl []
-- >    [ Html.dt []
-- >        [ "HTML" ]
-- >    , Html.dd []
-- >        [ "HyperText Markup Language" ]
-- >    , Html.dt []
-- >        [ "CSS" ]
-- >    , Html.dd []
-- >        [ "Cascading Style Sheets" ]
-- >    ]
--
-- __Output:__
--
-- ><dl>
-- >    <dt>HTML</dt>
-- >    <dd>HyperText Markup Language</dd>
-- >    <dt>CSS</dt>
-- >    <dd>Cascading Style Sheets</dd>
-- ></dl>
dd :: [Attribute] -> [Html lng] -> Html lng
dd = ParentNode "<dd" "</dd>"
{-# INLINE dd #-}


-- | Generates an HTML /\<del\>/ element with the given attributes and contents.
--
-- The /\<del\>/ element represents text that has been deleted or removed from a document. It is often used to show changes in revisions or
-- edits.
--
-- ==== __Example__
--
-- __Input:__
--
-- >Html.p []
-- >    [ "Original price: "
-- >    , Html.del []
-- >        [ "$100" ]
-- >    , " "
-- >    , Html.ins []
-- >        [ "$80" ]
-- >    ]
--
-- __Output:__
--
-- ><p>Original price: <del>$100</del> <ins>$80</ins></p>
del :: [Attribute] -> [Html lng] -> Html lng
del = ParentNode "<del" "</del>"
{-# INLINE del #-}


-- | Generates an HTML /\<details\>/ element with the given attributes and contents.
--
-- The /\<details\>/ element represents a disclosure widget that allows users to view or hide additional content. It is often used to create
-- collapsible sections of information.
--
-- ==== __Example__
--
-- __Input:__
--
-- >Html.details []
-- >    [ Html.summary []
-- >        [ "Click to reveal more information" ]
-- >    , Html.p []
-- >        [ "This is additional content that can be shown or hidden." ]
-- >    ]
--
-- __Output:__
--
-- ><details>
-- >    <summary>Click to reveal more information</summary>
-- >    <p>This is additional content that can be shown or hidden.</p>
-- ></details>
details :: [Attribute] -> [Html lng] -> Html lng
details = ParentNode "<details" "</details>"
{-# INLINE details #-}


-- | Generates an HTML /\<dfn\>/ element with the given attributes and contents.
--
-- The /\<dfn\>/ element marks text that is being defined within a document, often indicating terms that are being introduced or explained.
--
-- ==== __Example__
--
-- __Input:__
--
-- >Html.p []
-- >    [ Html.dfn []
-- >        [ "AI" ]
-- >    , " refers to the simulation of human intelligence in computers."
-- >    ]
--
-- __Output:__
--
-- ><p><dfn>AI</dfn> refers to the simulation of human intelligence in computers.</p>
dfn :: [Attribute] -> [Html lng] -> Html lng
dfn = ParentNode "<dfn" "</dfn>"
{-# INLINE dfn #-}


-- | Generates an HTML /\<dialog\>/ element with the given attributes and contents.
--
-- The /\<dialog\>/ element represents a dialog box or modal window that can be used for interactive communication with the user. It is
-- often used for displaying alerts, messages, or user prompts.
--
-- ==== __Example__
--
-- __Input:__
--
-- >Html.dialog
-- >    [ Attr.open True ]
-- >    [ Html.p []
-- >        [ "This is a dialog box." ]
-- >    , Html.button []
-- >        [ "Close" ]
-- >    ]
--
-- __Output:__
--
-- ><dialog open>
-- >    <p>This is a dialog box.</p>
-- >    <button>Close</button>
-- ></dialog>
dialog :: [Attribute] -> [Html lng] -> Html lng
dialog = ParentNode "<dialog" "</dialog>"
{-# INLINE dialog #-}


-- | Generates an HTML /\<div\>/ element with the given attributes and contents.
--
-- The /\<div\>/ element is a generic container that is often used to group and structure content for styling or scripting purposes. It does
-- not inherently carry any specific meaning on its own.
--
-- ==== __Example__
--
-- __Input:__
--
-- >Html.div
-- >    [ Attr.class_ "container" ]
-- >    [ Html.h1 []
-- >        [ "Welcome" ]
-- >    , Html.p []
-- >        [ "This is a paragraph within a div." ]
-- >    ]
--
-- __Output:__
--
-- ><div class="container">
-- >  <h1>Welcome</h1>
-- >  <p>This is a paragraph within a div.</p>
-- ></div>
div :: [Attribute] -> [Html lng] -> Html lng
div = ParentNode "<div" "</div>"
{-# INLINE div #-}


-- | Generates an HTML /\<dl\>/ element with the given attributes and contents.
--
-- The /\<dl\>/ element represents a description list, consisting of terms (/\<dt\>/) and their corresponding descriptions (/\<dd\>/).
-- It is commonly used to present glossaries or sets of definitions.
--
-- ==== __Example__
--
-- __Input:__
--
-- >Html.dl []
-- >    [ Html.dt []
-- >        [ "HTML" ]
-- >    , Html.dd []
-- >        [ "HyperText Markup Language" ]
-- >    , Html.dt []
-- >        [ "CSS" ]
-- >    , Html.dd []
-- >        [ "Cascading Style Sheets" ]
-- >    ]
--
-- __Output:__
--
-- ><dl>
-- >    <dt>HTML</dt>
-- >    <dd>HyperText Markup Language</dd>
-- >    <dt>CSS</dt>
-- >    <dd>Cascading Style Sheets</dd>
-- ></dl>
dl :: [Attribute] -> [Html lng] -> Html lng
dl = ParentNode "<dl" "</dl>"
{-# INLINE dl #-}


-- | Generates an HTML /\<dt\>/ element with the given attributes and contents.
--
-- The /\<dt\>/ element is used withing a description list (/\<dl\>/) to define a term or name that is followed by its corresponding
-- description (/\<dd\>/).
--
-- ==== __Example__
--
-- __Input:__
--
-- >Html.dl []
-- >    [ Html.dt []
-- >        [ "HTML" ]
-- >    , Html.dd []
-- >        [ "HyperText Markup Language" ]
-- >    , Html.dt []
-- >        [ "CSS" ]
-- >    , Html.dd []
-- >        [ "Cascading Style Sheets" ]
-- >    ]
--
-- __Output:__
--
-- ><dl>
-- >    <dt>HTML</dt>
-- >    <dd>HyperText Markup Language</dd>
-- >    <dt>CSS</dt>
-- >    <dd>Cascading Style Sheets</dd>
-- ></dl>
dt :: [Attribute] -> [Html lng] -> Html lng
dt = ParentNode "<dt" "</dt>"
{-# INLINE dt #-}


-- | Generates an HTML /\<em\>/ element with the given attributes and contents.
--
-- The /\<em\>/ element is used to indicate text that should be emphasized, typically displayed in italic style. It carries semantic
-- meaning, implying that the enclosed content is of particular importance or should be stressed.
--
-- ==== __Example__
--
-- __Input:__
--
-- >Html.p []
-- >    [ Html.em []
-- >        [ "Important" ]
-- >    , " information should not be overlooked."
-- >    ]
--
-- __Output:__
--
-- ><p><em>Important</em> information should not be overlooked.</p>
em :: [Attribute] -> [Html lng] -> Html lng
em = ParentNode "<em" "</em>"
{-# INLINE em #-}


-- | Generates an HTML /\<embed\>/ element with the given attributes.
--
-- The /\<embed\>/ element embeds external content, typically multimedia like audio, video, or interactive applications, directly into a web
-- page. It is used to seamlessly integrate content from other sources.
--
-- ==== __Example__
--
-- __Input:__
--
-- >Html.embed
-- >    [ Attr.src "video.mp4"
-- >    , Attr.type_ "video/mp4"
-- >    , Attr.width "300"
-- >    , Attr.height "200"
-- >    ]
--
-- __Output:__
--
-- ><embed src="video.mp4" type="video/mp4" width="300" height="200">
embed :: [Attribute] -> Html lng
embed = LeafNode "<embed"
{-# INLINE embed #-}


-- | Generates an HTML /\<fieldset\>/ element with the given attributes and contents.
--
-- The /\<fieldset\>/ element is used to group related form elements together, providing a visual and logical grouping for better
-- organization and styling. It is often used in forms to structure related input elements.
--
-- ==== __Example__
--
-- __Input:__
--
-- >Html.fieldset []
-- >    [ Html.legend []
-- >        [ "Contact Information" ]
-- >    , Html.label
-- >        [ Attr.for "name" ]
-- >        [ "Name:" ]
-- >    , Html.input
-- >        [ Attr.type_ "text"
-- >        , Attr.id "name"
-- >        ]
-- >    , Html.label
-- >        [ Attr.for "email" ]
-- >        [ "Email:" ]
-- >    , Html.input
-- >        [ Attr.type_ "email"
-- >        , Attr.id "email"
-- >        ]
-- >    ]
--
-- __Output:__
--
-- ><fieldset>
-- >    <legend>Contact Information</legend>
-- >    <label for="name">Name:</label>
-- >    <input type="text" id="name">
-- >    <label for="email">Email:</label>
-- >    <input type="email" id="email">
-- ></fieldset>
fieldset :: [Attribute] -> [Html lng] -> Html lng
fieldset = ParentNode "<fieldset" "</fieldset>"
{-# INLINE fieldset #-}


-- | Generates an HTML /\<figcaption\>/ element with the given attributes and contents.
--
-- The /\<figcaption\>/ element is used to provide a caption or description for a /\<figure\>/ element, typically used to describe images,
-- illustrations, or multimedia content.
--
-- ==== __Example__
--
-- __Input:__
--
-- >Html.figure []
-- >    [ Html.img
-- >        [ Attr.src "image.jpg"
-- >        , Attr.alt "A beautiful landscape"
-- >        ]
-- >    , Html.figcaption []
-- >        [ "A breathtaking landscape." ]
-- >    ]
--
-- __Output:__
--
-- ><figure>
-- >    <img src="image.jpg" alt="A beautiful landscape">
-- >    <figcaption>A breathtaking landscape.</figcaption>
-- ></figure>
figcaption :: [Attribute] -> [Html lng] -> Html lng
figcaption = ParentNode "<figcaption" "</figcaption>"
{-# INLINE figcaption #-}


-- | Generates an HTML /\<figure\>/ element with the given attributes and contents.
--
-- The /\<figure\>/ element is used to encapsulate and group related content, often used with an associated /\<figcaption\>/ to provide
-- context or explanation for the content within.
--
-- ==== __Example__
--
-- __Input:__
--
-- >Html.figure []
-- >    [ Html.img
-- >        [ Attr.src "image.jpg"
-- >        , Attr.alt "An illustration of a city"
-- >        ]
-- >    , Html.figcaption []
-- >        [ "An artistic representation of a city skyline." ]
-- >    ]
--
-- __Output:__
--
-- ><figure>
-- >    <img src="image.jpg" alt="An illustration of a city">
-- >    <figcaption>An artistic representation of a city skyline.</figcaption>
-- ></figure>
figure :: [Attribute] -> [Html lng] -> Html lng
figure = ParentNode "<figure" "</figure>"
{-# INLINE figure #-}


-- | Generates an HTML /\<footer\>/ element with the given attributes and contents.
--
-- The /\<footer\>/ element represents a footer section or container typically used to include information such as authorship, copyright
-- details, or contact information at the bottom of a document or section.
--
-- ==== __Example__
--
-- __Input:__
--
-- >Html.footer []
-- >    [ Html.p []
-- >        [ "&copy; 2023 My Website. All rights reserved." ]
-- >    ]
--
-- __Output:__
--
-- ><footer>
-- >    <p>&copy; 2023 My Website. All rights reserved.</p>
-- ></footer>
footer :: [Attribute] -> [Html lng] -> Html lng
footer = ParentNode "<footer" "</footer>"
{-# INLINE footer #-}


-- | Generates an HTML /\<form\>/ element with the given attributes and contents.
--
-- The /\<form\>/ element is used to create a container for a set of form controls like input fields, checkboxes, radio buttons, and
-- buttons. It is used to collect user input that can be submitted to a server for processing.
--
-- ==== __Example__
--
-- __Input:__
--
-- >Html.form
-- >    [ Attr.action "submit.php"
-- >    , Attr.method "post"
-- >    ]
-- >    [ Html.label
-- >        [ Attr.for "name" ]
-- >        [ "Name:" ]
-- >    , Html.input
-- >        [ Attr.type_ "text"
-- >        , Attr.id "name"
-- >        , Attr.name "name"
-- >        , Attr.required True
-- >        ]
-- >    , Html.label
-- >        [ Attr.for "email" ]
-- >        [ "Email:" ]
-- >    , Html.input
-- >        [ Attr.type_ "email"
-- >        , Attr.id "email"
-- >        , Attr.name "email"
-- >        , Attr.required True
-- >        ]
-- >    , Html.button
-- >        [ Attr.type_ "submit" ]
-- >        [ "Submit" ]
-- >    ]
--
-- __Output:__
--
-- ><form action="submit.php" method="post">
-- >    <label for="name">Name:</label>
-- >    <input type="text" id="name" name="name" required>
-- >    <label for="email">Email:</label>
-- >    <input type="email" id="email" name="email" required>
-- >    <button type="submit">Submit</button>
-- ></form>
form :: [Attribute] -> [Html lng] -> Html lng
form = ParentNode "<form" "</form>"
{-# INLINE form #-}


-- | Generates an HTML /\<h1\>/ element with the given attributes and contents.
--
-- The /\<h1\>/ element represents the highest-level heading, typically used to indicate the main topic or section of a document.
--
-- ==== __Example__
--
-- __Input:__
--
-- >Html.h1 []
-- >    [ "Welcome to Our Website" ]
--
-- __Output:__
--
-- ><h1>Welcome to Our Website</h1>
h1 :: [Attribute] -> [Html lng] -> Html lng
h1 = ParentNode "<h1" "</h1>"
{-# INLINE h1 #-}


-- | Generates an HTML /\<h2\>/ element with the given attributes and contents.
--
-- The /\<h2\>/ element signifies a heading of a slightly lower level than /\<h1\>/, often used to subdivide content within sections of a
-- document.
--
-- ==== __Example__
--
-- __Input:__
--
-- >[ Html.h2 []
-- >    [ "About Us" ]
-- >, Html.p []
-- >    [ "Learn more about our company's history and mission." ]
-- >]
--
-- __Output:__
--
-- ><h2>About Us</h2>
-- ><p>Learn more about our company's history and mission.</p>
h2 :: [Attribute] -> [Html lng] -> Html lng
h2 = ParentNode "<h2" "</h2>"
{-# INLINE h2 #-}


-- | Generates an HTML /\<h3\>/ element with the given attributes and contents.
--
-- The /\<h3\>/ element denotes a heading that is of a lower level than /\<h2\>/, typically used to indicate sub-sections or finer details
-- within the content.
--
-- ==== __Example__
--
-- __Input:__
--
-- >[ Html.h3 []
-- >    [ "Services" ]
-- >, Html.ul []
-- >    [ Html.li []
-- >        [ "Web Design" ]
-- >    , Html.li []
-- >        [ "Graphic Design" ]
-- >    , Html.li []
-- >        [ "Content Writing" ]
-- >    ]
-- >]
--
-- __Output:__
--
-- ><h3>Services</h3>
-- ><ul>
-- >    <li>Web Design</li>
-- >    <li>Graphic Design</li>
-- >    <li>Content Writing</li>
-- ></ul>
h3 :: [Attribute] -> [Html lng] -> Html lng
h3 = ParentNode "<h3" "</h3>"
{-# INLINE h3 #-}


-- | Generates an HTML /\<h4\>/ element with the given attributes and contents.
--
-- The /\<h4\>/ element represents a heading with a lower hierarchical level than /\<h3\>/, usually utilized to introduce sub-subsections or
-- finer points within the content.
--
-- ==== __Example__
--
-- __Input:__
--
-- >[ Html.h4 []
-- >    [ "Web Design Packages" ]
-- >, Html.p []
-- >    [ "Choose from a variety of web design packages." ]
-- >]
--
-- __Output:__
--
-- ><h4>Web Design Packages</h4>
-- ><p>Choose from a variety of web design packages.</p>
h4 :: [Attribute] -> [Html lng] -> Html lng
h4 = ParentNode "<h4" "</h4>"
{-# INLINE h4 #-}


-- | Generates an HTML /\<h5\>/ element with the given attributes and contents.
--
-- The /\<h5\>/ element signifies a heading of a reduced level compared to /\<h4\>/, often used to introduce even more specific details or
-- subsections within the content.
--
-- ==== __Example__
--
-- __Input:__
--
-- >[ Html.h5 []
-- >    [ "Package Features" ]
-- >, Html.ul []
-- >    [ Html.li []
-- >        [ "Responsive Design" ]
-- >    , Html.li []
-- >        [ "Customization Options" ]
-- >    ]
-- >]
--
-- __Output:__
--
-- ><h5>Package Features</h5>
-- ><ul>
-- >    <li>Responsive Design</li>
-- >    <li>Customization Options</li>
-- ></ul>
h5 :: [Attribute] -> [Html lng] -> Html lng
h5 = ParentNode "<h5" "</h5>"
{-# INLINE h5 #-}


-- | Generates an HTML /\<h6\>/ element with the given attributes and contents.
--
-- The /\<h6\>/ element defines the lowest-level heading among the heading elements, typically employed for the most specific details or
-- subsections within the content.
--
-- ==== __Example__
--
-- __Input:__
--
-- >[ Html.h6 []
-- >    [ "Customization Options" ]
-- >, Html.p []
-- >    [ "Choose colors, fonts, and layouts for your website." ]
-- >]
--
-- __Output:__
--
-- ><h6>Customization Options</h6>
-- ><p>Choose colors, fonts, and layouts for your website.</p>
h6 :: [Attribute] -> [Html lng] -> Html lng
h6 = ParentNode "<h6" "</h6>"
{-# INLINE h6 #-}


-- | Generates an HTML /\<head\>/ element with the given attributes and contents.
--
-- The /\<head\>/ element serves as a container for metadata and other non-visible information about the document, such as title, character
-- encoding, and linked stylesheets.
--
-- ==== __Example__
--
-- __Input:__
--
-- >Html.doctype
-- >    [ Html.html []
-- >        [ Html.head []
-- >            [ Html.meta
-- >                [ Attr.charset "UTF-8" ]
-- >            , Html.title []
-- >                [ "My Website" ]
-- >            , Html.link
-- >                [ Attr.rel "stylesheet"
-- >                , Attr.href "styles.css"
-- >                ]
-- >            ]
-- >        , Html.body []
-- >            [ Html.h1 []
-- >                [ "Welcome" ]
-- >            , Html.p []
-- >                [ "This is the main content." ]
-- >            ]
-- >        ]
-- >    ]
--
-- __Output:__
--
-- ><!DOCTYPE html>
-- ><html>
-- >    <head>
-- >        <meta charset="UTF-8">
-- >        <title>My Website</title>
-- >        <link rel="stylesheet" href="styles.css">
-- >    </head>
-- >    <body>
-- >        <h1>Welcome</h1>
-- >        <p>This is the main content.</p>
-- >    </body>
-- ></html>
head :: [Attribute] -> [Html lng] -> Html lng
head = ParentNode "<head" "</head>"
{-# INLINE head #-}


-- | Generates an HTML /\<header\>/ element with the given attributes and contents.
--
-- The /\<header\>/ element represents a container for introductory content or a group of navigation and branding elements typically found
-- at the top of a section or page.
--
-- ==== __Example__
--
-- __Input:__
--
-- >Html.header []
-- >    [ Html.h1 []
-- >        [ "My Website" ]
-- >    , Html.nav []
-- >        [ Html.a
-- >            [ Attr.href "home.html" ]
-- >            [ "Home" ]
-- >        , Html.a
-- >            [ Attr.href "about.html" ]
-- >            [ "About" ]
-- >        , Html.a
-- >            [ Attr.href "contact.html" ]
-- >            [ "Contact" ]
-- >        ]
-- >    ]
--
-- __Output:__
--
-- ><header>
-- >    <h1>My Website</h1>
-- >    <nav>
-- >        <a href="home.html">Home</a>
-- >        <a href="about.html">About</a>
-- >        <a href="contact.html">Contact</a>
-- >    </nav>
-- ></header>
header :: [Attribute] -> [Html lng] -> Html lng
header = ParentNode "<header" "</header>"
{-# INLINE header #-}


-- | Generates an HTML /\<hgroup\>/ element with the given attributes and contents.
--
-- The /\<hgroup\>/ element groups together multiple heading elements (/\<h1\>/ to /\<h6\>/) as a single entity, often used to create a
-- heading with a subheading or a title with a subtitle.
--
-- ==== __Example__
--
-- __Input:__
--
-- >Html.hgroup []
-- >    [ Html.h1 []
-- >        [ "Web Design Services" ]
-- >    , Html.h2 []
-- >        [ "Creating Stunning Websites" ]
-- >    ]
--
-- __Output:__
--
-- ><hgroup>
-- >    <h1>Web Design Services</h1>
-- >    <h2>Creating Stunning Websites</h2>
-- ></hgroup>
hgroup :: [Attribute] -> [Html lng] -> Html lng
hgroup = ParentNode "<hgroup" "</hgroup>"
{-# INLINE hgroup #-}


-- | Generates an HTML /\<hr\>/ element with the given attributes.
--
-- The /\<hr\>/ element is a self-closing tag that represents a thematic break or separation between content, typically displayed as a
-- horizontal line.
--
-- ==== __Example__
--
-- __Input:__
--
-- >[ Html.p []
-- >    [ "This is some text." ]
-- >, Html.hr []
-- >, Html.p []
-- >    [ "This is more text after the horizontal rule." ]
-- >]
--
-- __Output:__
--
-- ><p>This is some text.</p>
-- ><hr>
-- ><p>This is more text after the horizontal rule.</p>
hr :: [Attribute] -> Html lng
hr = LeafNode "<hr"
{-# INLINE hr #-}


-- | Generates an HTML /\<html\>/ element with the given attributes and contents.
--
-- The /\<html\>/ element encloses the entire HTML document and serves as the root element, containing all other HTML elements like
-- /\<head\>/ and /\<body\>/.
--
-- ==== __Example__
--
-- __Input:__
--
-- >Html.doctype
-- >    [ Html.html []
-- >        [ Html.head []
-- >            [ Html.meta
-- >                [ Attr.charset "UTF-8" ]
-- >            , Html.title []
-- >                [ "My Website" ]
-- >            ]
-- >        , Html.body []
-- >            [ Html.h1 []
-- >                [ "Welcome" ]
-- >            , Html.p []
-- >                [ "This is the main content." ]
-- >            ]
-- >        ]
-- >    ]
--
-- __Output:__
--
-- ><!DOCTYPE html>
-- ><html>
-- >    <head>
-- >        <meta charset="UTF-8">
-- >        <title>My Website</title>
-- >    </head>
-- >    <body>
-- >        <h1>Welcome</h1>
-- >        <p>This is the main content.</p>
-- >    </body>
-- ></html>
html :: [Attribute] -> [Html lng] -> Html lng
html = ParentNode "<html" "</html>"
{-# INLINE html #-}


-- | Generates an HTML /\<i\>/ element with the given attributes and contents.
--
-- The /\<i\>/ element is used to apply italics to text, indicating that the content within should be styled in an italic font.
--
-- ==== __Example__
--
-- __Input:__
--
-- >Html.p []
-- >    [ "This is "
-- >    , Html.i []
-- >        [ "italicized" ]
-- >    , " text."
-- >    ]
--
-- __Output:__
--
-- ><p>This is <i>italicized</i> text.</p>
i :: [Attribute] -> [Html lng] -> Html lng
i = ParentNode "<i" "</i>"
{-# INLINE i #-}


-- | Generates an HTML /\<iframe\>/ element with the given attributes and contents.
--
-- The /\<iframe\>/ element embeds another HTML document within the current document, allowing for the display of external content such as
-- web pages or multimedia.
--
-- ==== __Example__
--
-- __Input:__
--
-- >Html.iframe
-- >    [ Attr.src "https://www.example.com" ]
-- >    []
--
-- __Output:__
--
-- ><iframe src="https://www.example.com"></iframe>
iframe :: [Attribute] -> [Html lng] -> Html lng
iframe = ParentNode "<iframe" "</iframe>"
{-# INLINE iframe #-}


-- | Generates an HTML /\<img\>/ element with the given attributes.
--
-- The /\<img\>/ element embeds an image in the document, displaying the visual content specified by the /src/ attribute.
--
-- ==== __Example__
--
-- __Input:__
--
-- >Html.img
-- >    [ Attr.src "image.jpg"
-- >    , Attr.alt "A beautiful sunset"
-- >    ]
--
-- __Output:__
--
-- ><img src="image.jpg" alt="A beautiful sunset">
img :: [Attribute] -> Html lng
img = LeafNode "<img"
{-# INLINE img #-}


-- | Generates an HTML /\<input\>/ element with the given attributes.
--
-- The /\<input\>/ element creates a user-input field, such as a text box, radio button, checkbox, or submit button, allowing users to enter
-- or select data.
--
-- ==== __Example__
--
-- __Input:__
--
-- >Html.input
-- >    [ Attr.type_ "text"
-- >    , Attr.id "username"
-- >    , Attr.name "username"
-- >    , Attr.placeholder "Enter your username"
-- >    ]
--
-- __Output:__
--
-- ><input type="text" id="username" name="username" placeholder="Enter your username">
input :: [Attribute] -> Html lng
input = LeafNode "<input"
{-# INLINE input #-}


-- | Generates an HTML /\<ins\>/ element with the given attributes and contents.
--
-- The /\<ins\>/ element represents text that has been inserted into the document after the original content, often displayed with an
-- underline.
--
-- ==== __Example__
--
-- __Input:__
--
-- >Html.p []
-- >    [ "Check out our "
-- >    , Html.ins []
-- >        [ "new" ]
-- >    , " collection of products."
-- >    ]
--
-- __Output:__
--
-- ><p>Check out our <ins>new</ins> collection of products.</p>
ins :: [Attribute] -> [Html lng] -> Html lng
ins = ParentNode "<ins" "</ins>"
{-# INLINE ins #-}


-- | Generates an HTML /\<kbd\>/ element with the given attributes and contents.
--
-- The /\<kbd\>/ element is used to indicate user input, typically keyboard input, within the content, often rendering the enclosed text in
-- a monospace font.
--
-- ==== __Example__
--
-- __Input:__
--
-- >Html.p []
-- >    [ "To save, press "
-- >    , Html.kbd []
-- >        [ "Ctrl" ]
-- >    , " + "
-- >    , Html.kbd []
-- >        [ "S" ]
-- >    , "."
-- >    ]
--
-- __Output:__
--
-- ><p>To save, press <kbd>Ctrl</kbd> + <kbd>S</kbd>.</p>
kbd :: [Attribute] -> [Html lng] -> Html lng
kbd = ParentNode "<kbd" "</kbd>"
{-# INLINE kbd #-}


-- | Generates an HTML /\<label\>/ element with the given attributes and contents.
--
-- The /\<label\>/ element associates a text label with a form control, enhancing usability and accessibility by providing a descriptive
-- label for user interaction.
--
-- ==== __Example__
--
-- __Input:__
--
-- >[ Html.label
-- >    [ Attr.for "username" ]
-- >    [ "Username:" ]
-- >, Html.input
-- >    [ Attr.type_ "text"
-- >    , Attr.id "username"
-- >    , Attr.name "username"
-- >    ]
-- >]
--
-- __Output:__
--
-- ><label for="username">Username:</label>
-- ><input type="text" id="username" name="username">
label :: [Attribute] -> [Html lng] -> Html lng
label = ParentNode "<label" "</label>"
{-# INLINE label #-}


-- | Generates an HTML /\<legend\>/ element with the given attributes and contents.
--
-- The /\<legend\>/ element provides a caption or title for a /\<fieldset\>/ element, offering a concise description or heading for the
-- group of related form controls within the fieldset.
--
-- ==== __Example__
--
-- __Input:__
--
-- >Html.fieldset []
-- >    [ Html.legend []
-- >        [ "Contact Information" ]
-- >    , Html.label
-- >        [ Attr.for "name" ]
-- >        [ "Name:" ]
-- >    , Html.input
-- >        [ Attr.type_ "text"
-- >        , Attr.id "name"
-- >        , Attr.name "name"
-- >        ]
-- >    ]
--
-- __Output:__
--
-- ><fieldset>
-- >    <legend>Contact Information</legend>
-- >    <label for="name">Name:</label>
-- >    <input type="text" id="name" name="name">
-- ></fieldset>
legend :: [Attribute] -> [Html lng] -> Html lng
legend = ParentNode "<legend" "</legend>"
{-# INLINE legend #-}


-- | Generates an HTML /\<li\>/ element with the given attributes and contents.
--
-- The /\<li\>/ element defines a list item within an ordered (/\<ol\>/) or unordered (/\<ul\>/) list, representing an individual entry or
-- point in the list.
--
-- ==== __Example__
--
-- __Input:__
--
-- >Html.ul []
-- >    [ Html.li []
-- >        [ "Item 1" ]
-- >    , Html.li []
-- >        [ "Item 2" ]
-- >    ]
--
-- __Output:__
--
-- ><ul>
-- >    <li>Item 1</li>
-- >    <li>Item 2</li>
-- ></ul>
li :: [Attribute] -> [Html lng] -> Html lng
li = ParentNode "<li" "</li>"
{-# INLINE li #-}


-- | Generates an HTML /\<link\>/ element with the given attributes.
--
-- The /\<link\>/ element associates external resources, typically stylesheets, with the document, enabling the application of additional
-- styles and behaviors to the content.
--
-- ==== __Example__
--
-- __Input:__
--
-- >Html.head []
-- >    [ Html.link
-- >        [ Attr.rel "stylesheet"
-- >        , Attr.href "styles.css"
-- >        ]
-- >    ]
--
-- __Output:__
--
-- ><head>
-- >    <link rel="stylesheet" href="styles.css">
-- ></head>
link :: [Attribute] -> Html lng
link = LeafNode "<link"
{-# INLINE link #-}


-- | Generates an HTML /\<main\>/ element with the given attributes and contents.
--
-- The /\<main\>/ element indicates the main content of the document, providing a distinct region for the central subject matter of the
-- webpage.
--
-- ==== __Example__
--
-- __Input:__
--
-- >Html.main []
-- >    [ Html.h1 []
-- >        [ "Welcome to Our Website" ]
-- >    , Html.p []
-- >        [ "This is the main content of the page." ]
-- >    ]
--
-- __Output:__
--
-- ><main>
-- >    <h1>Welcome to Our Website</h1>
-- >    <p>This is the main content of the page.</p>
-- ></main>
main :: [Attribute] -> [Html lng] -> Html lng
main = ParentNode "<main" "</main>"
{-# INLINE main #-}


-- | Generates an HTML /\<map\>/ element with the given attributes and contents.
--
-- The /\<map\>/ element defines an image map, which is used to associate clickable areas within an image to specific links or actions.
--
-- ==== __Example__
--
-- __Input:__
--
-- >[ Html.img
-- >    [ Attr.src "worldmap.png"
-- >    , Attr.alt "World Map"
-- >    , Attr.usemap "#worldmap"
-- >    ]
-- >, Html.map
-- >    [ Attr.name "worldmap" ]
-- >    [ Html.area
-- >        [ Attr.shape "rect"
-- >        , Attr.coords "0,0,200,200"
-- >        , Attr.alt "North America"
-- >        , Attr.href "northamerica.html"
-- >        ]
-- >    , Html.area
-- >        [ Attr.shape "rect"
-- >        , Attr.coords "200,0,400,200"
-- >        , Attr.alt "Europe"
-- >        , Attr.href "europe.html"
-- >        ]
-- >    ]
-- >]
--
-- __Output:__
--
-- ><img src="worldmap.png" alt="World Map" usemap="#worldmap">
-- ><map name="worldmap">
-- >    <area shape="rect" coords="0,0,200,200" alt="North America" href="northamerica.html">
-- >    <area shape="rect" coords="200,0,400,200" alt="Europe" href="europe.html">
-- ></map>
map :: [Attribute] -> [Html lng] -> Html lng
map = ParentNode "<map" "</map>"
{-# INLINE map #-}


-- | Generates an HTML /\<mark\>/ element with the given attributes and contents.
--
-- The /\<mark\>/ element highlights text as if it has been marked for reference or emphasis, often rendering the enclosed content with a
-- distinctive background color.
--
-- ==== __Example__
--
-- __Input:__
--
-- >Html.p []
-- >    [ "Make sure to "
-- >    , Html.mark []
-- >        [ "complete" ]
-- >    , " the assignment by Friday."
-- >    ]
--
-- __Output:__
--
-- ><p>Make sure to <mark>complete</mark> the assignment by Friday.</p>
mark :: [Attribute] -> [Html lng] -> Html lng
mark = ParentNode "<mark" "</mark>"
{-# INLINE mark #-}


-- | Generates an HTML /\<menu\>/ element with the given attributes and contents.
--
-- The /\<menu\>/ element represents a list of commands or options, typically used for contextual menus or navigation menus in web
-- applications.
--
-- ==== __Example__
--
-- __Input:__
--
-- >Html.menu []
-- >    [ Html.li []
-- >        [ Html.a
-- >            [ Attr.href "profile.html" ]
-- >            [ "Profile" ]
-- >        ]
-- >    , Html.li []
-- >        [ Html.a
-- >            [ Attr.href "settings.html" ]
-- >            [ "Settings" ]
-- >        ]
-- >    , Html.li []
-- >        [ Html.a
-- >            [ Attr.href "logout.html" ]
-- >            [ "Logout" ]
-- >        ]
-- >    ]
--
-- __Output:__
--
-- ><menu>
-- >    <li><a href="profile.html">Profile</a></li>
-- >    <li><a href="settings.html">Settings</a></li>
-- >    <li><a href="logout.html">Logout</a></li>
-- ></menu>
menu :: [Attribute] -> [Html lng] -> Html lng
menu = ParentNode "<menu" "</menu>"
{-# INLINE menu #-}


-- | Generates an HTML /\<meta\>/ element with the given attributes.
--
-- The /\<meta\>/ element provides metadata about the document, such as character encoding, authorship, and viewport settings, which are
-- used by browsers and search engines but not typically displayed to users.
--
-- ==== __Example__
--
-- __Input:__
--
-- >Html.head []
-- >    [ Html.meta
-- >        [ Attr.charset "UTF-8" ]
-- >    , Html.meta
-- >        [ Attr.name "author"
-- >        , Attr.content "John Doe"
-- >        ]
-- >    , Html.meta
-- >        [ Attr.name "viewport"
-- >        , Attr.content "width=device-width, initial-scale=1.0"
-- >        ]
-- >    ]
--
-- __Output:__
--
-- ><head>
-- >    <meta charset="UTF-8">
-- >    <meta name="author" content="John Doe">
-- >    <meta name="viewport" content="width=device-width, initial-scale=1.0">
-- ></head>
meta :: [Attribute] -> Html lng
meta = LeafNode "<meta"
{-# INLINE meta #-}


-- | Generates an HTML /\<meter\>/ element with the given attributes and contents.
--
-- The /\<meter\>/ element represents a scalar measurement within a known range, often used to display gauges, progress bars, or other
-- visual representations of data.
--
-- ==== __Example__
--
-- __Input:__
--
-- >Html.p []
-- >    [ "Progress: "
-- >    , Html.meter
-- >        [ Attr.value "75"
-- >        , Attr.min "0"
-- >        , Attr.max "100"
-- >        ]
-- >        [ "75%" ]
-- >    ]
--
-- __Output:__
--
-- ><p>Progress: <meter value="75" min="0" max="100">75%</meter></p>
meter :: [Attribute] -> [Html lng] -> Html lng
meter = ParentNode "<meter" "</meter>"
{-# INLINE meter #-}


-- | Generates an HTML /\<nav\>/ element with the given attributes and contents.
--
-- The /\<nav\>/ element defines a section of navigation links or menus, typically containing links to other pages, sections of the current
-- page, or related content.
--
-- ==== __Example__
--
-- __Input:__
--
-- >Html.nav []
-- >    [ Html.ul []
-- >        [ Html.li []
-- >            [ Html.a
-- >                [ Attr.href "home.html" ]
-- >                [ "Home" ]
-- >            ]
-- >        , Html.li []
-- >            [ Html.a
-- >                [ Attr.href "about.html" ]
-- >                [ "About" ]
-- >            ]
-- >        , Html.li []
-- >            [ Html.a
-- >                [ Attr.href "contact.html" ]
-- >                [ "Contact" ]
-- >            ]
-- >        ]
-- >    ]
--
-- __Output:__
--
-- ><nav>
-- >    <ul>
-- >        <li><a href="home.html">Home</a></li>
-- >        <li><a href="about.html">About</a></li>
-- >        <li><a href="contact.html">Contact</a></li>
-- >    </ul>
-- ></nav>
nav :: [Attribute] -> [Html lng] -> Html lng
nav = ParentNode "<nav" "</nav>"
{-# INLINE nav #-}


-- | Generates an HTML /\<noscript\>/ element with the given attributes and contents.
--
-- The /\<noscript\>/ element is used to provide alternative content that should be displayed if a browser does not support scripting or if
-- scripting is disabled. It is often used to display a message or instructions for enabling JavaScript.
--
-- ==== __Example__
--
-- __Input:__
--
-- >Html.noscript []
-- >    [ Html.p []
-- >        [ "This page requires JavaScript to function properly." ]
-- >    ]
--
-- __Output:__
--
-- ><noscript>
-- >    <p>This page requires JavaScript to function properly.</p>
-- ></noscript>
noscript :: [Attribute] -> [Html lng] -> Html lng
noscript = ParentNode "<noscript" "</noscript>"
{-# INLINE noscript #-}


-- | Generates an HTML /\<object\>/ element with the given attributes and contents.
--
-- The /\<object\>/ element embeds external resources, such as multimedia or interactive content, into a web page. It is often used to embed
-- multimedia content like audio, video, or Flash animations.
--
-- ==== __Example__
--
-- __Input:__
--
-- >Html.object
-- >    [ Attr.data_ "video.mp4"
-- >    , Attr.type_ "video/mp4"
-- >    , Attr.width "300"
-- >    , Attr.height "200"
-- >    ]
-- >    []
--
-- __Output:__
--
-- ><object data="video.mp4" type="video/mp4" width="300" height="200"></object>
object :: [Attribute] -> [Html lng] -> Html lng
object = ParentNode "<object" "</object>"
{-# INLINE object #-}


-- | Generates an HTML /\<ol\>/ element with the given attributes and contents.
--
-- The /\<ol\>/ element is used to create an ordered list, where each list item is numbered sequentially. It is commonly used to represent
-- items with a specific order or sequence.
--
-- ==== __Example__
--
-- __Input:__
--
-- >Html.ol []
-- >    [ Html.li []
-- >        [ "Item 1" ]
-- >    , Html.li []
-- >        [ "Item 2" ]
-- >    ]
--
-- __Output:__
--
-- ><ol>
-- >    <li>Item 1</li>
-- >    <li>Item 2</li>
-- ></ol>
ol :: [Attribute] -> [Html lng] -> Html lng
ol = ParentNode "<ol" "</ol>"
{-# INLINE ol #-}


-- | Generates an HTML /\<optgroup\>/ element with the given attributes and contents.
--
-- The /\<optgroup\>/ element is used to group related options within a /\<select\>/ element, providing a way to create hierarchical or
-- categorized dropdown lists.
--
-- ==== __Example__
--
-- __Input:__
--
-- >Html.select []
-- >    [ Html.optgroup
-- >        [ Attr.label "Fruits" ]
-- >        [ Html.option []
-- >            [ "Apple" ]
-- >        , Html.option []
-- >            [ "Orange" ]
-- >        ]
-- >    , Html.optgroup
-- >        [ Attr.label "Vegetables" ]
-- >        [ Html.option []
-- >            [ "Carrot" ]
-- >        , Html.option []
-- >            [ "Spinach" ]
-- >        ]
-- >    ]
--
-- __Output:__
--
-- ><select>
-- >    <optgroup label="Fruits">
-- >        <option>Apple</option>
-- >        <option>Orange</option>
-- >    </optgroup>
-- >    <optgroup label="Vegetables">
-- >        <option>Carrot</option>
-- >        <option>Spinach</option>
-- >    </optgroup>
-- ></select>
optgroup :: [Attribute] -> [Html lng] -> Html lng
optgroup = ParentNode "<optgroup" "</optgroup>"
{-# INLINE optgroup #-}


-- | Generates an HTML /\<option\>/ element with the given attributes and contents.
--
-- The /\<option\>/ element is used within a /\<select\>/ or /\<datalist\>/ element to define individual options that users can select from
-- in a dropdown list or autocomplete input.
--
-- ==== __Example__
--
-- __Input:__
--
-- >Html.select []
-- >    [ Html.option
-- >        [ Attr.value "apple" ]
-- >        [ "Apple" ]
-- >    , Html.option
-- >        [ Attr.value "banana" ]
-- >        [ "Banana" ]
-- >    , Html.option
-- >        [ Attr.value "orange" ]
-- >        [ "Orange" ]
-- >    ]
--
-- __Output:__
--
-- ><select>
-- >    <option value="apple">Apple</option>
-- >    <option value="banana">Banana</option>
-- >    <option value="orange">Orange</option>
-- ></select>
option :: [Attribute] -> [Html lng] -> Html lng
option = ParentNode "<option" "</option>"
{-# INLINE option #-}


-- | Generates an HTML /\<output\>/ element with the given attributes and contents.
--
-- The /\<output\>/ element is employed to present the outcome of calculations, user interactions, or scripting actions, often utilized in
-- combination with form components and scripts.
--
-- ==== __Example__
--
-- __Input:__
--
-- >[ Html.p []
-- >    [ "Total: "
-- >    , Html.output
-- >        [ Attr.name "result" ]
-- >        [ "0" ]
-- >    ]
-- >, Html.input
-- >    [ Attr.type_ "range"
-- >    , Attr.min "0"
-- >    , Attr.max "100"
-- >    , Evnt.oninput "result.value = this.value"
-- >    ]
-- >]
--
-- __Output:__
--
-- ><p>Total: <output name="result">0</output></p>
-- ><input type="range" min="0" max="100" oninput="result.value = this.value">
output :: [Attribute] -> [Html lng] -> Html lng
output = ParentNode "<output" "</output>"
{-# INLINE output #-}


-- | Generates an HTML /\<p\>/ element with the given attributes and contents.
--
-- The /\<p\>/ element designates a paragraph of text, providing a structured way to separate and present blocks of content in a readable
-- format.
--
-- ==== __Example__
--
-- __Input:__
--
-- >Html.p []
-- >    [ "This is a paragraph of text." ]
--
-- __Output:__
--
-- ><p>This is a paragraph of text.</p>
p :: [Attribute] -> [Html lng] -> Html lng
p = ParentNode "<p" "</p>"
{-# INLINE p #-}


-- | Generates an HTML /\<picture\>/ element with the given attributes and contents.
--
-- The /\<picture\>/ element provides multiple sources for an image, allowing the browser to select the most appropriate version based on
-- factors such as device size or resolution.
--
-- ==== __Example__
--
-- __Input:__
--
-- >Html.picture []
-- >    [ Html.source
-- >        [ Attr.srcset "image-large.jpg"
-- >        , Attr.media "(min-width: 800px)"
-- >        ]
-- >    , Html.source
-- >        [ Attr.srcset "image-medium.jpg"
-- >        , Attr.media "(min-width: 500px)"
-- >        ]
-- >    , Html.img
-- >        [ Attr.src "image-small.jpg"
-- >        , Attr.alt "An image"
-- >        ]
-- >    ]
--
-- __Output:__
--
-- ><picture>
-- >    <source srcset="image-large.jpg" media="(min-width: 800px)">
-- >    <source srcset="image-medium.jpg" media="(min-width: 500px)">
-- >    <img src="image-small.jpg" alt="An image">
-- ></picture>
picture :: [Attribute] -> [Html lng] -> Html lng
picture = ParentNode "<picture" "</picture>"
{-# INLINE picture #-}


-- | Generates an HTML /\<pre\>/ element with the given attributes and contents.
--
-- The /\<pre\>/ element defines preformatted text, preserving both whitespace and line breaks, often used for displaying code or other
-- content with fixed formatting.
--
-- ==== __Example__
--
-- __Input:__
--
-- >Html.pre []
-- >    [ "console.log("Hello, world!");" ]
--
-- __Output:__
--
-- ><pre>
-- >    console.log("Hello, world!");
-- ></pre>
pre :: [Attribute] -> [Html lng] -> Html lng
pre = ParentNode "<pre" "</pre>"
{-# INLINE pre #-}


-- | Generates an HTML /\<progress\>/ element with the given attributes and contents.
--
-- The /\<progress\>/ element represents the completion progress of a task or process, typically displayed as a bar or other visual
-- indicator.
--
-- ==== __Example__
--
-- __Input:__
--
-- >Html.p []
-- >    [ "Uploading: "
-- >    , Html.progress
-- >        [ Attr.value "70"
-- >        , Attr.max "100"
-- >        ]
-- >        [ "70%" ]
-- >    ]
--
-- __Output:__
--
-- ><p>Uploading: <progress value="70" max="100">70%</progress></p>
progress :: [Attribute] -> [Html lng] -> Html lng
progress = ParentNode "<progress" "</progress>"
{-# INLINE progress #-}


-- | Generates an HTML /\<q\>/ element with the given attributes and contents.
--
-- The /\<q\>/ element represents a short inline quotation. It is used to indicate quoted content withing a paragraph or other block of
-- text.
--
-- ==== __Example__
--
-- __Input:__
--
-- >Html.p []
-- >    [ "He said, "
-- >    , Html.q []
-- >        [ "Life is either a daring adventure or nothing at all." ]
-- >    ]
--
-- __Output:__
--
-- ><p>He said, <q>Life is either a daring adventure or nothing at all.</q></p>
q :: [Attribute] -> [Html lng] -> Html lng
q = ParentNode "<q" "</q>"
{-# INLINE q #-}


-- | Generates an HTML /\<rp\>/ element with the given attributes and contents.
--
-- The /\<rp\>/ element is used to provide parentheses for browsers that do not support ruby annotations. It is used in conjunction with the
-- /\<ruby\>/ element to surround the text that provides the pronunciation or meaning of characters in East Asian typography.
--
-- ==== __Example__
--
-- __Input:__
--
-- >Html.ruby []
-- >    [ "漢"
-- >    , Html.rp []
-- >        [ "(" ]
-- >    , Html.rt []
-- >        [ "Kan" ]
-- >    , Html.rp []
-- >        [ ")" ]
-- >    ]
--
-- __Output:__
--
-- ><ruby>
-- >    漢<rp>(</rp><rt>Kan</rt><rp>)</rp>
-- ></ruby>
rp :: [Attribute] -> [Html lng] -> Html lng
rp = ParentNode "<rp" "</rp>"
{-# INLINE rp #-}


-- | Generates an HTML /\<rt\>/ element with the given attributes and contents.
--
-- The /\<rt\>/ element encloses the text that provides the pronunciation or meaning of characters in East Asian typography when using ruby
-- annotations.
--
-- ==== __Example__
--
-- __Input:__
--
-- >Html.ruby []
-- >    [ "漢"
-- >    , Html.rp []
-- >        [ "(" ]
-- >    , Html.rt []
-- >        [ "Kan" ]
-- >    , Html.rp []
-- >        [ ")" ]
-- >    ]
--
-- __Output:__
--
-- ><ruby>
-- >    漢<rp>(</rp><rt>Kan</rt><rp>)</rp>
-- ></ruby>
rt :: [Attribute] -> [Html lng] -> Html lng
rt = ParentNode "<rt" "</rt>"
{-# INLINE rt #-}


-- | Generates an HTML /\<ruby\>/ element with the given attributes and contents.
--
-- The /\<ruby\>/ element represents a ruby annotation, which consists of one or more text elements along with an optional pronunciation
-- guide. It is typically used in East Asian typography to indicate the pronunciation or meaning of characters.
--
-- ==== __Example__
--
-- __Input:__
--
-- >Html.ruby []
-- >    [ "漢"
-- >    , Html.rp []
-- >        [ "(" ]
-- >    , Html.rt []
-- >        [ "Kan" ]
-- >    , Html.rp []
-- >        [ ")" ]
-- >    ]
--
-- __Output:__
--
-- ><ruby>
-- >    漢<rp>(</rp><rt>Kan</rt><rp>)</rp>
-- ></ruby>
ruby :: [Attribute] -> [Html lng] -> Html lng
ruby = ParentNode "<ruby" "</ruby>"
{-# INLINE ruby #-}


-- | Generates an HTML /\<s\>/ element with the given attributes and contents.
--
-- The /\<s\>/ element is used to represent text that is no longer accurate or relevant. It renders the enclosed text with a strikethrough
-- style to indicate that it has been deleted or deprecated.
--
-- ==== __Example__
--
-- __Input:__
--
-- >Html.p []
-- >    [ "This feature is "
-- >    , Html.s []
-- >        [ "no longer supported" ]
-- >    , " deprecated."
-- >    ]
--
-- __Output:__
--
-- ><p>This feature is <s>no longer supported</s> deprecated.</p>
s :: [Attribute] -> [Html lng] -> Html lng
s = ParentNode "<s" "</s>"
{-# INLINE s #-}


-- | Generates an HTML /\<samp\>/ element with the given attributes and contents.
--
-- The /\<samp\>/ element encloses text that represents sample output from a computer program or script. It typically renders the text in a
-- monospace font to distinguish it from regular content.
--
-- ==== __Example__
--
-- __Input:__
--
-- >Html.p []
-- >    [ "The program output is: "
-- >    , Html.samp []
-- >        [ "Hello, world!" ]
-- >    ]
--
-- __Output:__
--
-- ><p>The program output is: <samp>Hello, world!</samp></p>
samp :: [Attribute] -> [Html lng] -> Html lng
samp = ParentNode "<samp" "</samp>"
{-# INLINE samp #-}


-- | Generates an HTML /\<script\>/ element with the given attributes and contents.
--
-- The /\<script\>/ element is used to embed or reference external JavaScript code within an HTML document. It can be placed in the
-- document\'s /\<head\>/ or /\<body\>/ section.
--
-- ==== __Example__
--
-- __Input:__
--
-- >Html.script []
-- >    [ "console.log("Hello, world!");" ]
--
-- __Output:__
--
-- ><script>
-- >    console.log("Hello, world!");
-- ></script>
script :: [Attribute] -> [Html lng] -> Html lng
script = ParentNode "<script" "</script>"
{-# INLINE script #-}


-- | Generates an HTML /\<section\>/ element with the given attributes and contents.
--
-- The /\<section\>/ element represents a thematic grouping of content within a document. It is used to structure the content into
-- semantically meaningful sections.
--
-- ==== __Example__
--
-- __Input:__
--
-- >Html.section []
-- >    [ Html.h2 []
-- >        [ "About Us" ]
-- >    , Html.p []
-- >        [ "We are a company dedicated to creating innovative solutions." ]
-- >    ]
--
-- __Output:__
--
-- ><section>
-- >    <h2>About Us</h2>
-- >    <p>We are a company dedicated to creating innovative solutions.</p>
-- ></section>
section :: [Attribute] -> [Html lng] -> Html lng
section = ParentNode "<section" "</section>"
{-# INLINE section #-}


-- | Generates an HTML /\<select\>/ element with the given attributes and contents.
--
-- The /\<select\>/ element creates a dropdown list from which users can select one or more options. It is used for creating form controls
-- to select choices from a predefined set.
--
-- ==== __Example__
--
-- __Input:__
--
-- >[ Html.label
-- >    [ Attr.for "country" ]
-- >    [ "Select your country:" ]
-- >, Html.select
-- >    [ Attr.id "country"
-- >    , Attr.name "country"
-- >    ]
-- >    [ Html.option
-- >        [ Attr.value "usa" ]
-- >        [ "United States" ]
-- >    , Html.option
-- >        [ Attr.value "canada" ]
-- >        [ "Canada" ]
-- >    , Html.option
-- >        [ Attr.value "uk" ]
-- >        [ "United Kingdom" ]
-- >    ]
-- >]
--
-- __Output:__
--
-- ><label for="country">Select your country:</label>
-- ><select id="country" name="country">
-- >    <option value="usa">United States</option>
-- >    <option value="canada">Canada</option>
-- >    <option value="uk">United Kingdom</option>
-- ></select>
select :: [Attribute] -> [Html lng] -> Html lng
select = ParentNode "<select" "</select>"
{-# INLINE select #-}


-- | Generates an HTML /\<slot\>/ element with the given attributes and contents.
--
-- The /\<slot\>/ element is used within a shadow DOM to define placeholders for where specific content should be inserted from the light
-- DOM. It enables custom elements to distribute content.
--
-- ==== __Example__
--
-- __Input:__
--
-- >[ Html.template
-- >    [ Attr.id "my-template" ]
-- >    [ Html.p []
-- >        [ Html.slot
-- >            [ Attr.name "content" ]
-- >            []
-- >        ]
-- >    ]
-- >, ParentNode "<my-element" "</my-element>" []
-- >    [ Html.p
-- >        [ Attr.slot "content" ]
-- >        [ "This content goes inside the slot." ]
-- >    ]
-- >]
--
-- __Output:__
--
-- ><template id="my-template">
-- >    <p><slot name="content"></slot></p>
-- ></template>
-- ><my-element>
-- >    <p slot="content">This content goes inside the slot.</p>
-- ></my-element>
slot :: [Attribute] -> [Html lng] -> Html lng
slot = ParentNode "<slot" "</slot>"
{-# INLINE slot #-}


-- | Generates an HTML /\<small\>/ element with the given attributes and contents.
--
-- The /\<small\>/ element is used to indicate that the enclosed text is smaller and has reduced importance, typically used for legal
-- disclaimers, copyright notices, or fine print.
--
-- ==== __Example__
--
-- __Input:__
--
-- >Html.p []
-- >    [ Html.small []
-- >        [ "This information is subject to change without notice." ]
-- >    ]
--
-- __Output:__
--
-- ><p><small>This information is subject to change without notice.</small></p>
small :: [Attribute] -> [Html lng] -> Html lng
small = ParentNode "<small" "</small>"
{-# INLINE small #-}


-- | Generates an HTML /\<source\>/ element with the given attributes.
--
-- The /\<source\>/ element is used within the /\<video\>/ and /\<audio\>/ elements to specify multiple media sources for the same media,
-- allowing the browser to choose the most suitable source based on its capabilities.
--
-- ==== __Example__
--
-- __Input:__
--
-- >Html.video
-- >    [ Attr.controls True ]
-- >    [ Html.source
-- >        [ Attr.src "video.mp4"
-- >        , Attr.type_ "video/mp4"
-- >        ]
-- >    , Html.source
-- >        [ Attr.src "video.webm"
-- >        , Attr.type_ "video/webm"
-- >        ]
-- >    , "Your browser does not support the video tag."
-- >    ]
--
-- __Output:__
--
-- ><video controls>
-- >    <source src="video.mp4" type="video/mp4">
-- >    <source src="video.webm" type="video/webm">
-- >    Your browser does not support the video tag.
-- ></video>
source :: [Attribute] -> Html lng
source = LeafNode "<source"
{-# INLINE source #-}


-- | Generates an HTML /\<span\>/ element with the given attributes and contents.
--
-- The /\<span\>/ element is an inline container used to apply styles or scripting to a specific portion of text within a larger element,
-- without affecting the overall layout.
--
-- ==== __Example__
--
-- __Input:__
--
-- >Html.p []
-- >    [ "Click "
-- >    , Html.span
-- >        [ Attr.class_ "highlight" ]
-- >        [ "here" ]
-- >    , " to learn more."
-- >    ]
--
-- __Output:__
--
-- ><p>Click <span class="highlight">here</span> to learn more.</p>
span :: [Attribute] -> [Html lng] -> Html lng
span = ParentNode "<span" "</span>"
{-# INLINE span #-}


-- | Generates an HTML /\<strong\>/ element with the given attributes and contents.
--
-- The /\<strong\>/ element is used to indicate that the enclosed text has strong importance or emphasis. It is typically rendered as bold
-- text.
--
-- ==== __Example__
--
-- __Input:__
--
-- >Html.p []
-- >    [ Html.strong []
-- >        [ "Warning:" ]
-- >    , " Do not proceed without proper safety gear."
-- >    ]
--
-- __Output:__
--
-- ><p><strong>Warning:</strong> Do not proceed without proper safety gear.</p>
strong :: [Attribute] -> [Html lng] -> Html lng
strong = ParentNode "<strong" "</strong>"
{-# INLINE strong #-}


-- | Generates an HTML /\<style\>/ element with the given attributes and contents.
--
-- The /\<style\>/ element defines inline CSS within an HTML document. It allows you to apply specific styles to the content within the same
-- document.
--
-- ==== __Example__
--
-- __Input:__
--
-- >[ Html.style []
-- >    [ "p { color: blue; font-size: 16px; }" ]
-- >, Html.p []
-- >    [ "This is a styled paragraph." ]
-- >]
--
-- __Output:__
--
-- ><style>
-- >    p {
-- >        color: blue;
-- >        font-size: 16px;
-- >    }
-- ></style>
-- ><p>This is a styled paragraph.</p>
style :: [Attribute] -> [Html lng] -> Html lng
style = ParentNode "<style" "</style>"
{-# INLINE style #-}


-- | Generates an HTML /\<sub\>/ element with the given attributes and contents.
--
-- The /\<sub\>/ element renders text as subscript, which is often used for chemical formulas, mathematical subscripts, or annotations.
--
-- ==== __Example__
--
-- __Input:__
--
-- >Html.p []
-- >    [ "H"
-- >    , Html.sub []
-- >        [ "2" ]
-- >    , "O is the chemical formula for water."
-- >    ]
--
-- __Output:__
--
-- ><p>H<sub>2</sub>O is the chemical formula for water.</p>
sub :: [Attribute] -> [Html lng] -> Html lng
sub = ParentNode "<sub" "</sub>"
{-# INLINE sub #-}


-- | Generates an HTML /\<summary\>/ element with the given attributes and contents.
--
-- The /\<summary\>/ element is used as a clickable summary for the content of a /\<details\>/ element, which provides a way to create
-- collapsible sections of content with a visible summary.
--
-- ==== __Example__
--
-- __Input:__
--
-- >Html.details []
-- >    [ Html.summary []
-- >        [ "Click to reveal more information" ]
-- >    , Html.p []
-- >        [ "Here is the additional content that was hidden." ]
-- >    ]
--
-- __Output:__
--
-- ><details>
-- >    <summary>Click to reveal more information</summary>
-- >    <p>Here is the additional content that was hidden.</p>
-- ></details>
summary :: [Attribute] -> [Html lng] -> Html lng
summary = ParentNode "<summary" "</summary>"
{-# INLINE summary #-}


-- | Generates an HTML /\<sup\>/ element with the given attributes and contents.
--
-- The /\<sup\>/ element is used to render text as superscript, which is typically used for mathematical exponents or footnotes.
--
-- ==== __Example__
--
-- __Input:__
--
-- >Html.p []
-- >    [ "Euler's identity: e"
-- >    , Html.sup []
-- >        [ "iπ" ]
-- >    , " + 1 = 0"
-- >    ]
--
-- __Output:__
--
-- ><p>Euler's identity: e<sup>iπ</sup> + 1 = 0</p>
sup :: [Attribute] -> [Html lng] -> Html lng
sup = ParentNode "<sup" "</sup>"
{-# INLINE sup #-}


-- | Generates an HTML /\<table\>/ element with the given attributes and contents.
--
-- The /\<table\>/ element is used to create a table that organizes data into rows and columns. It contains various table-related elements
-- like /\<thead\>/, /\<tbody\>/, and /\<tfoot\>/ for structuring the table.
--
-- ==== __Example__
--
-- __Input:__
--
-- >Html.table []
-- >    [ Html.thead []
-- >        [ Html.tr []
-- >            [ Html.th []
-- >                [ "Name" ]
-- >            , Html.th []
-- >                [ "Age" ]
-- >            ]
-- >        ]
-- >    , Html.tbody []
-- >        [ Html.tr []
-- >            [ Html.td []
-- >                [ "John" ]
-- >            , Html.td []
-- >                [ "30" ]
-- >            ]
-- >        , Html.tr []
-- >            [ Html.td []
-- >                [ "Alice" ]
-- >            , Html.td []
-- >                [ "25" ]
-- >            ]
-- >        ]
-- >    ]
--
-- __Output:__
--
-- ><table>
-- >    <thead>
-- >        <tr>
-- >            <th>Name</th>
-- >            <th>Age</th>
-- >        </tr>
-- >    </thead>
-- >    <tbody>
-- >        <tr>
-- >            <td>John</td>
-- >            <td>30</td>
-- >        </tr>
-- >        <tr>
-- >            <td>Alice</td>
-- >            <td>25</td>
-- >        </tr>
-- >    </tbody>
-- ></table>
table :: [Attribute] -> [Html lng] -> Html lng
table = ParentNode "<table" "</table>"
{-# INLINE table #-}


-- | Generates an HTML /\<tbody\>/ element with the given attributes and contents.
--
-- The /\<tbody\>/ element is used to group the body content of a table. It is used in conjunction with the /\<table\>/ element to contain
-- rows of data within the table.
--
-- ==== __Example__
--
-- __Input:__
--
-- >Html.table []
-- >    [ Html.thead []
-- >        [ Html.tr []
-- >            [ Html.th []
-- >                [ "Name" ]
-- >            , Html.th []
-- >                [ "Age" ]
-- >            ]
-- >        ]
-- >    , Html.tbody []
-- >        [ Html.tr []
-- >            [ Html.td []
-- >                [ "John" ]
-- >            , Html.td []
-- >                [ "30" ]
-- >            ]
-- >        , Html.tr []
-- >            [ Html.td []
-- >                [ "Alice" ]
-- >            , Html.td []
-- >                [ "25" ]
-- >            ]
-- >        ]
-- >    ]
--
-- __Output:__
--
-- ><table>
-- >    <thead>
-- >        <tr>
-- >            <th>Name</th>
-- >            <th>Age</th>
-- >        </tr>
-- >    </thead>
-- >    <tbody>
-- >        <tr>
-- >            <td>John</td>
-- >            <td>30</td>
-- >        </tr>
-- >        <tr>
-- >            <td>Alice</td>
-- >            <td>25</td>
-- >        </tr>
-- >    </tbody>
-- ></table>
tbody :: [Attribute] -> [Html lng] -> Html lng
tbody = ParentNode "<tbody" "</tbody>"
{-# INLINE tbody #-}


-- | Generates an HTML /\<td\>/ element with the given attributes and contents.
--
-- The /\<td\>/ element represents a single data cell within a table row (/\<tr\>/). It is used to display data within the columns of a
-- table.
--
-- ==== __Example__
--
-- __Input:__
--
-- >Html.table []
-- >    [ Html.tr []
-- >        [ Html.td []
-- >            [ "Apple" ]
-- >        , Html.td []
-- >            [ "Red" ]
-- >        , Html.td []
-- >            [ "Fruit" ]
-- >        ]
-- >    ]
--
-- __Output:__
--
-- ><table>
-- >    <tr>
-- >        <td>Apple</td>
-- >        <td>Red</td>
-- >        <td>Fruit</td>
-- >    </tr>
-- ></table>
td :: [Attribute] -> [Html lng] -> Html lng
td = ParentNode "<td" "</td>"
{-# INLINE td #-}


-- | Generates an HTML /\<template\>/ element with the given attributes and contents.
--
-- The /\<template\>/ element is used to declare content that is not rendered when the page loads but can be cloned and used later via
-- JavaScript. It is useful for creating reusable content that can be instantiated programmatically.
--
-- ==== __Example__
--
-- __Input:__
--
-- >Html.template
-- >    [ Attr.id "my-template" ]
-- >    [ Html.p []
-- >        [ "This is a template." ]
-- >    ]
--
-- __Output:__
--
-- ><template id="my-template">
-- >    <p>This is a template content.</p>
-- ></template>
template :: [Attribute] -> [Html lng] -> Html lng
template = ParentNode "<template" "</template>"
{-# INLINE template #-}


-- | Generates an HTML /\<textarea\>/ element with the given attributes and contents.
--
-- The /\<textarea\>/ element creates a multiline text input field where users can enter longer pieces of text, such as comments or
-- messages.
--
-- ==== __Example__
--
-- __Input:__
--
-- >[ Html.label
-- >    [ Attr.for "comment" ]
-- >    [ "Enter your comment:" ]
-- >, Html.textarea
-- >    [ Attr.id "comment"
-- >    , Attr.name "comment"
-- >    , Attr.rows "4"
-- >    , Attr.cols "50"
-- >    ]
-- >    []
-- >]
--
-- __Output:__
--
-- ><label for="comment">Enter your comment:</label>
-- ><textarea id="comment" name="comment" rows="4" cols="50"></textarea>
textarea :: [Attribute] -> [Html lng] -> Html lng
textarea = ParentNode "<textarea" "</textarea>"
{-# INLINE textarea #-}


-- | Generates an HTML /\<tfoot\>/ element with the given attributes and contents.
--
-- The /\<tfoot\>/ element is used to group the footer content of a table. It is used in conjunction with the /\<table\>/ element to contain
-- footer rows that provide additional information about the table data.
--
-- ==== __Example__
--
-- __Input:__
--
-- >Html.table []
-- >    [ Html.thead []
-- >        [ Html.tr []
-- >            [ Html.th []
-- >                [ "Name" ]
-- >            , Html.th []
-- >                [ "Age" ]
-- >            ]
-- >        ]
-- >    , Html.tbody []
-- >        [ Html.tr []
-- >            [ Html.td []
-- >                [ "John" ]
-- >            , Html.td []
-- >                [ "30" ]
-- >            ]
-- >        , Html.tr []
-- >            [ Html.td []
-- >                [ "Alice" ]
-- >            , Html.td []
-- >                [ "25" ]
-- >            ]
-- >        ]
-- >    , Html.tfoot []
-- >        [ Html.tr []
-- >            [ Html.td
-- >                [ Attr.colspan "2" ]
-- >                [ "Total: 2 persons" ]
-- >            ]
-- >        ]
-- >    ]
--
-- __Output:__
--
-- ><table>
-- >    <thead>
-- >        <tr>
-- >            <th>Name</th>
-- >            <th>Age</th>
-- >        </tr>
-- >    </thead>
-- >    <tbody>
-- >        <tr>
-- >            <td>John</td>
-- >            <td>30</td>
-- >        </tr>
-- >        <tr>
-- >            <td>Alice</td>
-- >            <td>25</td>
-- >        </tr>
-- >    </tbody>
-- >    <tfoot>
-- >        <tr>
-- >            <td colspan="2">Total: 2 persons</td>
-- >        </tr>
-- >    </tfoot>
-- ></table>
tfoot :: [Attribute] -> [Html lng] -> Html lng
tfoot = ParentNode "<tfoot" "</tfoot>"
{-# INLINE tfoot #-}


-- | Generates an HTML /\<th\>/ element with the given attributes and contents.
--
-- The /\<th\>/ element represents a header cell within a table row (/\<tr\>/). It is used to define header information for a column or row
-- in a table. The content within a /\<th\>/ element is typically bold and centered by default.
--
-- ==== __Example__
--
-- __Input:__
--
-- >Html.table []
-- >    [ Html.thead []
-- >        [ Html.tr []
-- >            [ Html.th []
-- >                [ "Name" ]
-- >            , Html.th []
-- >                [ "Age" ]
-- >            ]
-- >        ]
-- >    , Html.tbody []
-- >        [ Html.tr []
-- >            [ Html.td []
-- >                [ "John" ]
-- >            , Html.td []
-- >                [ "30" ]
-- >            ]
-- >        , Html.tr []
-- >            [ Html.td []
-- >                [ "Alice" ]
-- >            , Html.td []
-- >                [ "25" ]
-- >            ]
-- >        ]
-- >    ]
--
-- __Output:__
--
-- ><table>
-- >    <thead>
-- >        <tr>
-- >            <th>Name</th>
-- >            <th>Age</th>
-- >        </tr>
-- >    </thead>
-- >    <tbody>
-- >        <tr>
-- >            <td>John</td>
-- >            <td>30</td>
-- >        </tr>
-- >        <tr>
-- >            <td>Alice</td>
-- >            <td>25</td>
-- >        </tr>
-- >    </tbody>
-- ></table>
th :: [Attribute] -> [Html lng] -> Html lng
th = ParentNode "<th" "</th>"
{-# INLINE th #-}


-- | Generates an HTML /\<thead\>/ element with the given attributes and contents.
--
-- The /\<thead\>/ element is used to group the header content of a table. It is used in conjunction with the /\<table\>/ element to contain
-- header rows that label the columns or rows in the table.
--
-- ==== __Example__
--
-- __Input:__
--
-- >Html.table []
-- >    [ Html.thead []
-- >        [ Html.tr []
-- >            [ Html.th []
-- >                [ "Name" ]
-- >            , Html.th []
-- >                [ "Age" ]
-- >            ]
-- >        ]
-- >    , Html.tbody []
-- >        [ Html.tr []
-- >            [ Html.td []
-- >                [ "John" ]
-- >            , Html.td []
-- >                [ "30" ]
-- >            ]
-- >        , Html.tr []
-- >            [ Html.td []
-- >                [ "Alice" ]
-- >            , Html.td []
-- >                [ "25" ]
-- >            ]
-- >        ]
-- >    ]
--
-- __Output:__
--
-- ><table>
-- >    <thead>
-- >        <tr>
-- >            <th>Name</th>
-- >            <th>Age</th>
-- >        </tr>
-- >    </thead>
-- >    <tbody>
-- >        <tr>
-- >            <td>John</td>
-- >            <td>30</td>
-- >        </tr>
-- >        <tr>
-- >            <td>Alice</td>
-- >            <td>25</td>
-- >        </tr>
-- >    </tbody>
-- ></table>
thead :: [Attribute] -> [Html lng] -> Html lng
thead = ParentNode "<thead" "</thead>"
{-# INLINE thead #-}


-- | Generates an HTML /\<time\>/ element with the given attributes and contents.
--
-- The /\<time\>/ element represents a specific time or date, either as machine-readable text or for user-friendly display. It can include a
-- /datetime/ attribute for semantic and accessible purposes.
--
-- ==== __Example__
--
-- __Input:__
--
-- >Html.p []
-- >    [ "Meeting time: "
-- >    , Html.time
-- >        [ Attr.datetime "2023-09-15T10:00" ]
-- >        [ "September 15, 10:00 AM" ]
-- >    ]
--
-- __Output:__
--
-- ><p>Meeting time: <time datetime="2023-09-15T10:00">September 15, 10:00 AM</time></p>
time :: [Attribute] -> [Html lng] -> Html lng
time = ParentNode "<time" "</time>"
{-# INLINE time #-}


-- | Generates an HTML /\<title\>/ element with the given attributes and contents.
--
-- The /\<title\>/ element defines the title of the document, which is displayed in the browser\'s title bar or tab. It is also used by
-- search engines and social media when sharing links.
--
-- ==== __Example__
--
-- __Input:__
--
-- >Html.head []
-- >    [ Html.title []
-- >        [ "My Website" ]
-- >    ]
--
-- __Output:__
--
-- ><head>
-- >    <title>My Website</title>
-- ></head>
title :: [Attribute] -> [Html lng] -> Html lng
title = ParentNode "<title" "</title>"
{-# INLINE title #-}


-- | Generates an HTML /\<tr\>/ element with the given attributes and contents.
--
-- The /\<tr\>/ element defines a row within a table. It contains a collection of table cells (/\<td\>/ or /\<th\>/) that represent data for
-- that particular row.
--
-- ==== __Example__
--
-- __Input:__
--
-- >Html.table []
-- >    [ Html.thead []
-- >        [ Html.tr []
-- >            [ Html.th []
-- >                [ "Name" ]
-- >            , Html.th []
-- >                [ "Age" ]
-- >            ]
-- >        ]
-- >    , Html.tbody []
-- >        [ Html.tr []
-- >            [ Html.td []
-- >                [ "John" ]
-- >            , Html.td []
-- >                [ "30" ]
-- >            ]
-- >        , Html.tr []
-- >            [ Html.td []
-- >                [ "Alice" ]
-- >            , Html.td []
-- >                [ "25" ]
-- >            ]
-- >        ]
-- >    ]
--
-- __Output:__
--
-- ><table>
-- >    <thead>
-- >        <tr>
-- >            <th>Name</th>
-- >            <th>Age</th>
-- >        </tr>
-- >    </thead>
-- >    <tbody>
-- >        <tr>
-- >            <td>John</td>
-- >            <td>30</td>
-- >        </tr>
-- >        <tr>
-- >            <td>Alice</td>
-- >            <td>25</td>
-- >        </tr>
-- >    </tbody>
-- ></table>
tr :: [Attribute] -> [Html lng] -> Html lng
tr = ParentNode "<tr" "</tr>"
{-# INLINE tr #-}


-- | Generates an HTML /\<track\>/ element with the given attributes.
--
-- The /\<track\>/ element specifies text tracks for media elements like /\<video\>/ and /\<audio\>/, such as subtitles, captions, or
-- descriptions, to provide additional content for users.
--
-- ==== __Example__
--
-- __Input:__
--
-- >Html.video
-- >    [ Attr.controls True ]
-- >    [ Html.source
-- >        [ Attr.src "video.mp4"
-- >        , Attr.type_ "video/mp4"
-- >        ]
-- >    , Html.track
-- >        [ Attr.src "subtitles.vtt"
-- >        , Attr.kind "subtitles"
-- >        , Attr.label "English"
-- >        , Attr.srclang "en"
-- >        ]
-- >    , "Your browser does not support the video tag."
-- >    ]
--
-- __Output:__
--
-- ><video controls>
-- >    <source src="video.mp4" type="video/mp4">
-- >    <track src="subtitles.vtt" kind="subtitles" label="English" srclang="en">
-- >    Your browser does not support the video tag.
-- ></video>
track :: [Attribute] -> Html lng
track = LeafNode "<track"
{-# INLINE track #-}


-- | Generates an HTML /\<u\>/ element with the given attributes and contents.
--
-- The /\<u\>/ element renders enclosed text with an underline, traditionally indicating that the text is meant to be a hyperlink. However,
-- it is generally recommended to use CSS for styling, and /\<u\>/ is mostly avoided for indicating hyperlinks.
--
-- ==== __Example__
--
-- __Input:__
--
-- >Html.p []
-- >    [ Html.u []
-- >        [ "This text is underlined." ]
-- >    ]
--
-- __Output:__
--
-- ><p><u>This text is underlined.</u></p>
u :: [Attribute] -> [Html lng] -> Html lng
u = ParentNode "<u" "</u>"
{-# INLINE u #-}


-- | Generates an HTML /\<ul\>/ element with the given attributes and contents.
--
-- The /\<ul\>/ element is used to create an unordered (bulleted) list. It contains one or more /\<li\>/ elements that represent individual
-- list items.
--
-- ==== __Example__
--
-- __Input:__
--
-- >Html.ul []
-- >    [ Html.li []
-- >        [ "Item 1" ]
-- >    , Html.li []
-- >        [ "Item 2" ]
-- >    , Html.li []
-- >        [ "Item 3" ]
-- >    ]
--
-- __Output:__
--
-- ><ul>
-- >    <li>Item 1</li>
-- >    <li>Item 2</li>
-- >    <li>Item 3</li>
-- ></ul>
ul :: [Attribute] -> [Html lng] -> Html lng
ul = ParentNode "<ul" "</ul>"
{-# INLINE ul #-}


-- | Generates an HTML /\<var\>/ element with the given attributes and contents.
--
-- The /\<var\>/ element indicates that the enclosed text represents a variable or placeholder within a mathematical expression or
-- programming context.
--
-- ==== __Example__
--
-- __Input:__
--
-- >Html.p []
-- >    [ "The equation is "
-- >    , Html.var []
-- >        [ "x = y + z" ]
-- >    , "."
-- >    ]
--
-- __Output:__
--
-- ><p>The equation is <var>x = y + z</var>.</p>
var :: [Attribute] -> [Html lng] -> Html lng
var = ParentNode "<var" "</var>"
{-# INLINE var #-}


-- | Generates an HTML /\<video\>/ element with the given attributes and contents.
--
-- The /\<video\>/ element is used to embed video content in a web page. It provides a container for playing videos and allows for controls,
-- source specifications, and various settings.
--
-- ==== __Example__
--
-- __Input:__
--
-- >Html.video
-- >    [ Attr.controls True ]
-- >    [ Html.source
-- >        [ Attr.src "video.mp4"
-- >        , Attr.type_ "video/mp4"
-- >        ]
-- >    , "Your browser does not support the video tag."
-- >    ]
--
-- __Output:__
--
-- ><video controls>
-- >    <source src="video.mp4" type="video/mp4">
-- >    Your browser does not support the video tag.
-- ></video>
video :: [Attribute] -> [Html lng] -> Html lng
video = ParentNode "<video" "</video>"
{-# INLINE video #-}


-- | Generates an HTML /\<wbr\>/ element with the given attributes.
--
-- The /\<wbr\>/ element suggests a point in text where a line break may occur when necessary, such as in long URLs or strings without
-- spaces, helping to improve text layout and readability.
--
-- ==== __Example__
--
-- __Input:__
--
-- >Html.p []
-- >    [ "Thisisaverylongwordthatmightneed"
-- >    , Html.wbr []
-- >    , "to"
-- >    , Html.br []
-- >    , "break"
-- >    ]
--
-- __Output:__
--
-- ><p>Thisisaverylongwordthatmightneed<wbr>to<br>break</p>
wbr :: [Attribute] -> Html lng
wbr = LeafNode "<wbr"
{-# INLINE wbr #-}
