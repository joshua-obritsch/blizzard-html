{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The "Html" module provides a set of types, classes and functions for generating HTML elements.
--
-- These elements along with their attributes in the "Html.Attributes" module can be used to dynamically compose HTML documents natively in
-- Haskell, without relying on templating engines or other techniques that can be error-prone and difficult to maintain.
--
-- Additionally, the functions provided in the "Html.Intl" module can be used to facilitate internationalization.
--
-- === Example
--
-- __Input:__
--
-- @
-- Html.doctype []
--     [ Html.html []
--         [ Html.head []
--             [ Html.title []
--                 [ Html.text \"The Elder Scrolls: Skyrim Guide\" ]
--             ]
--         , Html.body []
--             [ Html.h1 []
--                 [ Html.text \"Mastering Skyrim: A Guide to Becoming the Dragonborn\" ]
--             , Html.article []
--                 [ Html.h2 []
--                     [ Html.text \"Alchemy for Beginners\" ]
--                 , Html.p []
--                     [ Html.text \"Learn the art of potion-making and uncover rare ingredients to enhance your adventures.\" ]
--                 ]
--             , Html.article []
--                 [ Html.h2 []
--                     [ Html.text \"Battling Dragons\" ]
--                 , Html.p []
--                     [ Html.text \"Equip yourself with strategies and weapons to conquer fearsome dragons.\" ]
--                 ]
--             , Html.article []
--                 [ Html.h2 []
--                     [ Html.text \"Thieves Guild Secrets\" ]
--                 , Html.p []
--                     [ Html.text \"Join the Thieves Guild and delve into the hidden world of stealth and thievery.\" ]
--                 ]
--             ]
--         ]
--     ]
-- @
--
-- __Output:__
--
-- @
-- \<!DOCTYPE html\>
-- \<html\>
--     \<head\>
--         \<title\>The Elder Scrolls: Skyrim Guide\<\/title\>
--     \<\/head\>
--     \<body\>
--         \<h1\>Mastering Skyrim: A Guide to Becoming the Dragonborn\<\/h1\>
--         \<article\>
--             \<h2\>Alchemy for Beginners\<\/h2\>
--             \<p\>Learn the art of potion-making and uncover rare ingredients to enhance your adventures.\<\/p\>
--         \<\/article\>
--         \<article\>
--             \<h2\>Battling Dragons\<\/h2\>
--             \<p\>Equip yourself with strategies and weapons to conquer fearsome dragons.\<\/p\>
--         \<\/article\>
--         \<article\>
--             \<h2\>Thieves Guild Secrets\<\/h2\>
--             \<p\>Join the Thieves Guild and delve into the hidden world of stealth and thievery.\<\/p\>
--         \<\/article\>
--     \<\/body\>
-- \<\/html\>
-- @
--
-- /Note: All examples in this module assume the following imports:/
--
-- @
-- import qualified Html
-- import qualified Html.Attributes as Attr
-- @
--
-- /Note: All example results in this module are formatted neatly for readability but are condensed in practice./
module Html
    ( -- * Types
      Html(..)
    , Attribute(..)

      -- * Classes
    , Buildable(..)
    , Translatable(..)

      -- * Declarations
    , doctype

      -- * Elements
    , a
    , abbr
    , address
    , area
    , article
    , aside
    , audio
    , b
    , base
    , bdi
    , bdo
    , blockquote
    , body
    , br
    , button
    , canvas
    , caption
    , cite
    , code
    , col
    , colgroup
    , data_
    , datalist
    , dd
    , del
    , details
    , dfn
    , dialog
    , div
    , dl
    , dt
    , em
    , embed
    , fieldset
    , figcaption
    , figure
    , footer
    , form
    , h1
    , h2
    , h3
    , h4
    , h5
    , h6
    , head
    , header
    , hgroup
    , hr
    , html
    , i
    , iframe
    , img
    , input
    , ins
    , kbd
    , label
    , legend
    , li
    , link
    , main
    , map
    , mark
    , menu
    , meta
    , meter
    , nav
    , noscript
    , object
    , ol
    , optgroup
    , option
    , output
    , p
    , picture
    , pre
    , progress
    , q
    , rp
    , rt
    , ruby
    , s
    , samp
    , script
    , section
    , select
    , slot
    , small
    , source
    , span
    , strong
    , style
    , sub
    , summary
    , sup
    , table
    , tbody
    , td
    , template
    , textarea
    , tfoot
    , th
    , thead
    , time
    , title
    , tr
    , track
    , u
    , ul
    , var
    , video
    , wbr

      -- * Text
    , text
    , empty
    ) where


import Data.Bool (Bool(..))
import Data.Foldable (foldr)
import Data.Function (($), (.))
import Data.Monoid ((<>), mempty)
import Data.Text.Lazy (unpack)
import Data.Text.Lazy.Builder (Builder, singleton, toLazyText)
import Text.Show (Show(..), showString)


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


instance Show (Html lng) where
    show = unpack . toLazyText . build
    {-# INLINE show #-}


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
    {-# INLINE show #-}


instance Buildable [Html lng] where
    build = foldr ((<>) . build) mempty
    {-# INLINE build #-}


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
    {-# INLINE show #-}


instance Buildable Attribute where
    build attribute = case attribute of
        BoolAttribute _   False -> mempty
        BoolAttribute key True  -> key
        TextAttribute _   ""    -> mempty
        TextAttribute key value -> key <> value <> singleton '"'


instance {-# OVERLAPPING #-} Show [Attribute] where
    show = unpack . toLazyText . build
    {-# INLINE show #-}


instance Buildable [Attribute] where
    build = foldr ((<>) . build) mempty
    {-# INLINE build #-}


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
-- @
-- Html.doctype
--     [ Html.html []
--         [ Html.head []
--             [ Html.title []
--                 [ Html.text \"Gigatron\" ]
--             ]
--         , Html.body []
--             [ Html.h1 []
--                 [ Html.text \"About\" ]
--             , Html.p []
--                 [ Html.text \"Gigatron is an 8-bit microcomputer built from TTL chips.\" ]
--             ]
--         ]
--     ]
-- @
--
-- __Output:__
--
-- @
-- \<!DOCTYPE html\>
-- \<html\>
--     \<head\>
--         \<title\>Gigatron\<\/title\>
--     \<\/head\>
--     \<body\>
--         \<h1\>About\<\/h1\>
--         \<p\>Gigatron is an 8-bit microcomputer built from TTL chips.\<\/p\>
--     \<\/body\>
-- \<\/html\>
-- @
doctype :: [Html lng] -> Html lng
doctype = RootNode "<!DOCTYPE html>\n"
{-# INLINE doctype #-}


-- ELEMENTS


-- | Generates an HTML /\<a\>/ element with the given attributes and contents.
--
-- The /\<a\>/ element, or anchor element, is used to create hyperlinks that link to other web pages or resources. It defines the clickable
-- content that, when clicked, navigates to the specified URL.
--
-- ==== __Example__
--
-- __Input:__
--
-- @
-- Html.nav []
--     [ Html.ul []
--         [ Html.li []
--             [ Html.a
--                 [ Attr.href \"\#heroes\" ]
--                 [ Html.text \"Valiant Adventurers\" ]
--             , Html.a
--                 [ Attr.href \"\#monsters\" ]
--                 [ Html.text \"Unspeakable Horrors\" ]
--             , Html.a
--                 [ Attr.href \"\#trinkets\" ]
--                 [ Html.text \"Cursed Relics\" ]
--             , Html.a
--                 [ Attr.href \"\#strategies\" ]
--                 [ Html.text \"Descent Tactics\" ]
--             ]
--         ]
--     ]
-- @
--
-- __Output:__
--
-- @
-- \<nav\>
--     \<ul\>
--         \<li\>\<a href=\"\#heroes\"\>Valiant Adventurers\<\/a\>\<\/li\>
--         \<li\>\<a href=\"\#monsters\"\>Unspeakable Horrors\<\/a\>\<\/li\>
--         \<li\>\<a href=\"\#trinkets\"\>Cursed Relics\<\/a\>\<\/li\>
--         \<li\>\<a href=\"\#strategies\"\>Descent Tactics\<\/a\>\<\/li\>
--     \<\/ul\>
-- \<\/nav\>
-- @
a :: [Attribute] -> [Html lng] -> Html lng
a = ParentNode "<a" "</a>"
{-# INLINE a #-}


-- | Generates an HTML /\<abbr\>/ element with the given attributes and contents.
--
-- The /\<abbr\>/ element is used to mark up an abbreviation or acronym in the text. It can include a title attribute to provide the full or
-- expanded form of the abbreviation when hovered over.
--
-- ==== __Example__
--
-- __Input:__
--
-- @
-- Html.p []
--     [ Html.text \"The \"
--     , Html.abbr
--         [ Attr.title \"Hypertext Markup Language\" ]
--         [ Html.text \"HTML\" ]
--     , Html.text \" standard revolutionized web development.\"
--     ]
-- @
--
-- __Output:__
--
-- @
-- \<p\>The \<abbr title=\"Hypertext Markup Language\"\>HTML\<\/abbr\> standard revolutionized web development.\<\/p\>
-- @
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
-- @
-- Html.address []
--     [ Html.text \"Adventuring through Middle-earth? Get in touch with the Fellowship:\"
--     , Html.br []
--     , Html.text \"Eagle-Mail: \"
--     , Html.a
--         [ Attr.href \"mailto:frodo\@theringquest.me\" ]
--         [ Html.text \"frodo\@theringquest.me\" ]
--     , Html.br []
--     , Html.text \"Entphone: 1-800-ENT-GUARD\"
--     ]
-- @
--
-- __Output:__
--
-- @
-- \<address\>
--     Adventuring through Middle-earth? Get in touch with the Fellowship:\<br\>
--     Eagle-Mail: \<a href=\"mailto:frodo\@theringquest.me\"\>frodo\@theringquest.me\<\/a\>\<br\>
--     Entphone: 1-800-ENT-GUARD
-- \<\/address\>
-- @
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
-- @
-- Html.map
--     [ Attr.name \"stonehenge-map\" ]
--     [ Html.area
--         [ Attr.alt    \"Stonehenge\"
--         , Attr.coords \"150,150,100\"
--         , Attr.shape  \"circle\"
--         ]
--     ]
-- @
--
-- __Output:__
--
-- @
-- \<map name=\"stonehenge-map\"\>
--     \<area alt=\"Stonehenge\" coords=\"150,150,100\" shape=\"circle\"\>
-- \<\/map\>
-- @
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
-- @
-- Html.article []
--     [ Html.h2 []
--         [ Html.text \"Franz Kafka\'s Novels\" ]
--     , Html.ul []
--         [ Html.li []
--             [ Html.text \"The Man Who Disappeared\" ]
--         , Html.li []
--             [ Html.text \"The Trial\" ]
--         , Html.li []
--             [ Html.text \"The Castle\" ]
--         ]
--     ]
-- @
--
-- __Output:__
--
-- @
-- \<article\>
--     \<h2\>Franz Kafka\'s Novels\<\/h2\>
--     \<ul\>
--         \<li\>The Man Who Disappeared\<\/li\>
--         \<li\>The Trial\<\/li\>
--         \<li\>The Castle\<\/li\>
--     \<\/ul\>
-- \<\/article\>
-- @
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
-- @
-- Html.aside []
--     [ Html.h4 []
--         [ Html.text \"House of the Dragon\" ]
--     , Html.p []
--         [ Html.text \"House of the Dragon is a prequal to Game of Thrones.\" ]
--     ]
-- @
--
-- __Output:__
--
-- @
-- \<aside\>
--     \<h4\>House of the Dragon\<\/h4\>
--     \<p\>House of the Dragon is a prequel to Game of Thrones.\<\/p\>
-- \<\/aside\>
-- @
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
-- @
-- Html.audio
--     [ Attr.controls True ]
--     [ Html.source
--         [ Attr.src   \"bossfight-warp.mp3\"
--         , Attr.type_ \"audio\/mpeg\"
--         ]
--     , Html.text \"Your browser does not support the audio tag.\"
--     ]
-- @
--
-- __Output:__
--
-- @
-- \<audio controls\>
--     \<source src=\"bossfight-warp.mp3\" type=\"audio\/mpeg\"\>
--     Your browser does not support the audio tag.
-- \<\/audio\>
-- @
audio :: [Attribute] -> [Html lng] -> Html lng
audio = ParentNode "<audio" "</audio>"
{-# INLINE audio #-}


-- | Generates an HTML /\<b\>/ element with the given attributes and contents.
--
-- The /\<b\>/ element is used to apply bold formatting to the enclosed text, indicating that the content should be presented in a stronger
-- or more prominent manner, without implying any specific semantic importance.
--
-- ==== __Example__
--
-- __Input:__
--
-- @
-- Html.p []
--     [ Html.text \"Gary Gygax hands Fry his \"
--     , Html.b []
--         [ Html.text \"+1 mace\" ]
--     , Html.text \".\"
--     ]
-- @
--
-- __Output:__
--
-- @
-- \<p\>Gary Gygax hands Fry his \<b\>+1 mace\<\/b\>.\<\/p\>
-- @
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
-- @
-- Html.head []
--     [ Html.base
--         [ Attr.href \"https:\/\/news.ycombinator.com\" ]
--     ]
-- @
--
-- __Output:__
--
-- @
-- \<head\>
--     \<base href=\"https:\/\/news.ycombinator.com\"\>
-- \<\/head\>
-- @
base :: [Attribute] -> Html lng
base = LeafNode "<base"
{-# INLINE base #-}


-- | Generates an HTML /\<bdi\>/ element with the given attributes and contents.
--
-- The /\<bdi\>/ element isolates a span of text that needs to be formatted in a specific direction for languages that are written
-- right-to-left, like Arabic or Hebrew, within a predominantly left-to-right text.
--
-- ==== __Example__
--
-- __Input:__
--
-- @
-- Html.ul []
--     [ Html.li []
--         [ Html.text \"Character \"
--         , Html.bdi []
--             [ Html.text \"فاصوليا\" ]
--         , Html.text \": Human\"
--         ]
--     , Html.li []
--         [ Html.text \"Character \"
--         , Html.bdi []
--             [ Html.text \"Lucy\" ]
--         , Html.text \": Demon\"
--         ]
--     , Html.li []
--         [ Html.text \"Character \"
--         , Html.bdi []
--             [ Html.text \"Elfo\" ]
--         , Html.text \": Elf\"
--         ]
--     ]
-- @
--
-- __Output:__
--
-- @
-- \<ul\>
--     \<li\>Character \<bdi\>فاصوليا\<\/bdi\>: Human\<\/li\>
--     \<li\>Character \<bdi\>Lucy\<\/bdi\>: Demon\<\/li\>
--     \<li\>Character \<bdi\>Elfo\<\/bdi\>: Elf\<\/li\>
-- \<\/ul\>
-- @
bdi :: [Attribute] -> [Html lng] -> Html lng
bdi = ParentNode "<bdi" "</bdi>"
{-# INLINE bdi #-}


-- | Generates an HTML /\<bdo\>/ element with the given attributes and contents.
--
-- The /\<bdo\>/ element overrides the bidirectional algorithm setting of the surrounding text. It is used to explicitly define the text
-- direction, either left-to-right or right-to-left, for languages with different writing directions.
--
-- ==== __Example__
--
-- __Input:__
--
-- @
-- Html.bdo
--     [ Attr.dir \"rtl\" ]
--     [ Html.text \"The sun rises in the east and sets in the west.\" ]
-- @
--
-- __Output:__
--
-- @
-- \<bdo dir=\"rtl\"\>The sun rises in the east and sets in the west.\<\/bdo\>
-- @
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
-- @
-- Html.blockquote []
--     [ Html.p []
--         [ Html.text \"When life gives you lemons, make lemonade.\" ]
--     ]
-- @
--
-- __Output:__
--
-- @
-- \<blockquote\>
--     \<p\>When life gives you lemons, make lemonade.\<\/p\>
-- \<\/blockquote\>
-- @
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
-- @
-- Html.body []
--     [ Html.h1 []
--         [ Html.text \"An Introduction to Elm\" ]
--     , Html.p []
--         [ Html.text \"Elm is a functional language for front-end web development.\" ]
--     ]
-- @
--
-- __Output:__
--
-- @
-- \<body\>
--     \<h1\>An Introduction to Elm\<\/h1\>
--     \<p\>Elm is a functional language for front-end web development.\<\/p\>
-- \<\/body\>
-- @
body :: [Attribute] -> [Html lng] -> Html lng
body = ParentNode "<body" "</body>"
{-# INLINE body #-}


-- | Generates an HTML /\<br\>/ element with the given attributes.
--
-- The /\<br\>/ element represents a line break, indicating that the content following it should appear on the next line. It is used to
-- create single-line breaks within text.
--
-- ==== __Example__
--
-- __Input:__
--
-- @
-- Html.p []
--     [ Html.text \"That which we call a rose\"
--     , Html.br []
--     , Html.text \"By any other name would smell as sweet.\"
--     ]
-- @
--
-- __Output:__
--
-- @
-- \<p\>That which we call a rose\<br\>By any other name would smell as sweet.\<\/p\>
-- @
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
-- @
-- Html.button
--     [ Attr.type_ \"submit\" ]
--     [ Html.text \"Log in\" ]
-- @
--
-- __Output:__
--
-- @
-- \<button type=\"submit\"\>Log in\<\/button\>
-- @
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
-- @
-- Html.canvas
--     [ Attr.height \"500\"
--     , Attr.width  \"500\"
--     ]
--     [ Html.text \"Your browser does not support the canvas tag.\" ]
-- @
--
-- __Output:__
--
-- @
-- \<canvas height=\"500\" width=\"500\"\>
--     Your browser does not support the canvas tag.
-- \<\/canvas\>
-- @
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
-- @
-- Html.table []
--     [ Html.caption []
--         [ Html.text \"Monthly earnings\" ]
--     , Html.tr []
--         [ Html.th []
--             [ Html.text \"Month\" ]
--         , Html.th []
--             [ Html.text \"Earnings\" ]
--         ]
--     , Html.tr []
--         [ Html.td []
--             [ Html.text \"January\" ]
--         , Html.td []
--             [ Html.text \"$4,456\" ]
--         ]
--     , Html.tr []
--         [ Html.td []
--             [ Html.text \"February\" ]
--         , Html.td []
--             [ Html.text \"$4,230\" ]
--         ]
--     ]
-- @
--
-- __Output:__
--
-- @
-- \<table\>
--     \<caption\>Monthly earnings\<\/caption\>
--     \<tr\>
--         \<th\>Month\<\/th\>
--         \<th\>Earnings\<\/th\>
--     \<\/tr\>
--     \<tr\>
--         \<td\>January\<\/td\>
--         \<td\>$4,456\<\/td\>
--     \<\/tr\>
--     \<tr\>
--         \<td\>February\<\/td\>
--         \<td\>$4,230\<\/td\>
--     \<\/tr\>
-- \<\/table\>
-- @
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
-- @
-- Html.p []
--     [ Html.text \"My favorite movie is \"
--     , Html.cite []
--         [ Html.text \"Psycho\" ]
--     , Html.text \" by Alfred Hitchcock.\"
--     ]
-- @
--
-- __Output:__
--
-- @
-- \<p\>My favorite movie is \<cite\>Psycho\<\/cite\> by Alfred Hitchcock.\<\/p\>
-- @
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
-- @
-- Html.p []
--     [ Html.text \"The \"
--     , Html.code []
--         [ Html.text \"map\" ]
--     , Html.text \" function is a higher-order function.\"
--     ]
-- @
--
-- __Output:__
--
-- @
-- \<p\>The \<code\>map\<\/code\> function is a higher-order function.\<\/p\>
-- @
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
-- @
-- Html.table []
--     [ Html.colgroup []
--         [ Html.col
--             [ Attr.span  \"2\"
--             , Attr.style \"background-color: red;\"
--             ]
--         , Html.col
--             [ Attr.style \"background-color: blue;\" ]
--         ]
--     , Html.tr []
--         [ Html.th []
--             [ Html.text \"ISBN\" ]
--         , Html.th []
--             [ Html.text \"Author\" ]
--         , Html.th []
--             [ Html.text \"Title\" ]
--         ]
--     , Html.tr []
--         [ Html.td []
--             [ Html.text \"9781617293764\" ]
--         , Html.td []
--             [ Html.text \"Will Kurt\" ]
--         , Html.td []
--             [ Html.text \"Get Programming with Haskell\" ]
--         ]
--     , Html.tr []
--         [ Html.td []
--             [ Html.text \"9781617295409\" ]
--         , Html.td []
--             [ Html.text \"Vitaly Bragilevsky\" ]
--         , Html.td []
--             [ Html.text \"Haskell in Depth\" ]
--         ]
--     ]
-- @
--
-- __Output:__
--
-- @
-- \<table\>
--     \<colgroup\>
--         \<col span=\"2\" style=\"background-color: red;\"\>
--         \<col style=\"background-color: blue;\"\>
--     \<\/colgroup\>
--     \<tr\>
--         \<th\>ISBN\<\/th\>
--         \<th\>Author\<\/th\>
--         \<th\>Title\<\/th\>
--     \<\/tr\>
--     \<tr\>
--         \<td\>9781617293764\<\/td\>
--         \<td\>Will Kurt\<\/td\>
--         \<td\>Get Programming with Haskell\<\/td\>
--     \<\/tr\>
--     \<tr\>
--         \<td\>9781617295409\<\/td\>
--         \<td\>Vitaly Bragilevsky\<\/td\>
--         \<td\>Haskell in Depth\<\/td\>
--     \<\/tr\>
-- \<\/table\>
-- @
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
-- @
-- Html.table []
--     [ Html.colgroup []
--         [ Html.col
--             [ Attr.span  \"2\"
--             , Attr.style \"background-color: purple;\"
--             ]
--         , Html.col
--             [ Attr.style \"background-color: white;\" ]
--         ]
--     , Html.tr []
--         [ Html.th []
--             [ Html.text \"Framework\" ]
--         , Html.th []
--             [ Html.text \"Language\" ]
--         , Html.th []
--             [ Html.text \"Github Stars\" ]
--         ]
--     , Html.tr []
--         [ Html.td []
--             [ Html.text \"IHP\" ]
--         , Html.td []
--             [ Html.text \"Haskell\" ]
--         , Html.td []
--             [ Html.text \"3.4k\" ]
--         ]
--     , Html.tr []
--         [ Html.td []
--             [ Html.text \"Phoenix\" ]
--         , Html.td []
--             [ Html.text \"Elixir\" ]
--         , Html.td []
--             [ Html.text \"18.3k\" ]
--         ]
--     ]
-- @
--
-- __Output:__
--
-- @
-- \<table\>
--     \<colgroup\>
--         \<col span=\"2\" style=\"background-color: purple;\"\>
--         \<col style=\"background-color: white;\"\>
--     \<\/colgroup\>
--     \<tr\>
--         \<th\>Framework\<\/th\>
--         \<th\>Language\<\/th\>
--         \<th\>Github Stars\<\/th\>
--     \<\/tr\>
--     \<tr\>
--         \<td\>IHP\<\/td\>
--         \<td\>Haskell\<\/td\>
--         \<td\>3.4k\<\/td\>
--     \<\/tr\>
--     \<tr\>
--         \<td\>Phoenix\<\/td\>
--         \<td\>Elixir\<\/td\>
--         \<td\>18.3k\<\/td\>
--     \<\/tr\>
-- \<\/table\>
-- @
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
-- @
-- Html.ul []
--     [ Html.li []
--         [ Html.data_
--             [ Attr.value \"1976\" ]
--             [ Html.text  \"Apple I\" ]
--         ]
--     , Html.li []
--         [ Html.data_
--             [ Attr.value \"1977\" ]
--             [ Html.text  \"Apple II\" ]
--         ]
--     , Html.li []
--         [ Html.data_
--             [ Attr.value \"1980\" ]
--             [ Html.text  \"Apple III\" ]
--         ]
--     ]
-- @
--
-- __Output:__
--
-- @
-- \<ul\>
--     \<li\>
--         \<data value=\"1976\"\>Apple I\<\/data\>
--     \<\/li\>
--     \<li\>
--         \<data value=\"1977\"\>Apple II\<\/data\>
--     \<\/li\>
--     \<li\>
--         \<data value=\"1980\"\>Apple III\<\/data\>
--     \<\/li\>
-- \<\/ul\>
-- @
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
-- @
-- Html.label []
--     [ Html.text \"Language:\"
--     , Html.input
--         [ Attr.name \"language\"
--         , Attr.list \"languages\"
--         ]
--     , Html.datalist
--         [ Attr.id \"languages\" ]
--         [ Html.option
--             [ Attr.value \"English\" ] []
--         , Html.option
--             [ Attr.value \"German\" ] []
--         ]
--     ]
-- @
--
-- __Output:__
--
-- @
-- \<label\>
--     Language:
--     \<input name=\"language\" list=\"languages\"\>
--     \<datalist id=\"languages\">
--         \<option value=\"English\"\>\<\/option\>
--         \<option value=\"German\"\>\<\/option\>
--     \<\/datalist\>
-- \<\/label\>
-- @
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
-- @
-- Html.dl []
--     [ Html.dt []
--         [ Html.text \"Chicken adobo\" ]
--     , Html.dd []
--         [ Html.text \"Chicken marinated in vinegar and soy sauce\" ]
--     , Html.dt []
--         [ Html.text \"Pancit\" ]
--     , Html.dd []
--         [ Html.text \"Rice noodles with pork\" ]
--     ]
-- @
--
-- __Output:__
--
-- @
-- \<dl\>
--     \<dt\>Chicken adobo\<\/dt\>
--     \<dd\>Chicken marinated in vinegar and soy sauce\<\/dd\>
--     \<dt\>Pancit\<\/dt\>
--     \<dd\>Rice noodles with pork\<\/dd\>
-- \<\/dl\>
-- @
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
-- @
-- Html.p []
--     [ Html.text \"Appointments are available on \"
--     , Html.del []
--         [ Html.text \"Tuesdays\" ]
--     , Html.text \", Wednesdays and Fridays.\"
--     ]
-- @
--
-- __Output:__
--
-- @
-- \<p\>Appointments are available on \<del\>Tuesdays\<\/del\>, Wednesdays and Fridays.\<\/p\>
-- @
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
-- @
-- Html.details []
--     [ Html.summary []
--         [ Html.text \"Phrasal Verbs\" ]
--     , Html.p []
--         [ Html.text \"A phrasal verb combines a normal verb with either a preposition or an adverb.\" ]
--     ]
-- @
--
-- __Output:__
--
-- @
-- \<details\>
--     \<summary\>Phrasal Verbs\<\/summary\>
--     \<p\>A phrasal verb combines a normal verb with either a preposition or an adverb.\<\/p\>
-- \<\/details\>
-- @
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
-- @
-- Html.p []
--     [ Html.dfn []
--         [ Html.text \"ChatGPT\" ]
--     , Html.text \" is an OpenAI language model.\"
--     ]
-- @
--
-- __Output:__
--
-- @
-- \<p\>\<dfn\>ChatGPT\<\/dfn\> is an OpenAI language model.\<\/p\>
-- @
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
-- @
-- Html.body []
--     [ Html.button
--         [ Attr.onclick \"openConfirmDialog()\" ]
--         [ Html.text \"Confirm\" ]
--     , Html.dialog
--         [ Attr.id "confirmDialog" ]
--         [ Html.h1 []
--             [ Html.text \"Confirm Transaction\" ]
--         , Html.p []
--             [ Html.text \"Would you really like to purchase this item?\" ]
--         , Html.button
--             [ Html.onclick \"confirmTransaction()\" ]
--             [ Html.text \"Yes\" ]
--         , Html.button
--             [ Html.onclick \"cancelTransaction()\" ]
--             [ Html.text \"No\" ]
--         ]
--     ]
-- @
--
-- __Output:__
--
-- @
-- \<body\>
--     \<button onclick=\"openConfirmDialog()\"\>Confirm\<\/button\>
--     \<dialog id=\"confirmDialog\"\>
--         \<h1\>Confirm Transaction\<\/h1\>
--         \<p\>Would you really like to purchase this item?\<\/p\>
--         \<button onclick=\"confirmTransaction()\"\>Yes\<\/button\>
--         \<button onclick=\"cancelTransaction()\"\>No\<\/button\>
--     \<\/dialog\>
-- \<\/body\>
-- @
dialog :: [Attribute] -> [Html lng] -> Html lng
dialog = ParentNode "<dialog" "</dialog>"
{-# INLINE dialog #-}


-- | Generates an HTML /\<div\>/ element with the given attributes and contents.
--
-- The /\<div\>/ element is a generic container that is often used to group and structure content for styling or scripting purposes. It does
-- not inherently carry any specific meaning on its own.
div :: [Attribute] -> [Html lng] -> Html lng
div = ParentNode "<div" "</div>"
{-# INLINE div #-}


-- | Generates an HTML /\<dl\>/ element with the given attributes and contents.
--
-- The /\<dl\>/ element represents a description list, consisting of terms (/\<dt\>/) and their corresponding descriptions (/\<dd\>/).
-- It is commonly used to present glossaries or sets of definitions.
dl :: [Attribute] -> [Html lng] -> Html lng
dl = ParentNode "<dl" "</dl>"
{-# INLINE dl #-}


-- | Generates an HTML /\<dt\>/ element with the given attributes and contents.
--
-- The /\<dt\>/ element is used withing a description list (/\<dl\>/) to define a term or name that is followed by its corresponding
-- description (/\<dd\>/).
dt :: [Attribute] -> [Html lng] -> Html lng
dt = ParentNode "<dt" "</dt>"
{-# INLINE dt #-}


-- | Generates an HTML /\<em\>/ element with the given attributes and contents.
--
-- The /\<em\>/ element is used to indicate text that should be emphasized, typically displayed in italic style. It carries semantic
-- meaning, implying that the enclosed content is of particular importance or should be stressed.
em :: [Attribute] -> [Html lng] -> Html lng
em = ParentNode "<em" "</em>"
{-# INLINE em #-}


-- | Generates an HTML /\<embed\>/ element with the given attributes.
--
-- The /\<embed\>/ element embeds external content, typically multimedia like audio, video, or interactive applications, directly into a web
-- page. It is used to seamlessly integrate content from other sources.
embed :: [Attribute] -> Html lng
embed = LeafNode "<embed"
{-# INLINE embed #-}


-- | Generates an HTML /\<fieldset\>/ element with the given attributes and contents.
--
-- The /\<fieldset\>/ element is used to group related form elements together, providing a visual and logical grouping for better
-- organization and styling. It is often used in forms to structure related input elements.
fieldset :: [Attribute] -> [Html lng] -> Html lng
fieldset = ParentNode "<fieldset" "</fieldset>"
{-# INLINE fieldset #-}


-- | Generates an HTML /\<figcaption\>/ element with the given attributes and contents.
--
-- The /\<figcaption\>/ element is used to provide a caption or description for a /\<figure\>/ element, typically used to describe images,
-- illustrations, or multimedia content.
figcaption :: [Attribute] -> [Html lng] -> Html lng
figcaption = ParentNode "<figcaption" "</figcaption>"
{-# INLINE figcaption #-}


-- | Generates an HTML /\<figure\>/ element with the given attributes and contents.
--
-- The /\<figure\>/ element is used to encapsulate and group related content, often used with an associated /\<figcaption\>/ to provide
-- context or explanation for the content within.
figure :: [Attribute] -> [Html lng] -> Html lng
figure = ParentNode "<figure" "</figure>"
{-# INLINE figure #-}


-- | Generates an HTML /\<footer\>/ element with the given attributes and contents.
--
-- The /\<footer\>/ element represents a footer section or container typically used to include information such as authorship, copyright
-- details, or contact information at the bottom of a document or section.
footer :: [Attribute] -> [Html lng] -> Html lng
footer = ParentNode "<footer" "</footer>"
{-# INLINE footer #-}


-- | Generates an HTML /\<form\>/ element with the given attributes and contents.
--
-- The /\<form\>/ element is used to create a container for a set of form controls like input fields, checkboxes, radio buttons, and
-- buttons. It is used to collect user input that can be submitted to a server for processing.
form :: [Attribute] -> [Html lng] -> Html lng
form = ParentNode "<form" "</form>"
{-# INLINE form #-}


-- | Generates an HTML /\<h1\>/ element with the given attributes and contents.
--
-- The /\<h1\>/ element represents the highest-level heading, typically used to indicate the main topic or section of a document.
h1 :: [Attribute] -> [Html lng] -> Html lng
h1 = ParentNode "<h1" "</h1>"
{-# INLINE h1 #-}


-- | Generates an HTML /\<h2\>/ element with the given attributes and contents.
--
-- The /\<h2\>/ element signifies a heading of a slightly lower level than /\<h1\>/, often used to subdivide content within sections of a
-- document.
h2 :: [Attribute] -> [Html lng] -> Html lng
h2 = ParentNode "<h2" "</h2>"
{-# INLINE h2 #-}


-- | Generates an HTML /\<h3\>/ element with the given attributes and contents.
--
-- The /\<h3\>/ element denotes a heading that is of a lower level than /\<h2\>/, typically used to indicate sub-sections or finer details
-- within the content.
h3 :: [Attribute] -> [Html lng] -> Html lng
h3 = ParentNode "<h3" "</h3>"
{-# INLINE h3 #-}


-- | Generates an HTML /\<h4\>/ element with the given attributes and contents.
--
-- The /\<h4\>/ element represents a heading with a lower hierarchical level than /\<h3\>/, usually utilized to introduce sub-subsections or
-- finer points within the content.
h4 :: [Attribute] -> [Html lng] -> Html lng
h4 = ParentNode "<h4" "</h4>"
{-# INLINE h4 #-}


-- | Generates an HTML /\<h5\>/ element with the given attributes and contents.
--
-- The /\<h5\>/ element signifies a heading of a reduced level compared to /\<h4\>/, often used to introduce even more specific details or
-- subsections within the content.
h5 :: [Attribute] -> [Html lng] -> Html lng
h5 = ParentNode "<h5" "</h5>"
{-# INLINE h5 #-}


-- | Generates an HTML /\<h6\>/ element with the given attributes and contents.
--
-- The /\<h6\>/ element defines the lowest-level heading among the heading elements, typically employed for the most specific details or
-- subsections within the content.
h6 :: [Attribute] -> [Html lng] -> Html lng
h6 = ParentNode "<h6" "</h6>"
{-# INLINE h6 #-}


-- | Generates an HTML /\<head\>/ element with the given attributes and contents.
--
-- The /\<head\>/ element serves as a container for metadata and other non-visible information about the document, such as title, character
-- encoding, and linked stylesheets.
head :: [Attribute] -> [Html lng] -> Html lng
head = ParentNode "<head" "</head>"
{-# INLINE head #-}


-- | Generates an HTML /\<header\>/ element with the given attributes and contents.
--
-- The /\<header\>/ element represents a container for introductory content or a group of navigation and branding elements typically found
-- at the top of a section or page.
header :: [Attribute] -> [Html lng] -> Html lng
header = ParentNode "<header" "</header>"
{-# INLINE header #-}


-- | Generates an HTML /\<hgroup\>/ element with the given attributes and contents.
--
-- The /\<hgroup\>/ element groups together multiple heading elements (/\<h1\>/ to /\<h6\>/) as a single entity, often used to create a
-- heading with a subheading or a title with a subtitle.
hgroup :: [Attribute] -> [Html lng] -> Html lng
hgroup = ParentNode "<hgroup" "</hgroup>"
{-# INLINE hgroup #-}


-- | Generates an HTML /\<hr\>/ element with the given attributes.
--
-- The /\<hr\>/ element is a self-closing tag that represents a thematic break or separation between content, typically displayed as a
-- horizontal line.
hr :: [Attribute] -> Html lng
hr = LeafNode "<hr"
{-# INLINE hr #-}


-- | Generates an HTML /\<html\>/ element with the given attributes and contents.
--
-- The /\<html\>/ element encloses the entire HTML document and serves as the root element, containing all other HTML elements like
-- /\<head\>/ and /\<body\>/.
html :: [Attribute] -> [Html lng] -> Html lng
html = ParentNode "<html" "</html>"
{-# INLINE html #-}


-- | Generates an HTML /\<i\>/ element with the given attributes and contents.
--
-- The /\<i\>/ element is used to apply italics to text, indicating that the content within should be styled in an italic font.
i :: [Attribute] -> [Html lng] -> Html lng
i = ParentNode "<i" "</i>"
{-# INLINE i #-}


-- | Generates an HTML /\<iframe\>/ element with the given attributes and contents.
--
-- The /\<iframe\>/ element embeds another HTML document within the current document, allowing for the display of external content such as
-- web pages or multimedia.
iframe :: [Attribute] -> [Html lng] -> Html lng
iframe = ParentNode "<iframe" "</iframe>"
{-# INLINE iframe #-}


-- | Generates an HTML /\<img\>/ element with the given attributes.
--
-- The /\<img\>/ element embeds an image in the document, displaying the visual content specified by the /src/ attribute.
img :: [Attribute] -> Html lng
img = LeafNode "<img"
{-# INLINE img #-}


-- | Generates an HTML /\<input\>/ element with the given attributes.
--
-- The /\<input\>/ element creates a user-input field, such as a text box, radio button, checkbox, or submit button, allowing users to enter
-- or select data.
input :: [Attribute] -> Html lng
input = LeafNode "<input"
{-# INLINE input #-}


-- | Generates an HTML /\<ins\>/ element with the given attributes and contents.
--
-- The /\<ins\>/ element represents text that has been inserted into the document after the original content, often displayed with an
-- underline.
ins :: [Attribute] -> [Html lng] -> Html lng
ins = ParentNode "<ins" "</ins>"
{-# INLINE ins #-}


-- | Generates an HTML /\<kbd\>/ element with the given attributes and contents.
--
-- The /\<kbd\>/ element is used to indicate user input, typically keyboard input, within the content, often rendering the enclosed text in
-- a monospace font.
kbd :: [Attribute] -> [Html lng] -> Html lng
kbd = ParentNode "<kbd" "</kbd>"
{-# INLINE kbd #-}


-- | Generates an HTML /\<label\>/ element with the given attributes and contents.
--
-- The /\<label\>/ element associates a text label with a form control, enhancing usability and accessibility by providing a descriptive
-- label for user interaction.
label :: [Attribute] -> [Html lng] -> Html lng
label = ParentNode "<label" "</label>"
{-# INLINE label #-}


-- | Generates an HTML /\<legend\>/ element with the given attributes and contents.
--
-- The /\<legend\>/ element provides a caption or title for a /\<fieldset\>/ element, offering a concise description or heading for the
-- group of related form controls within the fieldset.
legend :: [Attribute] -> [Html lng] -> Html lng
legend = ParentNode "<legend" "</legend>"
{-# INLINE legend #-}


-- | Generates an HTML /\<li\>/ element with the given attributes and contents.
--
-- The /\<li\>/ element defines a list item within an ordered (/\<ol\>/) or unordered (/\<ul\>/) list, representing an individual entry or
-- point in the list.
li :: [Attribute] -> [Html lng] -> Html lng
li = ParentNode "<li" "</li>"
{-# INLINE li #-}


-- | Generates an HTML /\<link\>/ element with the given attributes.
--
-- The /\<link\>/ element associates external resources, typically stylesheets, with the document, enabling the application of additional
-- styles and behaviors to the content.
link :: [Attribute] -> Html lng
link = LeafNode "<link"
{-# INLINE link #-}


-- | Generates an HTML /\<main\>/ element with the given attributes and contents.
--
-- The /\<main\>/ element indicates the main content of the document, providing a distinct region for the central subject matter of the
-- webpage.
main :: [Attribute] -> [Html lng] -> Html lng
main = ParentNode "<main" "</main>"
{-# INLINE main #-}


-- | Generates an HTML /\<map\>/ element with the given attributes and contents.
--
-- The /\<map\>/ element defines an image map, which is used to associate clickable areas within an image to specific links or actions.
map :: [Attribute] -> [Html lng] -> Html lng
map = ParentNode "<map" "</map>"
{-# INLINE map #-}


-- | Generates an HTML /\<mark\>/ element with the given attributes and contents.
--
-- The /\<mark\>/ element highlights text as if it has been marked for reference or emphasis, often rendering the enclosed content with a
-- distinctive background color.
mark :: [Attribute] -> [Html lng] -> Html lng
mark = ParentNode "<mark" "</mark>"
{-# INLINE mark #-}


-- | Generates an HTML /\<menu\>/ element with the given attributes and contents.
--
-- The /\<menu\>/ element represents a list of commands or options, typically used for contextual menus or navigation menus in web
-- applications.
menu :: [Attribute] -> [Html lng] -> Html lng
menu = ParentNode "<menu" "</menu>"
{-# INLINE menu #-}


-- | Generates an HTML /\<meta\>/ element with the given attributes.
--
-- The /\<meta\>/ element provides metadata about the document, such as character encoding, authorship, and viewport settings, which are
-- used by browsers and search engines but not typically displayed to users.
meta :: [Attribute] -> Html lng
meta = LeafNode "<meta"
{-# INLINE meta #-}


-- | Generates an HTML /\<meter\>/ element with the given attributes and contents.
--
-- The /\<meter\>/ element represents a scalar measurement within a known range, often used to display gauges, progress bars, or other
-- visual representations of data.
meter :: [Attribute] -> [Html lng] -> Html lng
meter = ParentNode "<meter" "</meter>"
{-# INLINE meter #-}


-- | Generates an HTML /\<nav\>/ element with the given attributes and contents.
--
-- The /\<nav\>/ element defines a section of navigation links or menus, typically containing links to other pages, sections of the current
-- page, or related content.
nav :: [Attribute] -> [Html lng] -> Html lng
nav = ParentNode "<nav" "</nav>"
{-# INLINE nav #-}


-- | Generates an HTML /\<noscript\>/ element with the given attributes and contents.
--
-- The /\<noscript\>/ element is used to provide alternative content that should be displayed if a browser does not support scripting or if
-- scripting is disabled. It is often used to display a message or instructions for enabling JavaScript.
noscript :: [Attribute] -> [Html lng] -> Html lng
noscript = ParentNode "<noscript" "</noscript>"
{-# INLINE noscript #-}


-- | Generates an HTML /\<object\>/ element with the given attributes and contents.
--
-- The /\<object\>/ element embeds external resources, such as multimedia or interactive content, into a web page. It is often used to embed
-- multimedia content like audio, video, or Flash animations.
object :: [Attribute] -> [Html lng] -> Html lng
object = ParentNode "<object" "</object>"
{-# INLINE object #-}


-- | Generates an HTML /\<ol\>/ element with the given attributes and contents.
--
-- The /\<ol\>/ element is used to create an ordered list, where each list item is numbered sequentially. It is commonly used to represent
-- items with a specific order or sequence.
ol :: [Attribute] -> [Html lng] -> Html lng
ol = ParentNode "<ol" "</ol>"
{-# INLINE ol #-}


-- | Generates an HTML /\<optgroup\>/ element with the given attributes and contents.
--
-- The /\<optgroup\>/ element is used to group related options within a /\<select\>/ element, providing a way to create hierarchical or
-- categorized dropdown lists.
optgroup :: [Attribute] -> [Html lng] -> Html lng
optgroup = ParentNode "<optgroup" "</optgroup>"
{-# INLINE optgroup #-}


-- | Generates an HTML /\<option\>/ element with the given attributes and contents.
--
-- The /\<option\>/ element is used within a /\<select\>/ or /\<datalist\>/ element to define individual options that users can select from
-- in a dropdown list or autocomplete input.
option :: [Attribute] -> [Html lng] -> Html lng
option = ParentNode "<option" "</option>"
{-# INLINE option #-}


-- | Generates an HTML /\<output\>/ element with the given attributes and contents.
--
-- The /\<output\>/ element is employed to present the outcome of calculations, user interactions, or scripting actions, often utilized in
-- combination with form components and scripts.
output :: [Attribute] -> [Html lng] -> Html lng
output = ParentNode "<output" "</output>"
{-# INLINE output #-}


-- | Generates an HTML /\<p\>/ element with the given attributes and contents.
--
-- The /\<p\>/ element designates a paragraph of text, providing a structured way to separate and present blocks of content in a readable
-- format.
p :: [Attribute] -> [Html lng] -> Html lng
p = ParentNode "<p" "</p>"
{-# INLINE p #-}


-- | Generates an HTML /\<picture\>/ element with the given attributes and contents.
--
-- The /\<picture\>/ element provides multiple sources for an image, allowing the browser to select the most appropriate version based on
-- factors such as device size or resolution.
picture :: [Attribute] -> [Html lng] -> Html lng
picture = ParentNode "<picture" "</picture>"
{-# INLINE picture #-}


-- | Generates an HTML /\<pre\>/ element with the given attributes and contents.
--
-- The /\<pre\>/ element defines preformatted text, preserving both whitespace and line breaks, often used for displaying code or other
-- content with fixed formatting.
pre :: [Attribute] -> [Html lng] -> Html lng
pre = ParentNode "<pre" "</pre>"
{-# INLINE pre #-}


-- | Generates an HTML /\<progress\>/ element with the given attributes and contents.
--
-- The /\<progress\>/ element represents the completion progress of a task or process, typically displayed as a bar or other visual
-- indicator.
progress :: [Attribute] -> [Html lng] -> Html lng
progress = ParentNode "<progress" "</progress>"
{-# INLINE progress #-}


-- | Generates an HTML /\<q\>/ element with the given attributes and contents.
--
-- The /\<q\>/ element encloses a short inline quotation, typically surrounded by quotation marks or other formatting.
q :: [Attribute] -> [Html lng] -> Html lng
q = ParentNode "<q" "</q>"
{-# INLINE q #-}


-- | Generates an HTML /\<rp\>/ element with the given attributes and contents.
--
-- The /\<rp\>/ element provides fallback parentheses for browsers that do not support the ruby annotation feature, often used in East Asian
-- typography.
rp :: [Attribute] -> [Html lng] -> Html lng
rp = ParentNode "<rp" "</rp>"
{-# INLINE rp #-}


-- | Generates an HTML /\<rt\>/ element with the given attributes and contents.
--
-- The /\<rt\>/ element defines the pronunciation of characters in a ruby annotation, commonly used to provide phonetic information for East
-- Asian languages.
rt :: [Attribute] -> [Html lng] -> Html lng
rt = ParentNode "<rt" "</rt>"
{-# INLINE rt #-}


-- | Generates an HTML /\<ruby\>/ element with the given attributes and contents.
--
-- The /\<ruby\>/ element represents a ruby annotation, used to provide additional pronunciation or translation information for East Asian
-- characters.
ruby :: [Attribute] -> [Html lng] -> Html lng
ruby = ParentNode "<ruby" "</ruby>"
{-# INLINE ruby #-}


-- | Generates an HTML __s__ element with the given attributes and contents.
s :: [Attribute] -> [Html lng] -> Html lng
s = ParentNode "<s" "</s>"
{-# INLINE s #-}


-- | Generates an HTML __samp__ element with the given attributes and contents.
samp :: [Attribute] -> [Html lng] -> Html lng
samp = ParentNode "<samp" "</samp>"
{-# INLINE samp #-}


-- | Generates an HTML __script__ element with the given attributes and contents.
script :: [Attribute] -> [Html lng] -> Html lng
script = ParentNode "<script" "</script>"
{-# INLINE script #-}


-- | Generates an HTML __section__ element with the given attributes and contents.
section :: [Attribute] -> [Html lng] -> Html lng
section = ParentNode "<section" "</section>"
{-# INLINE section #-}


-- | Generates an HTML __select__ element with the given attributes and contents.
select :: [Attribute] -> [Html lng] -> Html lng
select = ParentNode "<select" "</select>"
{-# INLINE select #-}


-- | Generates an HTML __slot__ element with the given attributes and contents.
slot :: [Attribute] -> [Html lng] -> Html lng
slot = ParentNode "<slot" "</slot>"
{-# INLINE slot #-}


-- | Generates an HTML __small__ element with the given attributes and contents.
small :: [Attribute] -> [Html lng] -> Html lng
small = ParentNode "<small" "</small>"
{-# INLINE small #-}


-- | Generates an HTML __source__ element with the given attributes.
source :: [Attribute] -> Html lng
source = LeafNode "<source"
{-# INLINE source #-}


-- | Generates an HTML __span__ element with the given attributes and contents.
span :: [Attribute] -> [Html lng] -> Html lng
span = ParentNode "<span" "</span>"
{-# INLINE span #-}


-- | Generates an HTML __strong__ element with the given attributes and contents.
strong :: [Attribute] -> [Html lng] -> Html lng
strong = ParentNode "<strong" "</strong>"
{-# INLINE strong #-}


-- | Generates an HTML __style__ element with the given attributes and contents.
style :: [Attribute] -> [Html lng] -> Html lng
style = ParentNode "<style" "</style>"
{-# INLINE style #-}


-- | Generates an HTML __sub__ element with the given attributes and contents.
sub :: [Attribute] -> [Html lng] -> Html lng
sub = ParentNode "<sub" "</sub>"
{-# INLINE sub #-}


-- | Generates an HTML __summary__ element with the given attributes and contents.
summary :: [Attribute] -> [Html lng] -> Html lng
summary = ParentNode "<summary" "</summary>"
{-# INLINE summary #-}


-- | Generates an HTML __sup__ element with the given attributes and contents.
sup :: [Attribute] -> [Html lng] -> Html lng
sup = ParentNode "<sup" "</sup>"
{-# INLINE sup #-}


-- | Generates an HTML __table__ element with the given attributes and contents.
table :: [Attribute] -> [Html lng] -> Html lng
table = ParentNode "<table" "</table>"
{-# INLINE table #-}


-- | Generates an HTML __tbody__ element with the given attributes and contents.
tbody :: [Attribute] -> [Html lng] -> Html lng
tbody = ParentNode "<tbody" "</tbody>"
{-# INLINE tbody #-}


-- | Generates an HTML __td__ element with the given attributes and contents.
td :: [Attribute] -> [Html lng] -> Html lng
td = ParentNode "<td" "</td>"
{-# INLINE td #-}


-- | Generates an HTML __template__ element with the given attributes and contents.
template :: [Attribute] -> [Html lng] -> Html lng
template = ParentNode "<template" "</template>"
{-# INLINE template #-}


-- | Generates an HTML __textarea__ element with the given attributes and contents.
textarea :: [Attribute] -> [Html lng] -> Html lng
textarea = ParentNode "<textarea" "</textarea>"
{-# INLINE textarea #-}


-- | Generates an HTML __tfoot__ element with the given attributes and contents.
tfoot :: [Attribute] -> [Html lng] -> Html lng
tfoot = ParentNode "<tfoot" "</tfoot>"
{-# INLINE tfoot #-}


-- | Generates an HTML __th__ element with the given attributes and contents.
th :: [Attribute] -> [Html lng] -> Html lng
th = ParentNode "<th" "</th>"
{-# INLINE th #-}


-- | Generates an HTML __thead__ element with the given attributes and contents.
thead :: [Attribute] -> [Html lng] -> Html lng
thead = ParentNode "<thead" "</thead>"
{-# INLINE thead #-}


-- | Generates an HTML __time__ element with the given attributes and contents.
time :: [Attribute] -> [Html lng] -> Html lng
time = ParentNode "<time" "</time>"
{-# INLINE time #-}


-- | Generates an HTML __title__ element with the given attributes and contents.
title :: [Attribute] -> [Html lng] -> Html lng
title = ParentNode "<title" "</title>"
{-# INLINE title #-}


-- | Generates an HTML __tr__ element with the given attributes and contents.
tr :: [Attribute] -> [Html lng] -> Html lng
tr = ParentNode "<tr" "</tr>"
{-# INLINE tr #-}


-- | Generates an HTML __track__ element with the given attributes.
track :: [Attribute] -> Html lng
track = LeafNode "<track"
{-# INLINE track #-}


-- | Generates an HTML __u__ element with the given attributes and contents.
u :: [Attribute] -> [Html lng] -> Html lng
u = ParentNode "<u" "</u>"
{-# INLINE u #-}


-- | Generates an HTML __ul__ element with the given attributes and contents.
ul :: [Attribute] -> [Html lng] -> Html lng
ul = ParentNode "<ul" "</ul>"
{-# INLINE ul #-}


-- | Generates an HTML __var__ element with the given attributes and contents.
var :: [Attribute] -> [Html lng] -> Html lng
var = ParentNode "<var" "</var>"
{-# INLINE var #-}


-- | Generates an HTML __video__ element with the given attributes and contents.
video :: [Attribute] -> [Html lng] -> Html lng
video = ParentNode "<video" "</video>"
{-# INLINE video #-}


-- | Generates an HTML __wbr__ element with the given attributes.
wbr :: [Attribute] -> Html lng
wbr = LeafNode "<wbr"
{-# INLINE wbr #-}


-- TEXT


-- | Generates a monolingual HTML text node with the given contents.
--
-- See 'Html.Intl.intl' for multilingual text nodes.
text :: Builder -> Html lng
text = TextNode
{-# INLINE text #-}


-- | Generates an empty HTML text node.
--
-- Intended to be used in /if-else expressions/ to generate nothing when the given condition is met.
--
-- ==== __Example__
--
-- __Input:__
--
-- @
-- Html.nav []
--     Html.ul []
--         [ Html.li []
--             [ Html.a
--                 [ Attr.href \"\/\" ]
--                 [ Html.text \"Home\" ]
--             , Html.a
--                 [ Attr.href \"\/about\" ]
--                 [ Html.text \"About\" ]
--             , if isLoggedIn then
--                 Html.a
--                     [ Attr.href \"\/profile\" ]
--                     [ Html.text \"Profile\" ]
--               else
--                 Html.empty
--             , if isLoggedIn then
--                 Html.a
--                     [ Attr.href \"\/log-out\" ]
--                     [ Html.text \"Log out\" ]
--               else
--                 Html.a
--                     [ Attr.href \"\/log-in\" ]
--                     [ Html.text \"Log in\" ]
--             ]
--         ]
--     ]
-- @
--
-- __Output__ (when /isLoggedIn/ is /True/)__:__
--
-- @
-- \<nav\>
--     \<ul\>
--         \<li\>\<a href=\"\/\"\>Home\<\/a\>\<\/li\>
--         \<li\>\<a href=\"\/about\"\>About\<\/a\>\<\/li\>
--         \<li\>\<a href=\"\/profile\"\>Profile\<\/a\>\<\/li\>
--         \<li\>\<a href=\"\/log-out\"\>Log out\<\/a\>\<\/li\>
--     \<\/ul\>
-- \<\/nav\>
-- @
--
-- __Output__ (when /isLoggedIn/ is /False/)__:__
--
-- @
-- \<nav\>
--     \<ul\>
--         \<li\>\<a href=\"\/\"\>Home\<\/a\>\<\/li\>
--         \<li\>\<a href=\"\/about\"\>About\<\/a\>\<\/li\>
--         \<li\>\<a href=\"\/log-in\"\>Log in\<\/a\>\<\/li\>
--     \<\/ul\>
-- \<\/nav\>
-- @
empty :: Html lng
empty = TextNode ""
{-# INLINE empty #-}
