{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The "Html" module provides a set of data types, typeclasses and functions for generating HTML elements.
--
-- These elements along with their attributes in "Html.Attributes" can be used to generate HTML documents directly in Haskell, without
-- relying on templating engines or other techniques that can be error-prone and difficult to maintain.
--
-- __Example:__
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
-- __Result:__
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
-- import Html (Html)
--
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
    , ToBuilder(..)

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
    ) where


import Prelude ((.), Bool(..), Show(..))

import Data.Foldable (foldr)
import Data.Monoid ((<>), mempty)
import Data.Text.Lazy (unpack)
import Data.Text.Lazy.Builder (Builder, singleton, toLazyText)

import qualified Prelude


--data Intl = Intl
    --{ de :: Builder
    --, en :: Builder
    --}


--instance ToHtml Intl where
    --toHtml = IntlNode


class ToHtml a where
    toHtml :: a -> Html a


intl :: ToHtml a => a -> Html a
intl = IntlNode


translate :: ToHtml a => (a -> Builder) -> Html a -> Html a
translate lang html = case html of
    IntlNode   intl                                -> TextNode   text'                                where text'     = lang intl
    ParentNode startTag endTag attributes children -> ParentNode startTag endTag attributes children' where children' = map children
    RootNode   startTag                   children -> RootNode   startTag                   children' where children' = map children
    node                                           -> node
  where
    map = Prelude.map (translate lang)


-- TYPES


-- | Represents an HTML element.
--
-- This data type can be used to generate HTML elements programmatically with the functions provided in this module.
data Html a where
    IntlNode   :: ToHtml a => a       ->                                       Html a -- ^ Constructs a multilingual HTML text node.
    TextNode   ::             Builder ->                                       Html a -- ^ Constructs a monolingual HTML text node.
    LeafNode   ::             Builder ->            [Attribute] ->             Html a -- ^ Constructs an HTML leaf node.
    ParentNode ::             Builder -> Builder -> [Attribute] -> [Html a] -> Html a -- ^ Constructs an HTML parent node.
    RootNode   ::             Builder ->                           [Html a] -> Html a -- ^ Constructs an HTML root node.
    {-
    = IntlNode   Intl                               -- ^ Constructs a multilingual HTML text node.
    | TextNode   Builder                            -- ^ Constructs a monolingual HTML text node.
    | LeafNode   Builder         [Attribute]        -- ^ Constructs an HTML leaf node.
    | ParentNode Builder Builder [Attribute] [Html a] -- ^ Constructs an HTML parent node.
    | RootNode   Builder                     [Html a] -- ^ Constructs an HTML root node.
    -}


instance Show (Html a) where
    show = unpack . toLazyText . toBuilder
    {-# INLINE show #-}


instance ToBuilder (Html a) where
    toBuilder html = case html of
        IntlNode   intl                                -> ""
        TextNode   text                                -> text
        LeafNode   startTag        []                  -> startTag <>                         singleton '>'
        LeafNode   startTag        attributes          -> startTag <> toBuilder attributes <> singleton '>'
        ParentNode startTag endTag []         []       -> startTag <>                         singleton '>' <>                       endTag
        ParentNode startTag endTag attributes []       -> startTag <> toBuilder attributes <> singleton '>' <>                       endTag
        ParentNode startTag endTag []         children -> startTag <>                         singleton '>' <> toBuilder children <> endTag
        ParentNode startTag endTag attributes children -> startTag <> toBuilder attributes <> singleton '>' <> toBuilder children <> endTag
        RootNode   startTag                   []       -> startTag
        RootNode   startTag                   children -> startTag <>                                          toBuilder children


instance {-# OVERLAPPING #-} Show [Html a] where
    show = unpack . toLazyText . toBuilder
    {-# INLINE show #-}


instance ToBuilder [Html a] where
    toBuilder = foldr ((<>) . toBuilder) mempty
    {-# INLINE toBuilder #-}


-- | Represents an HTML attribute.
--
-- This data type can be used to generate HTML attributes programmatically with the functions provided in the "Html.Attributes" module.
data Attribute
    = BoolAttribute Builder Bool    -- ^ Constructs a boolean HTML attribute.
    | TextAttribute Builder Builder -- ^ Constructs a textual HTML attribute.


instance Show Attribute where
    show = unpack . toLazyText . toBuilder
    {-# INLINE show #-}


instance ToBuilder Attribute where
    toBuilder attribute = case attribute of
        BoolAttribute _   False -> mempty
        BoolAttribute key True  -> key
        TextAttribute _   ""    -> mempty
        TextAttribute key value -> key <> value <> singleton '"'


instance {-# OVERLAPPING #-} Show [Attribute] where
    show = unpack . toLazyText . toBuilder
    {-# INLINE show #-}


instance ToBuilder [Attribute] where
    toBuilder = foldr ((<>) . toBuilder) mempty
    {-# INLINE toBuilder #-}


-- CLASSES


-- | This interface enables a value to be converted to a 'Builder'.
class ToBuilder a where
    -- | Converts a value to a 'Builder'.
    toBuilder :: a -> Builder


-- DECLARATIONS


-- | Generates an HTML @\<!DOCTYPE\>@ declaration with the given contents.
--
-- The @\<!DOCTYPE\>@ declaration defines the document type and version of the HTML being used. It ensures proper rendering by browsers and
-- sets the standard for the document's structure.
--
-- ==== __Example__
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
-- __Result:__
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
doctype :: [Html a] -> Html a
doctype = RootNode "<!DOCTYPE html>\n"
{-# INLINE doctype #-}


-- ELEMENTS


-- | Generates an HTML @\<a\>@ element with the given attributes and contents.
--
-- The @\<a\>@ element, or anchor element, is used to create hyperlinks that link to other web pages or resources. It defines the clickable
-- content that, when clicked, navigates to the specified URL.
--
-- ==== __Example__
--
-- @
-- Html.nav []
--     [ Html.ul []
--         [ Html.li []
--             [ Html.a
--                 [ Attr.href \"#heroes\" ]
--                 [ Html.text \"Valiant Adventurers\" ]
--             , Html.a
--                 [ Attr.href \"#monsters\" ]
--                 [ Html.text \"Unspeakable Horrors\" ]
--             , Html.a
--                 [ Attr.href \"#trinkets\" ]
--                 [ Html.text \"Cursed Relics\" ]
--             , Html.a
--                 [ Attr.href \"#strategies\" ]
--                 [ Html.text \"Descent Tactics\" ]
--             ]
--         ]
--     ]
-- @
--
-- __Result:__
--
-- @
-- \<nav\>
--     \<ul\>
--         \<li\>\<a href=\"#heroes\"\>Valiant Adventurers\<\/a\>\<\/li\>
--         \<li\>\<a href=\"#monsters\"\>Unspeakable Horrors\<\/a\>\<\/li\>
--         \<li\>\<a href=\"#trinkets\"\>Cursed Relics\<\/a\>\<\/li\>
--         \<li\>\<a href=\"#strategies\"\>Descent Tactics\<\/a\>\<\/li\>
--     \<\/ul\>
-- \<\/nav\>
-- @
a :: [Attribute] -> [Html a] -> Html a
a = ParentNode "<a" "</a>"
{-# INLINE a #-}


-- | Generates an HTML @\<abbr\>@ element with the given attributes and contents.
--
-- The @\<abbr\>@ element is used to mark up an abbreviation or acronym in the text. It can include a title attribute to provide the full or
-- expanded form of the abbreviation when hovered over.
--
-- ==== __Example__
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
-- __Result:__
--
-- @
-- \<p\>The \<abbr title=\"Hypertext Markup Language\"\>HTML\<\/abbr\> standard revolutionized web development.\<\/p\>
-- @
abbr :: [Attribute] -> [Html a] -> Html a
abbr = ParentNode "<abbr" "</abbr>"
{-# INLINE abbr #-}


-- | Generates an HTML @\<address\>@ element with the given attributes and contents.
--
-- The @\<address\>@ element is used to provide contact information or author details for the nearest @\<article\>@ or @\<body\>@ ancestor.
-- It typically includes information such as names, addresses, emails, or phone numbers.
--
-- ==== __Example__
--
-- @
-- Html.address []
--     [ Html.p []
--         [ Html.text \"Contact the Hegelian Society:\" ]
--     , Html.p []
--         [ Html.strong []
--             [ Html.text \"Hegel Archives, Jena\" ]
--         ]
--     , Html.p []
--         [ Html.text \"Email: \"
--         , Html.a
--             [ Attr.href \"mailto:info\@hegelarchives.org\" ]
--             [ Html.text \"info\@hegelarchives.org\" ]
--         ]
--     ]
-- @
--
-- __Result:__
--
-- @
-- \<address\>
--     \<p\>Contact the Hegelian Society:\<\/p\>
--     \<p\>\<strong\>Hegel Archives, Jena\<\/strong\>\<\/p\>
--     \<p\>Email: \<a href=\"mailto:info\@hegelarchives.org\"\>info\@hegelarchives.org\<\/a\>\<\/p\>
-- \<\/address\>
-- @
address :: [Attribute] -> [Html a] -> Html a
address = ParentNode "<address" "</address>"
{-# INLINE address #-}


-- | Generates an HTML @\<area\>@ element with the given attributes.
--
-- The @\<area\>@ element is used within a `<map>` element to define clickable areas within an image map. Each `<area>` defines a clickable
-- region that links to a specific URL or performs an action when clicked.
--
-- ==== __Example__
--
-- @
-- Html.map
--     [ Attr.name \"downtown\" ]
--     [ Html.area
--         [ Attr.alt    \"Library\"
--         , Attr.coords \"52,36,160,240\"
--         , Attr.shape  \"rect\"
--         ]
--     ]
-- @
--
-- __Result:__
--
-- @
-- \<map name=\"downtown\"\>
--     \<area alt=\"Library\" coords=\"52,36,160,240\" shape=\"rect\"\>
-- \<\/map\>
-- @
area :: [Attribute] -> Html a
area = LeafNode "<area"
{-# INLINE area #-}


-- | Generates an HTML __article__ element with the given attributes and contents.
--
-- The `<article>` element represents a self-contained composition within a document. It encapsulates content that can be distributed or
-- reused independently, such as news articles, blog posts, or forum entries.
--
-- ==== __Example__
--
-- @
-- Html.article []
--     [ Html.h2 []
--         [ Html.text \"Franz Kafka's Novels\" ]
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
-- __Result:__
--
-- @
-- \<article\>
--     \<h2\>Franz Kafka's Novels\<\/h2\>
--     \<ul\>
--         \<li\>The Man Who Disappeared\<\/li\>
--         \<li\>The Trial\<\/li\>
--         \<li\>The Castle\<\/li\>
--     \<\/ul\>
-- \<\/article\>
-- @
article :: [Attribute] -> [Html a] -> Html a
article = ParentNode "<article" "</article>"
{-# INLINE article #-}


-- | Generates an HTML __aside__ element with the given attributes and contents.
--
-- ==== __Example__
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
-- __Result:__
--
-- @
-- \<aside\>
--     \<h4\>House of the Dragon\<\/h4\>
--     \<p\>House of the Dragon is a prequel to Game of Thrones.\<\/p\>
-- \<\/aside\>
-- @
aside :: [Attribute] -> [Html a] -> Html a
aside = ParentNode "<aside" "</aside>"
{-# INLINE aside #-}


-- | Generates an HTML __audio__ element with the given attributes and contents.
--
-- ==== __Example__
--
-- @
-- Html.audio
--     [ Attr.controls True ]
--     [ Html.source
--         [ Attr.src   \"bossfight-warp.mp3\"
--         , Attr.type_ \"audio/mpeg\"
--         ]
--     , Html.text \"Your browser does not support the audio tag.\"
--     ]
-- @
--
-- __Result:__
--
-- @
-- \<audio controls\>
--     \<source src=\"bossfight-warp.mp3\" type=\"audio/mpeg\"\>
--     Your browser does not support the audio tag.
-- \<\/audio\>
-- @
audio :: [Attribute] -> [Html a] -> Html a
audio = ParentNode "<audio" "</audio>"
{-# INLINE audio #-}


-- | Generates an HTML __b__ element with the given attributes and contents.
--
-- ==== __Example__
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
-- __Result:__
--
-- > <p>Gary Gygax hands Fry his <b>+1 mace</b>.</p>
b :: [Attribute] -> [Html a] -> Html a
b = ParentNode "<b" "</b>"
{-# INLINE b #-}


-- | Generates an HTML __base__ element with the given attributes.
--
-- ==== __Example__
--
-- @
-- Html.head []
--     [ Html.base
--         [ Attr.href \"https:\/\/news.ycombinator.com\" ]
--     ]
-- @
--
-- __Result:__
--
-- @
-- \<head\>
--     \<base href=\"https:\/\/news.ycombinator.com\"\>
-- \<\/head\>
-- @
base :: [Attribute] -> Html a
base = LeafNode "<base"
{-# INLINE base #-}


-- | Generates an HTML __bdi__ element with the given attributes and contents.
--
-- ==== __Example__
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
-- __Result:__
--
-- @
-- \<ul\>
--     \<li\>Character \<bdi\>فاصوليا\<\/bdi\>: Human\<\/li\>
--     \<li\>Character \<bdi\>Lucy\<\/bdi\>: Demon\<\/li\>
--     \<li\>Character \<bdi\>Elfo\<\/bdi\>: Elf\<\/li\>
-- \<\/ul\>
-- @
bdi :: [Attribute] -> [Html a] -> Html a
bdi = ParentNode "<bdi" "</bdi>"
{-# INLINE bdi #-}


-- | Generates an HTML __bdo__ element with the given attributes and contents.
--
-- ==== __Example__
--
-- @
-- Html.bdo
--     [ Attr.dir "rtl" ]
--     [ Html.text "The sun rises in the east and sets in the west." ]
-- @
--
-- __Result:__
--
-- > <bdo dir="rtl">The sun rises in the east and sets in the west.</bdo>
bdo :: [Attribute] -> [Html a] -> Html a
bdo = ParentNode "<bdo" "</bdo>"
{-# INLINE bdo #-}


-- | Generates an HTML __blockquote__ element with the given attributes and contents.
--
-- ==== __Example__
--
-- @
-- Html.blockquote []
--     [ Html.p []
--         [ Html.text \"When life gives you lemons, make lemonade.\" ]
--     ]
-- @
--
-- __Result:__
--
-- @
-- \<blockquote\>
--     \<p\>When life gives you lemons, make lemonade.\<\/p\>
-- \<\/blockquote\>
-- @
blockquote :: [Attribute] -> [Html a] -> Html a
blockquote = ParentNode "<blockquote" "</blockquote>"
{-# INLINE blockquote #-}


-- | Generates an HTML __body__ element with the given attributes and contents.
--
-- ==== __Example__
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
-- __Result:__
--
-- @
-- \<body\>
--     \<h1\>An Introduction to Elm\<\/h1\>
--     \<p\>Elm is a functional language for front-end web development.\<\/p\>
-- \<\/body\>
-- @
body :: [Attribute] -> [Html a] -> Html a
body = ParentNode "<body" "</body>"
{-# INLINE body #-}


-- | Generates an HTML __br__ element with the given attributes.
--
-- ==== __Example__
--
-- @
-- Html.p []
--     [ Html.text \"That which we call a rose\"
--     , Html.br []
--     , Html.text \"By any other name would smell as sweet.\"
--     ]
-- @
--
-- __Result:__
--
-- > <p>That which we call a rose<br>By any other name would smell as sweet.</p>
br :: [Attribute] -> Html a
br = LeafNode "<br"
{-# INLINE br #-}


-- | Generates an HTML __button__ element with the given attributes and contents.
--
-- ==== __Example__
--
-- @
-- Html.button
--     [ Attr.type_ \"submit\" ]
--     [ Html.text \"Log in\" ]
-- @
--
-- __Result:__
--
-- > <button type="submit">Log in</button>
button :: [Attribute] -> [Html a] -> Html a
button = ParentNode "<button" "</button>"
{-# INLINE button #-}


-- | Generates an HTML __canvas__ element with the given attributes and contents.
--
-- ==== __Example__
--
-- @
-- Html.canvas
--     [ Attr.height \"500\"
--     , Attr.width  \"500\"
--     ]
--     [ Html.text \"Your browser does not support the canvas tag.\" ]
-- @
--
-- __Result:__
--
-- @
-- \<canvas height=\"500\" width=\"500\"\>
--     Your browser does not support the canvas tag.
-- \<\/canvas\>
-- @
canvas :: [Attribute] -> [Html a] -> Html a
canvas = ParentNode "<canvas" "</canvas>"
{-# INLINE canvas #-}


-- | Generates an HTML __caption__ element with the given attributes and contents.
--
-- ==== __Example__
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
-- __Result:__
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
caption :: [Attribute] -> [Html a] -> Html a
caption = ParentNode "<caption" "</caption>"
{-# INLINE caption #-}


-- | Generates an HTML __cite__ element with the given attributes and contents.
--
-- ==== __Example__
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
-- __Result:__
--
-- > <p>My favorite movie is <cite>Psycho</cite> by Alfred Hitchcock.</p>
cite :: [Attribute] -> [Html a] -> Html a
cite = ParentNode "<cite" "</cite>"
{-# INLINE cite #-}


-- | Generates an HTML __code__ element with the given attributes and contents.
--
-- ==== __Example__
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
-- __Result:__
--
-- > <p>The <code>map</code> function is a higher-order function.</p>
code :: [Attribute] -> [Html a] -> Html a
code = ParentNode "<code" "</code>"
{-# INLINE code #-}


-- | Generates an HTML __col__ element with the given attributes.
--
-- ==== __Example__
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
-- __Result:__
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
col :: [Attribute] -> Html a
col = LeafNode "<col"
{-# INLINE col #-}


-- | Generates an HTML __colgroup__ element with the given attributes and contents.
--
-- ==== __Example__
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
-- __Result:__
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
colgroup :: [Attribute] -> [Html a] -> Html a
colgroup = ParentNode "<colgroup" "</colgroup>"
{-# INLINE colgroup #-}


-- | Generates an HTML __data__ element with the given attributes and contents.
--
-- ==== __Example__
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
-- __Result:__
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
data_ :: [Attribute] -> [Html a] -> Html a
data_ = ParentNode "<data" "</data>"
{-# INLINE data_ #-}


-- | Generates an HTML __datalist__ element with the given attributes and contents.
--
-- ==== __Example__
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
-- __Result:__
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
datalist :: [Attribute] -> [Html a] -> Html a
datalist = ParentNode "<datalist" "</datalist>"
{-# INLINE datalist #-}


-- | Generates an HTML __dd__ element with the given attributes and contents.
--
-- ==== __Example__
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
-- __Result:__
--
-- @
-- \<dl\>
--     \<dt\>Chicken adobo\<\/dt\>
--     \<dd\>Chicken marinated in vinegar and soy sauce\<\/dd\>
--     \<dt\>Pancit\<\/dt\>
--     \<dd\>Rice noodles with pork\<\/dd\>
-- \<\/dl\>
-- @
dd :: [Attribute] -> [Html a] -> Html a
dd = ParentNode "<dd" "</dd>"
{-# INLINE dd #-}


-- | Generates an HTML __del__ element with the given attributes and contents.
--
-- ==== __Example__
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
-- __Result:__
--
-- > <p>Appointments are available on <del>Tuesdays</del>, Wednesdays and Fridays.</p>
del :: [Attribute] -> [Html a] -> Html a
del = ParentNode "<del" "</del>"
{-# INLINE del #-}


-- | Generates an HTML __details__ element with the given attributes and contents.
--
-- ==== __Example__
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
-- __Result:__
--
-- @
-- \<details\>
--     \<summary\>Phrasal Verbs\<\/summary\>
--     \<p\>A phrasal verb combines a normal verb with either a preposition or an adverb.\<\/p\>
-- \<\/details\>
-- @
details :: [Attribute] -> [Html a] -> Html a
details = ParentNode "<details" "</details>"
{-# INLINE details #-}


-- | Generates an HTML __dfn__ element with the given attributes and contents.
--
-- ==== __Example__
--
-- @
-- Html.p []
--     [ Html.dfn []
--         [ Html.text \"ChatGPT\" ]
--     , Html.text \" is an OpenAI language model.\"
--     ]
-- @
--
-- __Result:__
--
-- > <p><dfn>ChatGPT</dfn> is an OpenAI language model.</p>
dfn :: [Attribute] -> [Html a] -> Html a
dfn = ParentNode "<dfn" "</dfn>"
{-# INLINE dfn #-}


-- | Generates an HTML __dialog__ element with the given attributes and contents.
--
-- ==== __Example__
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
-- __Result:__
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
dialog :: [Attribute] -> [Html a] -> Html a
dialog = ParentNode "<dialog" "</dialog>"
{-# INLINE dialog #-}


-- | Generates an HTML __div__ element with the given attributes and contents.
div :: [Attribute] -> [Html a] -> Html a
div = ParentNode "<div" "</div>"
{-# INLINE div #-}


-- | Generates an HTML __dl__ element with the given attributes and contents.
dl :: [Attribute] -> [Html a] -> Html a
dl = ParentNode "<dl" "</dl>"
{-# INLINE dl #-}


-- | Generates an HTML __dt__ element with the given attributes and contents.
dt :: [Attribute] -> [Html a] -> Html a
dt = ParentNode "<dt" "</dt>"
{-# INLINE dt #-}


-- | Generates an HTML __em__ element with the given attributes and contents.
em :: [Attribute] -> [Html a] -> Html a
em = ParentNode "<em" "</em>"
{-# INLINE em #-}


-- | Generates an HTML __embed__ element with the given attributes.
embed :: [Attribute] -> Html a
embed = LeafNode "<embed"
{-# INLINE embed #-}


-- | Generates an HTML __fieldset__ element with the given attributes and contents.
fieldset :: [Attribute] -> [Html a] -> Html a
fieldset = ParentNode "<fieldset" "</fieldset>"
{-# INLINE fieldset #-}


-- | Generates an HTML __figcaption__ element with the given attributes and contents.
figcaption :: [Attribute] -> [Html a] -> Html a
figcaption = ParentNode "<figcaption" "</figcaption>"
{-# INLINE figcaption #-}


-- | Generates an HTML __figure__ element with the given attributes and contents.
figure :: [Attribute] -> [Html a] -> Html a
figure = ParentNode "<figure" "</figure>"
{-# INLINE figure #-}


-- | Generates an HTML __footer__ element with the given attributes and contents.
footer :: [Attribute] -> [Html a] -> Html a
footer = ParentNode "<footer" "</footer>"
{-# INLINE footer #-}


-- | Generates an HTML __form__ element with the given attributes and contents.
form :: [Attribute] -> [Html a] -> Html a
form = ParentNode "<form" "</form>"
{-# INLINE form #-}


-- | Generates an HTML __h1__ element with the given attributes and contents.
h1 :: [Attribute] -> [Html a] -> Html a
h1 = ParentNode "<h1" "</h1>"
{-# INLINE h1 #-}


-- | Generates an HTML __h2__ element with the given attributes and contents.
h2 :: [Attribute] -> [Html a] -> Html a
h2 = ParentNode "<h2" "</h2>"
{-# INLINE h2 #-}


-- | Generates an HTML __h3__ element with the given attributes and contents.
h3 :: [Attribute] -> [Html a] -> Html a
h3 = ParentNode "<h3" "</h3>"
{-# INLINE h3 #-}


-- | Generates an HTML __h4__ element with the given attributes and contents.
h4 :: [Attribute] -> [Html a] -> Html a
h4 = ParentNode "<h4" "</h4>"
{-# INLINE h4 #-}


-- | Generates an HTML __h5__ element with the given attributes and contents.
h5 :: [Attribute] -> [Html a] -> Html a
h5 = ParentNode "<h5" "</h5>"
{-# INLINE h5 #-}


-- | Generates an HTML __h6__ element with the given attributes and contents.
h6 :: [Attribute] -> [Html a] -> Html a
h6 = ParentNode "<h6" "</h6>"
{-# INLINE h6 #-}


-- | Generates an HTML __head__ element with the given attributes and contents.
head :: [Attribute] -> [Html a] -> Html a
head = ParentNode "<head" "</head>"
{-# INLINE head #-}


-- | Generates an HTML __header__ element with the given attributes and contents.
header :: [Attribute] -> [Html a] -> Html a
header = ParentNode "<header" "</header>"
{-# INLINE header #-}


-- | Generates an HTML __hgroup__ element with the given attributes and contents.
hgroup :: [Attribute] -> [Html a] -> Html a
hgroup = ParentNode "<hgroup" "</hgroup>"
{-# INLINE hgroup #-}


-- | Generates an HTML __hr__ element with the given attributes.
hr :: [Attribute] -> Html a
hr = LeafNode "<hr"
{-# INLINE hr #-}


-- | Generates an HTML __html__ element with the given attributes and contents.
html :: [Attribute] -> [Html a] -> Html a
html = ParentNode "<html" "</html>"
{-# INLINE html #-}


-- | Generates an HTML __i__ element with the given attributes and contents.
i :: [Attribute] -> [Html a] -> Html a
i = ParentNode "<i" "</i>"
{-# INLINE i #-}


-- | Generates an HTML __iframe__ element with the given attributes and contents.
iframe :: [Attribute] -> [Html a] -> Html a
iframe = ParentNode "<iframe" "</iframe>"
{-# INLINE iframe #-}


-- | Generates an HTML __img__ element with the given attributes.
img :: [Attribute] -> Html a
img = LeafNode "<img"
{-# INLINE img #-}


-- | Generates an HTML __input__ element with the given attributes.
input :: [Attribute] -> Html a
input = LeafNode "<input"
{-# INLINE input #-}


-- | Generates an HTML __ins__ element with the given attributes and contents.
ins :: [Attribute] -> [Html a] -> Html a
ins = ParentNode "<ins" "</ins>"
{-# INLINE ins #-}


-- | Generates an HTML __kbd__ element with the given attributes and contents.
kbd :: [Attribute] -> [Html a] -> Html a
kbd = ParentNode "<kbd" "</kbd>"
{-# INLINE kbd #-}


-- | Generates an HTML __label__ element with the given attributes and contents.
label :: [Attribute] -> [Html a] -> Html a
label = ParentNode "<label" "</label>"
{-# INLINE label #-}


-- | Generates an HTML __legend__ element with the given attributes and contents.
legend :: [Attribute] -> [Html a] -> Html a
legend = ParentNode "<legend" "</legend>"
{-# INLINE legend #-}


-- | Generates an HTML __li__ element with the given attributes and contents.
li :: [Attribute] -> [Html a] -> Html a
li = ParentNode "<li" "</li>"
{-# INLINE li #-}


-- | Generates an HTML __link__ element with the given attributes.
link :: [Attribute] -> Html a
link = LeafNode "<link"
{-# INLINE link #-}


-- | Generates an HTML __main__ element with the given attributes and contents.
main :: [Attribute] -> [Html a] -> Html a
main = ParentNode "<main" "</main>"
{-# INLINE main #-}


-- | Generates an HTML __map__ element with the given attributes and contents.
map :: [Attribute] -> [Html a] -> Html a
map = ParentNode "<map" "</map>"
{-# INLINE map #-}


-- | Generates an HTML __mark__ element with the given attributes and contents.
mark :: [Attribute] -> [Html a] -> Html a
mark = ParentNode "<mark" "</mark>"
{-# INLINE mark #-}


-- | Generates an HTML __menu__ element with the given attributes and contents.
menu :: [Attribute] -> [Html a] -> Html a
menu = ParentNode "<menu" "</menu>"
{-# INLINE menu #-}


-- | Generates an HTML __meta__ element with the given attributes.
meta :: [Attribute] -> Html a
meta = LeafNode "<meta"
{-# INLINE meta #-}


-- | Generates an HTML __meter__ element with the given attributes and contents.
meter :: [Attribute] -> [Html a] -> Html a
meter = ParentNode "<meter" "</meter>"
{-# INLINE meter #-}


-- | Generates an HTML __nav__ element with the given attributes and contents.
nav :: [Attribute] -> [Html a] -> Html a
nav = ParentNode "<nav" "</nav>"
{-# INLINE nav #-}


-- | Generates an HTML __noscript__ element with the given attributes and contents.
noscript :: [Attribute] -> [Html a] -> Html a
noscript = ParentNode "<noscript" "</noscript>"
{-# INLINE noscript #-}


-- | Generates an HTML __object__ element with the given attributes and contents.
object :: [Attribute] -> [Html a] -> Html a
object = ParentNode "<object" "</object>"
{-# INLINE object #-}


-- | Generates an HTML __ol__ element with the given attributes and contents.
ol :: [Attribute] -> [Html a] -> Html a
ol = ParentNode "<ol" "</ol>"
{-# INLINE ol #-}


-- | Generates an HTML __optgroup__ element with the given attributes and contents.
optgroup :: [Attribute] -> [Html a] -> Html a
optgroup = ParentNode "<optgroup" "</optgroup>"
{-# INLINE optgroup #-}


-- | Generates an HTML __option__ element with the given attributes and contents.
option :: [Attribute] -> [Html a] -> Html a
option = ParentNode "<option" "</option>"
{-# INLINE option #-}


-- | Generates an HTML __output__ element with the given attributes and contents.
output :: [Attribute] -> [Html a] -> Html a
output = ParentNode "<output" "</output>"
{-# INLINE output #-}


-- | Generates an HTML __p__ element with the given attributes and contents.
p :: [Attribute] -> [Html a] -> Html a
p = ParentNode "<p" "</p>"
{-# INLINE p #-}


-- | Generates an HTML __picture__ element with the given attributes and contents.
picture :: [Attribute] -> [Html a] -> Html a
picture = ParentNode "<picture" "</picture>"
{-# INLINE picture #-}


-- | Generates an HTML __pre__ element with the given attributes and contents.
pre :: [Attribute] -> [Html a] -> Html a
pre = ParentNode "<pre" "</pre>"
{-# INLINE pre #-}


-- | Generates an HTML __progress__ element with the given attributes and contents.
progress :: [Attribute] -> [Html a] -> Html a
progress = ParentNode "<progress" "</progress>"
{-# INLINE progress #-}


-- | Generates an HTML __q__ element with the given attributes and contents.
q :: [Attribute] -> [Html a] -> Html a
q = ParentNode "<q" "</q>"
{-# INLINE q #-}


-- | Generates an HTML __rp__ element with the given attributes and contents.
rp :: [Attribute] -> [Html a] -> Html a
rp = ParentNode "<rp" "</rp>"
{-# INLINE rp #-}


-- | Generates an HTML __rt__ element with the given attributes and contents.
rt :: [Attribute] -> [Html a] -> Html a
rt = ParentNode "<rt" "</rt>"
{-# INLINE rt #-}


-- | Generates an HTML __ruby__ element with the given attributes and contents.
ruby :: [Attribute] -> [Html a] -> Html a
ruby = ParentNode "<ruby" "</ruby>"
{-# INLINE ruby #-}


-- | Generates an HTML __s__ element with the given attributes and contents.
s :: [Attribute] -> [Html a] -> Html a
s = ParentNode "<s" "</s>"
{-# INLINE s #-}


-- | Generates an HTML __samp__ element with the given attributes and contents.
samp :: [Attribute] -> [Html a] -> Html a
samp = ParentNode "<samp" "</samp>"
{-# INLINE samp #-}


-- | Generates an HTML __script__ element with the given attributes and contents.
script :: [Attribute] -> [Html a] -> Html a
script = ParentNode "<script" "</script>"
{-# INLINE script #-}


-- | Generates an HTML __section__ element with the given attributes and contents.
section :: [Attribute] -> [Html a] -> Html a
section = ParentNode "<section" "</section>"
{-# INLINE section #-}


-- | Generates an HTML __select__ element with the given attributes and contents.
select :: [Attribute] -> [Html a] -> Html a
select = ParentNode "<select" "</select>"
{-# INLINE select #-}


-- | Generates an HTML __slot__ element with the given attributes and contents.
slot :: [Attribute] -> [Html a] -> Html a
slot = ParentNode "<slot" "</slot>"
{-# INLINE slot #-}


-- | Generates an HTML __small__ element with the given attributes and contents.
small :: [Attribute] -> [Html a] -> Html a
small = ParentNode "<small" "</small>"
{-# INLINE small #-}


-- | Generates an HTML __source__ element with the given attributes.
source :: [Attribute] -> Html a
source = LeafNode "<source"
{-# INLINE source #-}


-- | Generates an HTML __span__ element with the given attributes and contents.
span :: [Attribute] -> [Html a] -> Html a
span = ParentNode "<span" "</span>"
{-# INLINE span #-}


-- | Generates an HTML __strong__ element with the given attributes and contents.
strong :: [Attribute] -> [Html a] -> Html a
strong = ParentNode "<strong" "</strong>"
{-# INLINE strong #-}


-- | Generates an HTML __style__ element with the given attributes and contents.
style :: [Attribute] -> [Html a] -> Html a
style = ParentNode "<style" "</style>"
{-# INLINE style #-}


-- | Generates an HTML __sub__ element with the given attributes and contents.
sub :: [Attribute] -> [Html a] -> Html a
sub = ParentNode "<sub" "</sub>"
{-# INLINE sub #-}


-- | Generates an HTML __summary__ element with the given attributes and contents.
summary :: [Attribute] -> [Html a] -> Html a
summary = ParentNode "<summary" "</summary>"
{-# INLINE summary #-}


-- | Generates an HTML __sup__ element with the given attributes and contents.
sup :: [Attribute] -> [Html a] -> Html a
sup = ParentNode "<sup" "</sup>"
{-# INLINE sup #-}


-- | Generates an HTML __table__ element with the given attributes and contents.
table :: [Attribute] -> [Html a] -> Html a
table = ParentNode "<table" "</table>"
{-# INLINE table #-}


-- | Generates an HTML __tbody__ element with the given attributes and contents.
tbody :: [Attribute] -> [Html a] -> Html a
tbody = ParentNode "<tbody" "</tbody>"
{-# INLINE tbody #-}


-- | Generates an HTML __td__ element with the given attributes and contents.
td :: [Attribute] -> [Html a] -> Html a
td = ParentNode "<td" "</td>"
{-# INLINE td #-}


-- | Generates an HTML __template__ element with the given attributes and contents.
template :: [Attribute] -> [Html a] -> Html a
template = ParentNode "<template" "</template>"
{-# INLINE template #-}


-- | Generates an HTML __textarea__ element with the given attributes and contents.
textarea :: [Attribute] -> [Html a] -> Html a
textarea = ParentNode "<textarea" "</textarea>"
{-# INLINE textarea #-}


-- | Generates an HTML __tfoot__ element with the given attributes and contents.
tfoot :: [Attribute] -> [Html a] -> Html a
tfoot = ParentNode "<tfoot" "</tfoot>"
{-# INLINE tfoot #-}


-- | Generates an HTML __th__ element with the given attributes and contents.
th :: [Attribute] -> [Html a] -> Html a
th = ParentNode "<th" "</th>"
{-# INLINE th #-}


-- | Generates an HTML __thead__ element with the given attributes and contents.
thead :: [Attribute] -> [Html a] -> Html a
thead = ParentNode "<thead" "</thead>"
{-# INLINE thead #-}


-- | Generates an HTML __time__ element with the given attributes and contents.
time :: [Attribute] -> [Html a] -> Html a
time = ParentNode "<time" "</time>"
{-# INLINE time #-}


-- | Generates an HTML __title__ element with the given attributes and contents.
title :: [Attribute] -> [Html a] -> Html a
title = ParentNode "<title" "</title>"
{-# INLINE title #-}


-- | Generates an HTML __tr__ element with the given attributes and contents.
tr :: [Attribute] -> [Html a] -> Html a
tr = ParentNode "<tr" "</tr>"
{-# INLINE tr #-}


-- | Generates an HTML __track__ element with the given attributes.
track :: [Attribute] -> Html a
track = LeafNode "<track"
{-# INLINE track #-}


-- | Generates an HTML __u__ element with the given attributes and contents.
u :: [Attribute] -> [Html a] -> Html a
u = ParentNode "<u" "</u>"
{-# INLINE u #-}


-- | Generates an HTML __ul__ element with the given attributes and contents.
ul :: [Attribute] -> [Html a] -> Html a
ul = ParentNode "<ul" "</ul>"
{-# INLINE ul #-}


-- | Generates an HTML __var__ element with the given attributes and contents.
var :: [Attribute] -> [Html a] -> Html a
var = ParentNode "<var" "</var>"
{-# INLINE var #-}


-- | Generates an HTML __video__ element with the given attributes and contents.
video :: [Attribute] -> [Html a] -> Html a
video = ParentNode "<video" "</video>"
{-# INLINE video #-}


-- | Generates an HTML __wbr__ element with the given attributes.
wbr :: [Attribute] -> Html a
wbr = LeafNode "<wbr"
{-# INLINE wbr #-}


-- TEXT


-- | Generates an HTML text node with the given contents.
text :: Builder -> Html a
text = TextNode
{-# INLINE text #-}
