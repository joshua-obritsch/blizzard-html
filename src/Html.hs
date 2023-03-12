{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- | This module defines a set of types and functions responsible for generating and using HTML elements; and aims to be
-- compliant with the HTML Living Standard.
--
-- The example results are formatted in this document for the purpose of readability but are minified in practice.
--
-- All examples assume the following imports:
--
-- @
-- import Html (Attribute, Html)
--
-- import qualified Html as Html
-- import qualified Html.Attributes as Attr
-- @
module Html
    ( Html
    , Attribute

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


import Prelude ((.), mconcat)
import qualified Prelude

import Data.Text.Lazy.Builder (Builder)
import Html.Attributes (Attribute)
import Html.Internal (Buildable(..))


data Html
    = TextNode   Builder
    | LeafNode   Builder         [Attribute]
    | ParentNode Builder Builder [Attribute] [Html]
    | RootNode   Builder                     [Html]


instance Buildable Html where
    build (TextNode   text                               ) = text
    build (LeafNode   startTag        []                 ) = mconcat [ startTag,                   ">"                         ]
    build (LeafNode   startTag        attributes         ) = mconcat [ startTag, build attributes, ">"                         ]
    build (ParentNode startTag endTag []         []      ) = mconcat [ startTag,                   ">",                 endTag ]
    build (ParentNode startTag endTag attributes []      ) = mconcat [ startTag, build attributes, ">",                 endTag ]
    build (ParentNode startTag endTag []         children) = mconcat [ startTag,                   ">", build children, endTag ]
    build (ParentNode startTag endTag attributes children) = mconcat [ startTag, build attributes, ">", build children, endTag ]
    build (RootNode   startTag                   []      ) = mconcat [ startTag                                                ]
    build (RootNode   startTag                   children) = mconcat [ startTag,                        build children         ]


instance Buildable [Html] where
    build = mconcat . Prelude.map build


-- | The __DOCTYPE__ preamble declares the document type as HTML.
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
--             [ Html.p []
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
--         \<p\>Gigatron is an 8-bit microcomputer built from TTL chips.\<\/p\>
--     \<\/body\>
-- \<\/html\>
-- @
doctype :: [Html] -> Html
doctype = RootNode "<!DOCTYPE html>\n"
{-# INLINE doctype #-}


-- | The __\<a\>__ tag defines a hyperlink.
--
-- ==== __Example__
--
-- @
-- Html.a
--     [ Attr.href \"\/about\" ]
--     [ Html.text \"About\" ]
-- @
--
-- __Result:__
--
-- @
-- \<a href=\"\/about\"\>
--     About
-- \<\/a\>
-- @
a :: [Attribute] -> [Html] -> Html
a = ParentNode "<a" "</a>"
{-# INLINE a #-}


-- | The __\<abbr\>__ tag defines an abbreviation or acronym.
--
-- ==== __Example__
--
-- @
-- Html.abbr
--     [ Attr.title \"American Standard Code for Information Interchange\" ]
--     [ Html.text \"ASCII\" ]
-- @
--
-- __Result:__
--
-- @
-- \<abbr title=\"American Standard Code for Information Interchange\"\>
--     ASCII
-- \<\/abbr\>
-- @
abbr :: [Attribute] -> [Html] -> Html
abbr = ParentNode "<abbr" "</abbr>"
{-# INLINE abbr #-}


-- | The __\<address\>__ tag defines contact information.
--
-- ==== __Example__
--
-- @
-- Html.address []
--     [ Html.text \"123 Main St\"
--     , Html.br []
--     , Html.text \"Anytown, USA\"
--     ]
-- @
--
-- __Result:__
--
-- @
-- \<address\>
--     123 Main St\<br\>
--     Anytown, USA
-- \<\/address\>
-- @
address :: [Attribute] -> [Html] -> Html
address = ParentNode "<address" "</address>"
{-# INLINE address #-}


-- | The __\<area\>__ tag defines a hyperlink or dead area on an image map.
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
area :: [Attribute] -> Html
area = LeafNode "<area"
{-# INLINE area #-}


-- | The __\<article\>__ tag defines a self-contained syndicatable or reusable composition.
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
article :: [Attribute] -> [Html] -> Html
article = ParentNode "<article" "</article>"
{-# INLINE article #-}


-- | The __\<aside\>__ tag defines a sidebar for tangentially related content.
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
aside :: [Attribute] -> [Html] -> Html
aside = ParentNode "<aside" "</aside>"
{-# INLINE aside #-}


-- | The __\<audio\>__ tag defines an audio player.
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
-- \<audio controls=\"controls\"\>
--     \<source src=\"bossfight-warp.mp3\" type=\"audio/mpeg\"\>
--     Your browser does not support the audio tag.
-- \<\/audio\>
-- @
audio :: [Attribute] -> [Html] -> Html
audio = ParentNode "<audio" "</audio>"
{-# INLINE audio #-}


-- | The __\<b\>__ tag defines a span of text to which attention is being drawn without conveying extra importance.
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
b :: [Attribute] -> [Html] -> Html
b = ParentNode "<b" "</b>"
{-# INLINE b #-}


-- | The __\<base\>__ tag defines the base URL of a document.
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
base :: [Attribute] -> Html
base = LeafNode "<base"
{-# INLINE base #-}


-- | The __\<bdi\>__ tag defines text directionality isolation.
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
bdi :: [Attribute] -> [Html] -> Html
bdi = ParentNode "<bdi" "</bdi>"
{-# INLINE bdi #-}


-- | The __\<bdo\>__ tag defines text directionality formatting.
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
bdo :: [Attribute] -> [Html] -> Html
bdo = ParentNode "<bdo" "</bdo>"
{-# INLINE bdo #-}


-- | The __\<blockquote\>__ tag defines a section quoted from another source.
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
blockquote :: [Attribute] -> [Html] -> Html
blockquote = ParentNode "<blockquote" "</blockquote>"
{-# INLINE blockquote #-}


-- | The __\<body\>__ tag defines the document body.
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
body :: [Attribute] -> [Html] -> Html
body = ParentNode "<body" "</body>"
{-# INLINE body #-}


-- | The __\<br\>__ tag defines a line break.
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
br :: [Attribute] -> Html
br = LeafNode "<br"
{-# INLINE br #-}


-- | The __\<button\>__ tag defines a button control.
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
button :: [Attribute] -> [Html] -> Html
button = ParentNode "<button" "</button>"
{-# INLINE button #-}


-- | The __\<canvas\>__ tag defines a scriptable bitmap canvas.
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
canvas :: [Attribute] -> [Html] -> Html
canvas = ParentNode "<canvas" "</canvas>"
{-# INLINE canvas #-}


-- | The __\<caption\>__ tag defines a table caption.
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
caption :: [Attribute] -> [Html] -> Html
caption = ParentNode "<caption" "</caption>"
{-# INLINE caption #-}


-- | The __\<cite\>__ tag defines the title of a work.
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
cite :: [Attribute] -> [Html] -> Html
cite = ParentNode "<cite" "</cite>"
{-# INLINE cite #-}


-- | The __\<code\>__ tag defines a fragment of computer code.
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
code :: [Attribute] -> [Html] -> Html
code = ParentNode "<code" "</code>"
{-# INLINE code #-}


-- | The __\<col\>__ tag defines a table column.
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
col :: [Attribute] -> Html
col = LeafNode "<col"
{-# INLINE col #-}


-- | The __\<colgroup\>__ tag defines a group of columns in a table.
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
colgroup :: [Attribute] -> [Html] -> Html
colgroup = ParentNode "<colgroup" "</colgroup>"
{-# INLINE colgroup #-}


-- | The __\<data\>__ tag defines a machine-readable equivalent of its content.
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
data_ :: [Attribute] -> [Html] -> Html
data_ = ParentNode "<data_" "</data_>"
{-# INLINE data_ #-}


-- | The __\<datalist\>__ tag defines a container for options for a combo box control.
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
datalist :: [Attribute] -> [Html] -> Html
datalist = ParentNode "<datalist" "</datalist>"
{-# INLINE datalist #-}


-- | The __\<dd\>__ tag defines the description part of a term-description group in a description list.
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
dd :: [Attribute] -> [Html] -> Html
dd = ParentNode "<dd" "</dd>"
{-# INLINE dd #-}


-- | The __\<del\>__ tag defines a removal from the document.
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
del :: [Attribute] -> [Html] -> Html
del = ParentNode "<del" "</del>"
{-# INLINE del #-}


-- | The __\<details\>__ tag defines a disclosure control for hiding details.
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
details :: [Attribute] -> [Html] -> Html
details = ParentNode "<details" "</details>"
{-# INLINE details #-}


dfn :: [Attribute] -> [Html] -> Html
dfn = ParentNode "<dfn" "</dfn>"
{-# INLINE dfn #-}


dialog :: [Attribute] -> [Html] -> Html
dialog = ParentNode "<dialog" "</dialog>"
{-# INLINE dialog #-}


div :: [Attribute] -> [Html] -> Html
div = ParentNode "<div" "</div>"
{-# INLINE div #-}


-- | The __\<dl\>__ tag defines a description list.
dl :: [Attribute] -> [Html] -> Html
dl = ParentNode "<dl" "</dl>"
{-# INLINE dl #-}


-- | The __\<dt\>__ tag defines the term part of a term-description group in a description list.
dt :: [Attribute] -> [Html] -> Html
dt = ParentNode "<dt" "</dt>"
{-# INLINE dt #-}


em :: [Attribute] -> [Html] -> Html
em = ParentNode "<em" "</em>"
{-# INLINE em #-}


embed :: [Attribute] -> Html
embed = LeafNode "<embed"
{-# INLINE embed #-}


fieldset :: [Attribute] -> [Html] -> Html
fieldset = ParentNode "<fieldset" "</fieldset>"
{-# INLINE fieldset #-}


figcaption :: [Attribute] -> [Html] -> Html
figcaption = ParentNode "<figcaption" "</figcaption>"
{-# INLINE figcaption #-}


figure :: [Attribute] -> [Html] -> Html
figure = ParentNode "<figure" "</figure>"
{-# INLINE figure #-}


footer :: [Attribute] -> [Html] -> Html
footer = ParentNode "<footer" "</footer>"
{-# INLINE footer #-}


form :: [Attribute] -> [Html] -> Html
form = ParentNode "<form" "</form>"
{-# INLINE form #-}


h1 :: [Attribute] -> [Html] -> Html
h1 = ParentNode "<h1" "</h1>"
{-# INLINE h1 #-}


h2 :: [Attribute] -> [Html] -> Html
h2 = ParentNode "<h2" "</h2>"
{-# INLINE h2 #-}


h3 :: [Attribute] -> [Html] -> Html
h3 = ParentNode "<h3" "</h3>"
{-# INLINE h3 #-}


h4 :: [Attribute] -> [Html] -> Html
h4 = ParentNode "<h4" "</h4>"
{-# INLINE h4 #-}


h5 :: [Attribute] -> [Html] -> Html
h5 = ParentNode "<h5" "</h5>"
{-# INLINE h5 #-}


h6 :: [Attribute] -> [Html] -> Html
h6 = ParentNode "<h6" "</h6>"
{-# INLINE h6 #-}


head :: [Attribute] -> [Html] -> Html
head = ParentNode "<head" "</head>"
{-# INLINE head #-}


header :: [Attribute] -> [Html] -> Html
header = ParentNode "<header" "</header>"
{-# INLINE header #-}


hgroup :: [Attribute] -> [Html] -> Html
hgroup = ParentNode "<hgroup" "</hgroup>"
{-# INLINE hgroup #-}


hr :: [Attribute] -> Html
hr = LeafNode "<hr"
{-# INLINE hr #-}


html :: [Attribute] -> [Html] -> Html
html = ParentNode "<html" "</html>"
{-# INLINE html #-}


i :: [Attribute] -> [Html] -> Html
i = ParentNode "<i" "</i>"
{-# INLINE i #-}


iframe :: [Attribute] -> [Html] -> Html
iframe = ParentNode "<iframe" "</iframe>"
{-# INLINE iframe #-}


img :: [Attribute] -> Html
img = LeafNode "<img"
{-# INLINE img #-}


input :: [Attribute] -> Html
input = LeafNode "<input"
{-# INLINE input #-}


ins :: [Attribute] -> [Html] -> Html
ins = ParentNode "<ins" "</ins>"
{-# INLINE ins #-}


kbd :: [Attribute] -> [Html] -> Html
kbd = ParentNode "<kbd" "</kbd>"
{-# INLINE kbd #-}


label :: [Attribute] -> [Html] -> Html
label = ParentNode "<label" "</label>"
{-# INLINE label #-}


legend :: [Attribute] -> [Html] -> Html
legend = ParentNode "<legend" "</legend>"
{-# INLINE legend #-}


li :: [Attribute] -> [Html] -> Html
li = ParentNode "<li" "</li>"
{-# INLINE li #-}


link :: [Attribute] -> Html
link = LeafNode "<link"
{-# INLINE link #-}


main :: [Attribute] -> [Html] -> Html
main = ParentNode "<main" "</main>"
{-# INLINE main #-}


map :: [Attribute] -> [Html] -> Html
map = ParentNode "<map" "</map>"
{-# INLINE map #-}


mark :: [Attribute] -> [Html] -> Html
mark = ParentNode "<mark" "</mark>"
{-# INLINE mark #-}


menu :: [Attribute] -> [Html] -> Html
menu = ParentNode "<menu" "</menu>"
{-# INLINE menu #-}


meta :: [Attribute] -> Html
meta = LeafNode "<meta"
{-# INLINE meta #-}


meter :: [Attribute] -> [Html] -> Html
meter = ParentNode "<meter" "</meter>"
{-# INLINE meter #-}


nav :: [Attribute] -> [Html] -> Html
nav = ParentNode "<nav" "</nav>"
{-# INLINE nav #-}


noscript :: [Attribute] -> [Html] -> Html
noscript = ParentNode "<noscript" "</noscript>"
{-# INLINE noscript #-}


object :: [Attribute] -> [Html] -> Html
object = ParentNode "<object" "</object>"
{-# INLINE object #-}


ol :: [Attribute] -> [Html] -> Html
ol = ParentNode "<ol" "</ol>"
{-# INLINE ol #-}


optgroup :: [Attribute] -> [Html] -> Html
optgroup = ParentNode "<optgroup" "</optgroup>"
{-# INLINE optgroup #-}


option :: [Attribute] -> [Html] -> Html
option = ParentNode "<option" "</option>"
{-# INLINE option #-}


output :: [Attribute] -> [Html] -> Html
output = ParentNode "<output" "</output>"
{-# INLINE output #-}


p :: [Attribute] -> [Html] -> Html
p = ParentNode "<p" "</p>"
{-# INLINE p #-}


picture :: [Attribute] -> [Html] -> Html
picture = ParentNode "<picture" "</picture>"
{-# INLINE picture #-}


pre :: [Attribute] -> [Html] -> Html
pre = ParentNode "<pre" "</pre>"
{-# INLINE pre #-}


progress :: [Attribute] -> [Html] -> Html
progress = ParentNode "<progress" "</progress>"
{-# INLINE progress #-}


q :: [Attribute] -> [Html] -> Html
q = ParentNode "<q" "</q>"
{-# INLINE q #-}


rp :: [Attribute] -> [Html] -> Html
rp = ParentNode "<rp" "</rp>"
{-# INLINE rp #-}


rt :: [Attribute] -> [Html] -> Html
rt = ParentNode "<rt" "</rt>"
{-# INLINE rt #-}


ruby :: [Attribute] -> [Html] -> Html
ruby = ParentNode "<ruby" "</ruby>"
{-# INLINE ruby #-}


s :: [Attribute] -> [Html] -> Html
s = ParentNode "<s" "</s>"
{-# INLINE s #-}


samp :: [Attribute] -> [Html] -> Html
samp = ParentNode "<samp" "</samp>"
{-# INLINE samp #-}


script :: [Attribute] -> [Html] -> Html
script = ParentNode "<script" "</script>"
{-# INLINE script #-}


section :: [Attribute] -> [Html] -> Html
section = ParentNode "<section" "</section>"
{-# INLINE section #-}


select :: [Attribute] -> [Html] -> Html
select = ParentNode "<select" "</select>"
{-# INLINE select #-}


slot :: [Attribute] -> [Html] -> Html
slot = ParentNode "<slot" "</slot>"
{-# INLINE slot #-}


small :: [Attribute] -> [Html] -> Html
small = ParentNode "<small" "</small>"
{-# INLINE small #-}


source :: [Attribute] -> Html
source = LeafNode "<source"
{-# INLINE source #-}


span :: [Attribute] -> [Html] -> Html
span = ParentNode "<span" "</span>"
{-# INLINE span #-}


strong :: [Attribute] -> [Html] -> Html
strong = ParentNode "<strong" "</strong>"
{-# INLINE strong #-}


style :: [Attribute] -> [Html] -> Html
style = ParentNode "<style" "</style>"
{-# INLINE style #-}


sub :: [Attribute] -> [Html] -> Html
sub = ParentNode "<sub" "</sub>"
{-# INLINE sub #-}


summary :: [Attribute] -> [Html] -> Html
summary = ParentNode "<summary" "</summary>"
{-# INLINE summary #-}


sup :: [Attribute] -> [Html] -> Html
sup = ParentNode "<sup" "</sup>"
{-# INLINE sup #-}


table :: [Attribute] -> [Html] -> Html
table = ParentNode "<table" "</table>"
{-# INLINE table #-}


tbody :: [Attribute] -> [Html] -> Html
tbody = ParentNode "<tbody" "</tbody>"
{-# INLINE tbody #-}


td :: [Attribute] -> [Html] -> Html
td = ParentNode "<td" "</td>"
{-# INLINE td #-}


template :: [Attribute] -> [Html] -> Html
template = ParentNode "<template" "</template>"
{-# INLINE template #-}


textarea :: [Attribute] -> [Html] -> Html
textarea = ParentNode "<textarea" "</textarea>"
{-# INLINE textarea #-}


tfoot :: [Attribute] -> [Html] -> Html
tfoot = ParentNode "<tfoot" "</tfoot>"
{-# INLINE tfoot #-}


th :: [Attribute] -> [Html] -> Html
th = ParentNode "<th" "</th>"
{-# INLINE th #-}


thead :: [Attribute] -> [Html] -> Html
thead = ParentNode "<thead" "</thead>"
{-# INLINE thead #-}


time :: [Attribute] -> [Html] -> Html
time = ParentNode "<time" "</time>"
{-# INLINE time #-}


title :: [Attribute] -> [Html] -> Html
title = ParentNode "<title" "</title>"
{-# INLINE title #-}


tr :: [Attribute] -> [Html] -> Html
tr = ParentNode "<tr" "</tr>"
{-# INLINE tr #-}


track :: [Attribute] -> Html
track = LeafNode "<track"
{-# INLINE track #-}


u :: [Attribute] -> [Html] -> Html
u = ParentNode "<u" "</u>"
{-# INLINE u #-}


ul :: [Attribute] -> [Html] -> Html
ul = ParentNode "<ul" "</ul>"
{-# INLINE ul #-}


var :: [Attribute] -> [Html] -> Html
var = ParentNode "<var" "</var>"
{-# INLINE var #-}


video :: [Attribute] -> [Html] -> Html
video = ParentNode "<video" "</video>"
{-# INLINE video #-}


wbr :: [Attribute] -> Html
wbr = LeafNode "<wbr"
{-# INLINE wbr #-}


text :: Builder -> Html
text = TextNode
{-# INLINE text #-}
