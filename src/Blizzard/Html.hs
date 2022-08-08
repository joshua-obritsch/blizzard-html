{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}


-- | This module defines a set of types and functions responsible for generating and using HTML elements; and aims to be
-- compliant with the HTML Living Standard.
--
-- All examples of elements assume the following imports:
--
-- @
-- import qualified Blizzard.Html as Html
-- import qualified Blizzard.Html.Attributes as Attr
-- @
module Blizzard.Html
    ( Html
    , Attribute
    , (<|)

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


import Data.Text (Text)
import Prelude ((>>), ($), (.), Maybe)
import Text.Blaze (toMarkup)
import Text.Blaze.Internal (MarkupM(..), preEscapedText)

import qualified Text.Blaze.Internal

import Blizzard.Internal.Html (Html, documentTag, normalTag, voidTag)


-- | The __Attribute__ type defines an HTML attribute.
--
-- ==== __Example__
--
-- @
-- import Blizzard.Html (Attribute, Html)
--
-- import qualified Blizzard.Html as Html
--
-- renderSubmitButton :: [Attribute] -> Html
-- renderSubmitButton attrs =
--     Html.button attrs
--         [ Html.text \"Submit\" ]
-- @
type Attribute = Text.Blaze.Internal.Attribute


infixr 0 <|

(<|) = ($)


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
doctype = documentTag . (:) (preEscapedText "<!DOCTYPE html>\n")
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
-- > <a href="/about">About</a>
a :: [Maybe Attribute] -> [Html] -> Html
a = normalTag $ Parent "a" "<a" "</a>"
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
-- > <abbr title="American Standard Code for Information Interchange">ASCII</abbr>
abbr :: [Maybe Attribute] -> [Html] -> Html
abbr = normalTag $ Parent "abbr" "<abbr" "</abbr>"
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
-- > <address>123 Main St<br>Anytown, USA</address>
address :: [Maybe Attribute] -> [Html] -> Html
address = normalTag $ Parent "address" "<address" "</address>"
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
area :: [Maybe Attribute] -> Html
area = voidTag $ Leaf "area" "<area" ">" ()
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
article :: [Maybe Attribute] -> [Html] -> Html
article = normalTag $ Parent "article" "<article" "</article>"
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
aside :: [Maybe Attribute] -> [Html] -> Html
aside = normalTag $ Parent "aside" "<aside" "</aside>"
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
audio :: [Maybe Attribute] -> [Html] -> Html
audio = normalTag $ Parent "audio" "<audio" "</audio>"
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
b :: [Maybe Attribute] -> [Html] -> Html
b = normalTag $ Parent "b" "<b" "</b>"
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
base :: [Maybe Attribute] -> Html
base = voidTag $ Leaf "base" "<base" ">" ()
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
bdi :: [Maybe Attribute] -> [Html] -> Html
bdi = normalTag $ Parent "bdi" "<bdi" "</bdi>"
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
bdo :: [Maybe Attribute] -> [Html] -> Html
bdo = normalTag $ Parent "bdo" "<bdo" "</bdo>"
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
blockquote :: [Maybe Attribute] -> [Html] -> Html
blockquote = normalTag $ Parent "blockquote" "<blockquote" "</blockquote>"
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
body :: [Maybe Attribute] -> [Html] -> Html
body = normalTag $ Parent "body" "<body" "</body>"
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
br :: [Maybe Attribute] -> Html
br = voidTag $ Leaf "br" "<br" ">" ()
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
button :: [Maybe Attribute] -> [Html] -> Html
button = normalTag $ Parent "button" "<button" "</button>"
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
canvas :: [Maybe Attribute] -> [Html] -> Html
canvas = normalTag $ Parent "canvas" "<canvas" "</canvas>"
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
caption :: [Maybe Attribute] -> [Html] -> Html
caption = normalTag $ Parent "caption" "<caption" "</caption>"
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
cite :: [Maybe Attribute] -> [Html] -> Html
cite = normalTag $ Parent "cite" "<cite" "</cite>"
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
code :: [Maybe Attribute] -> [Html] -> Html
code = normalTag $ Parent "code" "<code" "</code>"
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
col :: [Maybe Attribute] -> Html
col = voidTag $ Leaf "col" "<col" ">" ()
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
colgroup :: [Maybe Attribute] -> [Html] -> Html
colgroup = normalTag $ Parent "colgroup" "<colgroup" "</colgroup>"
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
data_ :: [Maybe Attribute] -> [Html] -> Html
data_ = normalTag $ Parent "data_" "<data_" "</data_>"
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
datalist :: [Maybe Attribute] -> [Html] -> Html
datalist = normalTag $ Parent "datalist" "<datalist" "</datalist>"
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
dd :: [Maybe Attribute] -> [Html] -> Html
dd = normalTag $ Parent "dd" "<dd" "</dd>"
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
del :: [Maybe Attribute] -> [Html] -> Html
del = normalTag $ Parent "del" "<del" "</del>"
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
details :: [Maybe Attribute] -> [Html] -> Html
details = normalTag $ Parent "details" "<details" "</details>"
{-# INLINE details #-}


dfn :: [Maybe Attribute] -> [Html] -> Html
dfn = normalTag $ Parent "dfn" "<dfn" "</dfn>"
{-# INLINE dfn #-}


dialog :: [Maybe Attribute] -> [Html] -> Html
dialog = normalTag $ Parent "dialog" "<dialog" "</dialog>"
{-# INLINE dialog #-}


div :: [Maybe Attribute] -> [Html] -> Html
div = normalTag $ Parent "div" "<div" "</div>"
{-# INLINE div #-}


-- | The __\<dl\>__ tag defines a description list.
dl :: [Maybe Attribute] -> [Html] -> Html
dl = normalTag $ Parent "dl" "<dl" "</dl>"
{-# INLINE dl #-}


-- | The __\<dt\>__ tag defines the term part of a term-description group in a description list.
dt :: [Maybe Attribute] -> [Html] -> Html
dt = normalTag $ Parent "dt" "<dt" "</dt>"
{-# INLINE dt #-}


em :: [Maybe Attribute] -> [Html] -> Html
em = normalTag $ Parent "em" "<em" "</em>"
{-# INLINE em #-}


embed :: [Maybe Attribute] -> Html
embed = voidTag $ Leaf "embed" "<embed" ">" ()
{-# INLINE embed #-}


fieldset :: [Maybe Attribute] -> [Html] -> Html
fieldset = normalTag $ Parent "fieldset" "<fieldset" "</fieldset>"
{-# INLINE fieldset #-}


figcaption :: [Maybe Attribute] -> [Html] -> Html
figcaption = normalTag $ Parent "figcaption" "<figcaption" "</figcaption>"
{-# INLINE figcaption #-}


figure :: [Maybe Attribute] -> [Html] -> Html
figure = normalTag $ Parent "figure" "<figure" "</figure>"
{-# INLINE figure #-}


footer :: [Maybe Attribute] -> [Html] -> Html
footer = normalTag $ Parent "footer" "<footer" "</footer>"
{-# INLINE footer #-}


form :: [Maybe Attribute] -> [Html] -> Html
form = normalTag $ Parent "form" "<form" "</form>"
{-# INLINE form #-}


h1 :: [Maybe Attribute] -> [Html] -> Html
h1 = normalTag $ Parent "h1" "<h1" "</h1>"
{-# INLINE h1 #-}


h2 :: [Maybe Attribute] -> [Html] -> Html
h2 = normalTag $ Parent "h2" "<h2" "</h2>"
{-# INLINE h2 #-}


h3 :: [Maybe Attribute] -> [Html] -> Html
h3 = normalTag $ Parent "h3" "<h3" "</h3>"
{-# INLINE h3 #-}


h4 :: [Maybe Attribute] -> [Html] -> Html
h4 = normalTag $ Parent "h4" "<h4" "</h4>"
{-# INLINE h4 #-}


h5 :: [Maybe Attribute] -> [Html] -> Html
h5 = normalTag $ Parent "h5" "<h5" "</h5>"
{-# INLINE h5 #-}


h6 :: [Maybe Attribute] -> [Html] -> Html
h6 = normalTag $ Parent "h6" "<h6" "</h6>"
{-# INLINE h6 #-}


head :: [Maybe Attribute] -> [Html] -> Html
head = normalTag $ Parent "head" "<head" "</head>"
{-# INLINE head #-}


header :: [Maybe Attribute] -> [Html] -> Html
header = normalTag $ Parent "header" "<header" "</header>"
{-# INLINE header #-}


hgroup :: [Maybe Attribute] -> [Html] -> Html
hgroup = normalTag $ Parent "hgroup" "<hgroup" "</hgroup>"
{-# INLINE hgroup #-}


hr :: [Maybe Attribute] -> Html
hr = voidTag $ Leaf "hr" "<hr" ">" ()
{-# INLINE hr #-}


html :: [Maybe Attribute] -> [Html] -> Html
html = normalTag $ Parent "html" "<html" "</html>"
{-# INLINE html #-}


i :: [Maybe Attribute] -> [Html] -> Html
i = normalTag $ Parent "i" "<i" "</i>"
{-# INLINE i #-}


iframe :: [Maybe Attribute] -> [Html] -> Html
iframe = normalTag $ Parent "iframe" "<iframe" "</iframe>"
{-# INLINE iframe #-}


img :: [Maybe Attribute] -> Html
img = voidTag $ Leaf "img" "<img" ">" ()
{-# INLINE img #-}


input :: [Maybe Attribute] -> Html
input = voidTag $ Leaf "input" "<input" ">" ()
{-# INLINE input #-}


ins :: [Maybe Attribute] -> [Html] -> Html
ins = normalTag $ Parent "ins" "<ins" "</ins>"
{-# INLINE ins #-}


kbd :: [Maybe Attribute] -> [Html] -> Html
kbd = normalTag $ Parent "kbd" "<kbd" "</kbd>"
{-# INLINE kbd #-}


label :: [Maybe Attribute] -> [Html] -> Html
label = normalTag $ Parent "label" "<label" "</label>"
{-# INLINE label #-}


legend :: [Maybe Attribute] -> [Html] -> Html
legend = normalTag $ Parent "legend" "<legend" "</legend>"
{-# INLINE legend #-}


li :: [Maybe Attribute] -> [Html] -> Html
li = normalTag $ Parent "li" "<li" "</li>"
{-# INLINE li #-}


link :: [Maybe Attribute] -> Html
link = voidTag $ Leaf "link" "<link" ">" ()
{-# INLINE link #-}


main :: [Maybe Attribute] -> [Html] -> Html
main = normalTag $ Parent "main" "<main" "</main>"
{-# INLINE main #-}


map :: [Maybe Attribute] -> [Html] -> Html
map = normalTag $ Parent "map" "<map" "</map>"
{-# INLINE map #-}


mark :: [Maybe Attribute] -> [Html] -> Html
mark = normalTag $ Parent "mark" "<mark" "</mark>"
{-# INLINE mark #-}


menu :: [Maybe Attribute] -> [Html] -> Html
menu = normalTag $ Parent "menu" "<menu" "</menu>"
{-# INLINE menu #-}


meta :: [Maybe Attribute] -> Html
meta = voidTag $ Leaf "meta" "<meta" ">" ()
{-# INLINE meta #-}


meter :: [Maybe Attribute] -> [Html] -> Html
meter = normalTag $ Parent "meter" "<meter" "</meter>"
{-# INLINE meter #-}


nav :: [Maybe Attribute] -> [Html] -> Html
nav = normalTag $ Parent "nav" "<nav" "</nav>"
{-# INLINE nav #-}


noscript :: [Maybe Attribute] -> [Html] -> Html
noscript = normalTag $ Parent "noscript" "<noscript" "</noscript>"
{-# INLINE noscript #-}


object :: [Maybe Attribute] -> [Html] -> Html
object = normalTag $ Parent "object" "<object" "</object>"
{-# INLINE object #-}


ol :: [Maybe Attribute] -> [Html] -> Html
ol = normalTag $ Parent "ol" "<ol" "</ol>"
{-# INLINE ol #-}


optgroup :: [Maybe Attribute] -> [Html] -> Html
optgroup = normalTag $ Parent "optgroup" "<optgroup" "</optgroup>"
{-# INLINE optgroup #-}


option :: [Maybe Attribute] -> [Html] -> Html
option = normalTag $ Parent "option" "<option" "</option>"
{-# INLINE option #-}


output :: [Maybe Attribute] -> [Html] -> Html
output = normalTag $ Parent "output" "<output" "</output>"
{-# INLINE output #-}


p :: [Maybe Attribute] -> [Html] -> Html
p = normalTag $ Parent "p" "<p" "</p>"
{-# INLINE p #-}


picture :: [Maybe Attribute] -> [Html] -> Html
picture = normalTag $ Parent "picture" "<picture" "</picture>"
{-# INLINE picture #-}


pre :: [Maybe Attribute] -> [Html] -> Html
pre = normalTag $ Parent "pre" "<pre" "</pre>"
{-# INLINE pre #-}


progress :: [Maybe Attribute] -> [Html] -> Html
progress = normalTag $ Parent "progress" "<progress" "</progress>"
{-# INLINE progress #-}


q :: [Maybe Attribute] -> [Html] -> Html
q = normalTag $ Parent "q" "<q" "</q>"
{-# INLINE q #-}


rp :: [Maybe Attribute] -> [Html] -> Html
rp = normalTag $ Parent "rp" "<rp" "</rp>"
{-# INLINE rp #-}


rt :: [Maybe Attribute] -> [Html] -> Html
rt = normalTag $ Parent "rt" "<rt" "</rt>"
{-# INLINE rt #-}


ruby :: [Maybe Attribute] -> [Html] -> Html
ruby = normalTag $ Parent "ruby" "<ruby" "</ruby>"
{-# INLINE ruby #-}


s :: [Maybe Attribute] -> [Html] -> Html
s = normalTag $ Parent "s" "<s" "</s>"
{-# INLINE s #-}


samp :: [Maybe Attribute] -> [Html] -> Html
samp = normalTag $ Parent "samp" "<samp" "</samp>"
{-# INLINE samp #-}


script :: [Maybe Attribute] -> [Html] -> Html
script = normalTag $ Parent "script" "<script" "</script>"
{-# INLINE script #-}


section :: [Maybe Attribute] -> [Html] -> Html
section = normalTag $ Parent "section" "<section" "</section>"
{-# INLINE section #-}


select :: [Maybe Attribute] -> [Html] -> Html
select = normalTag $ Parent "select" "<select" "</select>"
{-# INLINE select #-}


slot :: [Maybe Attribute] -> [Html] -> Html
slot = normalTag $ Parent "slot" "<slot" "</slot>"
{-# INLINE slot #-}


small :: [Maybe Attribute] -> [Html] -> Html
small = normalTag $ Parent "small" "<small" "</small>"
{-# INLINE small #-}


source :: [Maybe Attribute] -> Html
source = voidTag $ Leaf "source" "<source" ">" ()
{-# INLINE source #-}


span :: [Maybe Attribute] -> [Html] -> Html
span = normalTag $ Parent "span" "<span" "</span>"
{-# INLINE span #-}


strong :: [Maybe Attribute] -> [Html] -> Html
strong = normalTag $ Parent "strong" "<strong" "</strong>"
{-# INLINE strong #-}


style :: [Maybe Attribute] -> [Html] -> Html
style = normalTag $ Parent "style" "<style" "</style>"
{-# INLINE style #-}


sub :: [Maybe Attribute] -> [Html] -> Html
sub = normalTag $ Parent "sub" "<sub" "</sub>"
{-# INLINE sub #-}


summary :: [Maybe Attribute] -> [Html] -> Html
summary = normalTag $ Parent "summary" "<summary" "</summary>"
{-# INLINE summary #-}


sup :: [Maybe Attribute] -> [Html] -> Html
sup = normalTag $ Parent "sup" "<sup" "</sup>"
{-# INLINE sup #-}


table :: [Maybe Attribute] -> [Html] -> Html
table = normalTag $ Parent "table" "<table" "</table>"
{-# INLINE table #-}


tbody :: [Maybe Attribute] -> [Html] -> Html
tbody = normalTag $ Parent "tbody" "<tbody" "</tbody>"
{-# INLINE tbody #-}


td :: [Maybe Attribute] -> [Html] -> Html
td = normalTag $ Parent "td" "<td" "</td>"
{-# INLINE td #-}


template :: [Maybe Attribute] -> [Html] -> Html
template = normalTag $ Parent "template" "<template" "</template>"
{-# INLINE template #-}


textarea :: [Maybe Attribute] -> [Html] -> Html
textarea = normalTag $ Parent "textarea" "<textarea" "</textarea>"
{-# INLINE textarea #-}


tfoot :: [Maybe Attribute] -> [Html] -> Html
tfoot = normalTag $ Parent "tfoot" "<tfoot" "</tfoot>"
{-# INLINE tfoot #-}


th :: [Maybe Attribute] -> [Html] -> Html
th = normalTag $ Parent "th" "<th" "</th>"
{-# INLINE th #-}


thead :: [Maybe Attribute] -> [Html] -> Html
thead = normalTag $ Parent "thead" "<thead" "</thead>"
{-# INLINE thead #-}


time :: [Maybe Attribute] -> [Html] -> Html
time = normalTag $ Parent "time" "<time" "</time>"
{-# INLINE time #-}


title :: [Maybe Attribute] -> [Html] -> Html
title = normalTag $ Parent "title" "<title" "</title>"
{-# INLINE title #-}


tr :: [Maybe Attribute] -> [Html] -> Html
tr = normalTag $ Parent "tr" "<tr" "</tr>"
{-# INLINE tr #-}


track :: [Maybe Attribute] -> Html
track = voidTag $ Leaf "track" "<track" ">" ()
{-# INLINE track #-}


u :: [Maybe Attribute] -> [Html] -> Html
u = normalTag $ Parent "u" "<u" "</u>"
{-# INLINE u #-}


ul :: [Maybe Attribute] -> [Html] -> Html
ul = normalTag $ Parent "ul" "<ul" "</ul>"
{-# INLINE ul #-}


var :: [Maybe Attribute] -> [Html] -> Html
var = normalTag $ Parent "var" "<var" "</var>"
{-# INLINE var #-}


video :: [Maybe Attribute] -> [Html] -> Html
video = normalTag $ Parent "video" "<video" "</video>"
{-# INLINE video #-}


wbr :: [Maybe Attribute] -> Html
wbr = voidTag $ Leaf "wbr" "<wbr" ">" ()
{-# INLINE wbr #-}


text :: Text -> Html
text = toMarkup
{-# INLINE text #-}
