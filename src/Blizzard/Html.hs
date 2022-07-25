{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- | All examples assume the following imports:
--
-- @
-- import qualified Blizzard.Html as Html
-- import qualified Blizzard.Html.Attributes as Attr
-- @
module Blizzard.Html
    ( -- * Data Types
      Attribute
    , Text

      -- * Type Synonyms
    , Html

      -- * Doctype
    , doctype
    , doctype'

      -- * Standard Elements
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

      -- * Auxiliary Elements
    , text

    , renderHtml
    ) where


import Text.Blaze.Renderer.Text (renderHtml)


import Data.Text (Text)
import Prelude ((>>), ($), (.), Maybe)
import Text.Blaze (toMarkup)
import Text.Blaze.Internal (Attribute, MarkupM(..), preEscapedText)

import Blizzard.Internal.Html (Html, documentTag, normalTag, voidTag)


doctype :: [Html] -> Html
doctype = documentTag . (:) doctype'


doctype' :: Html
doctype' = preEscapedText "<!DOCTYPE HTML>\n"


-- | The __\<a\>__ tag defines a hyperlink.
--
-- ==== __Example__
--
-- @
-- Html.a
--     [ Attr.href \"/about\" ]
--     [ Html.text \"About\" ]
-- @
--
-- __Result:__
--
-- > <a href="/about">About</a>
a :: [Maybe Attribute] -> [Html] -> Html
a = normalTag $ Parent "a" "<a" "</a>"


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


-- | The __\<address\>__ tag defines contact information for a page or article.
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


-- | The __\<b\>__ tag defines a key word.
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


-- | The __\<caption\>__ tag defines a table caption.
--
-- ==== __Example__
--
-- @
-- Html.table []
--     [ Html.caption []
--         [ Html.text \"Monthly profits\" ]
--     , Html.tr []
--         [ Html.th []
--             [ Html.text \"Month\" ]
--         , Html.th []
--             [ Html.text \"Profits\" ]
--         ]
--     , Html.tr []
--         [ Html.td []
--             [ Html.text \"January\" ]
--         , Html.td []
--             [ Html.text \"$20,000\" ]
--         ]
--     ]
-- @
--
-- __Result:__
--
-- @
-- \<table\>
--     \<caption\>Monthly profits\<\/caption\>
--     \<tr\>
--         \<th\>Month\<\/th\>
--         \<th\>Profits\<\/th\>
--     \<\/tr\>
--     \<tr\>
--         \<td\>January\<\/td\>
--         \<td\>$20,000\<\/td\>
--     \<\/tr\>
-- \<\/table\>
-- @
caption :: [Maybe Attribute] -> [Html] -> Html
caption = normalTag $ Parent "caption" "<caption" "</caption>"


-- | The __\<cite\>__ tag defines the title of a work.
--
-- ==== __Example__
--
-- @
-- Html.p []
--     [ Html.text \"My favorite movie by Alfred Hitchcock is \"
--     , Html.cite []
--         [ Html.text \"Psycho\" ]
--     , Html.text \".\"
--     ]
-- @
--
-- __Result:__
--
-- > <p>My favorite movie by Alfred Hitchcock is <cite>Psycho</cite>.</p>
cite :: [Maybe Attribute] -> [Html] -> Html
cite = normalTag $ Parent "cite" "<cite" "</cite>"


-- | The __\<code\>__ tag defines computer code.
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


-- | The __\<col\>__ tag defines a table column.
--
-- ==== __Example__
--
-- @
-- Html.table []
--     []
-- @
col :: [Maybe Attribute] -> Html
col = voidTag $ Leaf "col" "<col" ">" ()


colgroup :: [Maybe Attribute] -> [Html] -> Html
colgroup = normalTag $ Parent "colgroup" "<colgroup" "</colgroup>"


data_ :: [Maybe Attribute] -> [Html] -> Html
data_ = normalTag $ Parent "data_" "<data_" "</data_>"


datalist :: [Maybe Attribute] -> [Html] -> Html
datalist = normalTag $ Parent "datalist" "<datalist" "</datalist>"


dd :: [Maybe Attribute] -> [Html] -> Html
dd = normalTag $ Parent "dd" "<dd" "</dd>"


del :: [Maybe Attribute] -> [Html] -> Html
del = normalTag $ Parent "del" "<del" "</del>"


details :: [Maybe Attribute] -> [Html] -> Html
details = normalTag $ Parent "details" "<details" "</details>"


dfn :: [Maybe Attribute] -> [Html] -> Html
dfn = normalTag $ Parent "dfn" "<dfn" "</dfn>"


dialog :: [Maybe Attribute] -> [Html] -> Html
dialog = normalTag $ Parent "dialog" "<dialog" "</dialog>"


div :: [Maybe Attribute] -> [Html] -> Html
div = normalTag $ Parent "div" "<div" "</div>"


dl :: [Maybe Attribute] -> [Html] -> Html
dl = normalTag $ Parent "dl" "<dl" "</dl>"


dt :: [Maybe Attribute] -> [Html] -> Html
dt = normalTag $ Parent "dt" "<dt" "</dt>"


em :: [Maybe Attribute] -> [Html] -> Html
em = normalTag $ Parent "em" "<em" "</em>"


embed :: [Maybe Attribute] -> Html
embed = voidTag $ Leaf "embed" "<embed" ">" ()


fieldset :: [Maybe Attribute] -> [Html] -> Html
fieldset = normalTag $ Parent "fieldset" "<fieldset" "</fieldset>"


figcaption :: [Maybe Attribute] -> [Html] -> Html
figcaption = normalTag $ Parent "figcaption" "<figcaption" "</figcaption>"


figure :: [Maybe Attribute] -> [Html] -> Html
figure = normalTag $ Parent "figure" "<figure" "</figure>"


footer :: [Maybe Attribute] -> [Html] -> Html
footer = normalTag $ Parent "footer" "<footer" "</footer>"


form :: [Maybe Attribute] -> [Html] -> Html
form = normalTag $ Parent "form" "<form" "</form>"


h1 :: [Maybe Attribute] -> [Html] -> Html
h1 = normalTag $ Parent "h1" "<h1" "</h1>"


h2 :: [Maybe Attribute] -> [Html] -> Html
h2 = normalTag $ Parent "h2" "<h2" "</h2>"


h3 :: [Maybe Attribute] -> [Html] -> Html
h3 = normalTag $ Parent "h3" "<h3" "</h3>"


h4 :: [Maybe Attribute] -> [Html] -> Html
h4 = normalTag $ Parent "h4" "<h4" "</h4>"


h5 :: [Maybe Attribute] -> [Html] -> Html
h5 = normalTag $ Parent "h5" "<h5" "</h5>"


h6 :: [Maybe Attribute] -> [Html] -> Html
h6 = normalTag $ Parent "h6" "<h6" "</h6>"


head :: [Maybe Attribute] -> [Html] -> Html
head = normalTag $ Parent "head" "<head" "</head>"


header :: [Maybe Attribute] -> [Html] -> Html
header = normalTag $ Parent "header" "<header" "</header>"


hgroup :: [Maybe Attribute] -> [Html] -> Html
hgroup = normalTag $ Parent "hgroup" "<hgroup" "</hgroup>"


hr :: [Maybe Attribute] -> Html
hr = voidTag $ Leaf "hr" "<hr" ">" ()


html :: [Maybe Attribute] -> [Html] -> Html
html = normalTag $ Parent "html" "<html" "</html>"


i :: [Maybe Attribute] -> [Html] -> Html
i = normalTag $ Parent "i" "<i" "</i>"


iframe :: [Maybe Attribute] -> [Html] -> Html
iframe = normalTag $ Parent "iframe" "<iframe" "</iframe>"


img :: [Maybe Attribute] -> Html
img = voidTag $ Leaf "img" "<img" ">" ()


input :: [Maybe Attribute] -> Html
input = voidTag $ Leaf "input" "<input" ">" ()


ins :: [Maybe Attribute] -> [Html] -> Html
ins = normalTag $ Parent "ins" "<ins" "</ins>"


kbd :: [Maybe Attribute] -> [Html] -> Html
kbd = normalTag $ Parent "kbd" "<kbd" "</kbd>"


label :: [Maybe Attribute] -> [Html] -> Html
label = normalTag $ Parent "label" "<label" "</label>"


legend :: [Maybe Attribute] -> [Html] -> Html
legend = normalTag $ Parent "legend" "<legend" "</legend>"


li :: [Maybe Attribute] -> [Html] -> Html
li = normalTag $ Parent "li" "<li" "</li>"


link :: [Maybe Attribute] -> Html
link = voidTag $ Leaf "link" "<link" ">" ()


main :: [Maybe Attribute] -> [Html] -> Html
main = normalTag $ Parent "main" "<main" "</main>"


map :: [Maybe Attribute] -> [Html] -> Html
map = normalTag $ Parent "map" "<map" "</map>"


mark :: [Maybe Attribute] -> [Html] -> Html
mark = normalTag $ Parent "mark" "<mark" "</mark>"


menu :: [Maybe Attribute] -> [Html] -> Html
menu = normalTag $ Parent "menu" "<menu" "</menu>"


meta :: [Maybe Attribute] -> Html
meta = voidTag $ Leaf "meta" "<meta" ">" ()


meter :: [Maybe Attribute] -> [Html] -> Html
meter = normalTag $ Parent "meter" "<meter" "</meter>"


nav :: [Maybe Attribute] -> [Html] -> Html
nav = normalTag $ Parent "nav" "<nav" "</nav>"


noscript :: [Maybe Attribute] -> [Html] -> Html
noscript = normalTag $ Parent "noscript" "<noscript" "</noscript>"


object :: [Maybe Attribute] -> [Html] -> Html
object = normalTag $ Parent "object" "<object" "</object>"


ol :: [Maybe Attribute] -> [Html] -> Html
ol = normalTag $ Parent "ol" "<ol" "</ol>"


optgroup :: [Maybe Attribute] -> [Html] -> Html
optgroup = normalTag $ Parent "optgroup" "<optgroup" "</optgroup>"


option :: [Maybe Attribute] -> [Html] -> Html
option = normalTag $ Parent "option" "<option" "</option>"


output :: [Maybe Attribute] -> [Html] -> Html
output = normalTag $ Parent "output" "<output" "</output>"


p :: [Maybe Attribute] -> [Html] -> Html
p = normalTag $ Parent "p" "<p" "</p>"


picture :: [Maybe Attribute] -> [Html] -> Html
picture = normalTag $ Parent "picture" "<picture" "</picture>"


pre :: [Maybe Attribute] -> [Html] -> Html
pre = normalTag $ Parent "pre" "<pre" "</pre>"


progress :: [Maybe Attribute] -> [Html] -> Html
progress = normalTag $ Parent "progress" "<progress" "</progress>"


q :: [Maybe Attribute] -> [Html] -> Html
q = normalTag $ Parent "q" "<q" "</q>"


rp :: [Maybe Attribute] -> [Html] -> Html
rp = normalTag $ Parent "rp" "<rp" "</rp>"


rt :: [Maybe Attribute] -> [Html] -> Html
rt = normalTag $ Parent "rt" "<rt" "</rt>"


ruby :: [Maybe Attribute] -> [Html] -> Html
ruby = normalTag $ Parent "ruby" "<ruby" "</ruby>"


s :: [Maybe Attribute] -> [Html] -> Html
s = normalTag $ Parent "s" "<s" "</s>"


samp :: [Maybe Attribute] -> [Html] -> Html
samp = normalTag $ Parent "samp" "<samp" "</samp>"


script :: [Maybe Attribute] -> [Html] -> Html
script = normalTag $ Parent "script" "<script" "</script>"


section :: [Maybe Attribute] -> [Html] -> Html
section = normalTag $ Parent "section" "<section" "</section>"


select :: [Maybe Attribute] -> [Html] -> Html
select = normalTag $ Parent "select" "<select" "</select>"


slot :: [Maybe Attribute] -> [Html] -> Html
slot = normalTag $ Parent "slot" "<slot" "</slot>"


small :: [Maybe Attribute] -> [Html] -> Html
small = normalTag $ Parent "small" "<small" "</small>"


source :: [Maybe Attribute] -> Html
source = voidTag $ Leaf "source" "<source" ">" ()


span :: [Maybe Attribute] -> [Html] -> Html
span = normalTag $ Parent "span" "<span" "</span>"


strong :: [Maybe Attribute] -> [Html] -> Html
strong = normalTag $ Parent "strong" "<strong" "</strong>"


style :: [Maybe Attribute] -> [Html] -> Html
style = normalTag $ Parent "style" "<style" "</style>"


sub :: [Maybe Attribute] -> [Html] -> Html
sub = normalTag $ Parent "sub" "<sub" "</sub>"


summary :: [Maybe Attribute] -> [Html] -> Html
summary = normalTag $ Parent "summary" "<summary" "</summary>"


sup :: [Maybe Attribute] -> [Html] -> Html
sup = normalTag $ Parent "sup" "<sup" "</sup>"


table :: [Maybe Attribute] -> [Html] -> Html
table = normalTag $ Parent "table" "<table" "</table>"


tbody :: [Maybe Attribute] -> [Html] -> Html
tbody = normalTag $ Parent "tbody" "<tbody" "</tbody>"


td :: [Maybe Attribute] -> [Html] -> Html
td = normalTag $ Parent "td" "<td" "</td>"


template :: [Maybe Attribute] -> [Html] -> Html
template = normalTag $ Parent "template" "<template" "</template>"


textarea :: [Maybe Attribute] -> [Html] -> Html
textarea = normalTag $ Parent "textarea" "<textarea" "</textarea>"


tfoot :: [Maybe Attribute] -> [Html] -> Html
tfoot = normalTag $ Parent "tfoot" "<tfoot" "</tfoot>"


th :: [Maybe Attribute] -> [Html] -> Html
th = normalTag $ Parent "th" "<th" "</th>"


thead :: [Maybe Attribute] -> [Html] -> Html
thead = normalTag $ Parent "thead" "<thead" "</thead>"


time :: [Maybe Attribute] -> [Html] -> Html
time = normalTag $ Parent "time" "<time" "</time>"


title :: [Maybe Attribute] -> [Html] -> Html
title = normalTag $ Parent "title" "<title" "</title>"


tr :: [Maybe Attribute] -> [Html] -> Html
tr = normalTag $ Parent "tr" "<tr" "</tr>"


track :: [Maybe Attribute] -> Html
track = voidTag $ Leaf "track" "<track" ">" ()


u :: [Maybe Attribute] -> [Html] -> Html
u = normalTag $ Parent "u" "<u" "</u>"


ul :: [Maybe Attribute] -> [Html] -> Html
ul = normalTag $ Parent "ul" "<ul" "</ul>"


var :: [Maybe Attribute] -> [Html] -> Html
var = normalTag $ Parent "var" "<var" "</var>"


video :: [Maybe Attribute] -> [Html] -> Html
video = normalTag $ Parent "video" "<video" "</video>"


wbr :: [Maybe Attribute] -> Html
wbr = voidTag $ Leaf "wbr" "<wbr" ">" ()


text :: Text -> Html
text = toMarkup
