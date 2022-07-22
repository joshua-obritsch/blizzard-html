{-# LANGUAGE NoImplicitPrelude #-}

module Blizzard.Html
    ( Attribute
    , Html
    , Text
    , docType
    , docTypeHtml
    , a
    , abbr
    , address
    , area
    , article
    , aside
    , audio
    , b
    , base
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
    , command
    , datalist
    , dd
    , del
    , details
    , dfn
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
    , keygen
    , label
    , legend
    , li
    , link
    , main
    , map
    , mark
    , menu
    , menuitem
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
    , param
    , pre
    , progress
    , q
    , rp
    , rt
    , ruby
    , samp
    , script
    , section
    , select
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
    , text
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
    ) where


import Data.Text (Text)
import Prelude (Maybe)
import Text.Blaze.Html (Attribute, Html)

import qualified Text.Blaze.Html5 as Html

import Blizzard.Internal.Html (documentTag, normalTag, voidTag)


docType :: Html
docType = Html.docType


docTypeHtml :: [Html] -> Html
docTypeHtml = documentTag Html.docTypeHtml


a :: [Maybe Attribute] -> [Html] -> Html
a = normalTag Html.a


abbr :: [Maybe Attribute] -> [Html] -> Html
abbr = normalTag Html.abbr


address :: [Maybe Attribute] -> [Html] -> Html
address = normalTag Html.address


area :: [Maybe Attribute] -> Html
area = voidTag Html.area


article :: [Maybe Attribute] -> [Html] -> Html
article = normalTag Html.article


aside :: [Maybe Attribute] -> [Html] -> Html
aside = normalTag Html.aside


audio :: [Maybe Attribute] -> [Html] -> Html
audio = normalTag Html.audio


b :: [Maybe Attribute] -> [Html] -> Html
b = normalTag Html.b


base :: [Maybe Attribute] -> Html
base = voidTag Html.base


bdo :: [Maybe Attribute] -> [Html] -> Html
bdo = normalTag Html.bdo


blockquote :: [Maybe Attribute] -> [Html] -> Html
blockquote = normalTag Html.blockquote


body :: [Maybe Attribute] -> [Html] -> Html
body = normalTag Html.body


br :: [Maybe Attribute] -> Html
br = voidTag Html.br


button :: [Maybe Attribute] -> [Html] -> Html
button = normalTag Html.button


canvas :: [Maybe Attribute] -> [Html] -> Html
canvas = normalTag Html.canvas


caption :: [Maybe Attribute] -> [Html] -> Html
caption = normalTag Html.caption


cite :: [Maybe Attribute] -> [Html] -> Html
cite = normalTag Html.cite


code :: [Maybe Attribute] -> [Html] -> Html
code = normalTag Html.code


col :: [Maybe Attribute] -> Html
col = voidTag Html.col


colgroup :: [Maybe Attribute] -> [Html] -> Html
colgroup = normalTag Html.colgroup


command :: [Maybe Attribute] -> [Html] -> Html
command = normalTag Html.command


datalist :: [Maybe Attribute] -> [Html] -> Html
datalist = normalTag Html.datalist


dd :: [Maybe Attribute] -> [Html] -> Html
dd = normalTag Html.dd


del :: [Maybe Attribute] -> [Html] -> Html
del = normalTag Html.del


details :: [Maybe Attribute] -> [Html] -> Html
details = normalTag Html.details


dfn :: [Maybe Attribute] -> [Html] -> Html
dfn = normalTag Html.dfn


div :: [Maybe Attribute] -> [Html] -> Html
div = normalTag Html.div


dl :: [Maybe Attribute] -> [Html] -> Html
dl = normalTag Html.dl


dt :: [Maybe Attribute] -> [Html] -> Html
dt = normalTag Html.dt


em :: [Maybe Attribute] -> [Html] -> Html
em = normalTag Html.em


embed :: [Maybe Attribute] -> Html
embed = voidTag Html.embed


fieldset :: [Maybe Attribute] -> [Html] -> Html
fieldset = normalTag Html.fieldset


figcaption :: [Maybe Attribute] -> [Html] -> Html
figcaption = normalTag Html.figcaption


figure :: [Maybe Attribute] -> [Html] -> Html
figure = normalTag Html.figure


footer :: [Maybe Attribute] -> [Html] -> Html
footer = normalTag Html.footer


form :: [Maybe Attribute] -> [Html] -> Html
form = normalTag Html.form


h1 :: [Maybe Attribute] -> [Html] -> Html
h1 = normalTag Html.h1


h2 :: [Maybe Attribute] -> [Html] -> Html
h2 = normalTag Html.h2


h3 :: [Maybe Attribute] -> [Html] -> Html
h3 = normalTag Html.h3


h4 :: [Maybe Attribute] -> [Html] -> Html
h4 = normalTag Html.h4


h5 :: [Maybe Attribute] -> [Html] -> Html
h5 = normalTag Html.h5


h6 :: [Maybe Attribute] -> [Html] -> Html
h6 = normalTag Html.h6


head :: [Maybe Attribute] -> [Html] -> Html
head = normalTag Html.head


header :: [Maybe Attribute] -> [Html] -> Html
header = normalTag Html.header


hgroup :: [Maybe Attribute] -> [Html] -> Html
hgroup = normalTag Html.hgroup


hr :: [Maybe Attribute] -> Html
hr = voidTag Html.hr


html :: [Maybe Attribute] -> [Html] -> Html
html = normalTag Html.html


i :: [Maybe Attribute] -> [Html] -> Html
i = normalTag Html.i


iframe :: [Maybe Attribute] -> [Html] -> Html
iframe = normalTag Html.iframe


img :: [Maybe Attribute] -> Html
img = voidTag Html.img


input :: [Maybe Attribute] -> Html
input = voidTag Html.input


ins :: [Maybe Attribute] -> [Html] -> Html
ins = normalTag Html.ins


kbd :: [Maybe Attribute] -> [Html] -> Html
kbd = normalTag Html.kbd


keygen :: [Maybe Attribute] -> Html
keygen = voidTag Html.keygen


label :: [Maybe Attribute] -> [Html] -> Html
label = normalTag Html.label


legend :: [Maybe Attribute] -> [Html] -> Html
legend = normalTag Html.legend


li :: [Maybe Attribute] -> [Html] -> Html
li = normalTag Html.li


link :: [Maybe Attribute] -> Html
link = voidTag Html.link


main :: [Maybe Attribute] -> [Html] -> Html
main = normalTag Html.main


map :: [Maybe Attribute] -> [Html] -> Html
map = normalTag Html.map


mark :: [Maybe Attribute] -> [Html] -> Html
mark = normalTag Html.mark


menu :: [Maybe Attribute] -> [Html] -> Html
menu = normalTag Html.menu


menuitem :: [Maybe Attribute] -> Html
menuitem = voidTag Html.menuitem


meta :: [Maybe Attribute] -> Html
meta = voidTag Html.meta


meter :: [Maybe Attribute] -> [Html] -> Html
meter = normalTag Html.meter


nav :: [Maybe Attribute] -> [Html] -> Html
nav = normalTag Html.nav


noscript :: [Maybe Attribute] -> [Html] -> Html
noscript = normalTag Html.noscript


object :: [Maybe Attribute] -> [Html] -> Html
object = normalTag Html.object


ol :: [Maybe Attribute] -> [Html] -> Html
ol = normalTag Html.ol


optgroup :: [Maybe Attribute] -> [Html] -> Html
optgroup = normalTag Html.optgroup


option :: [Maybe Attribute] -> [Html] -> Html
option = normalTag Html.option


output :: [Maybe Attribute] -> [Html] -> Html
output = normalTag Html.output


p :: [Maybe Attribute] -> [Html] -> Html
p = normalTag Html.p


param :: [Maybe Attribute] -> Html
param = voidTag Html.param


pre :: [Maybe Attribute] -> [Html] -> Html
pre = normalTag Html.pre


progress :: [Maybe Attribute] -> [Html] -> Html
progress = normalTag Html.progress


q :: [Maybe Attribute] -> [Html] -> Html
q = normalTag Html.q


rp :: [Maybe Attribute] -> [Html] -> Html
rp = normalTag Html.rp


rt :: [Maybe Attribute] -> [Html] -> Html
rt = normalTag Html.rt


ruby :: [Maybe Attribute] -> [Html] -> Html
ruby = normalTag Html.ruby


samp :: [Maybe Attribute] -> [Html] -> Html
samp = normalTag Html.samp


script :: [Maybe Attribute] -> [Html] -> Html
script = normalTag Html.script


section :: [Maybe Attribute] -> [Html] -> Html
section = normalTag Html.section


select :: [Maybe Attribute] -> [Html] -> Html
select = normalTag Html.select


small :: [Maybe Attribute] -> [Html] -> Html
small = normalTag Html.small


source :: [Maybe Attribute] -> Html
source = voidTag Html.source


span :: [Maybe Attribute] -> [Html] -> Html
span = normalTag Html.span


strong :: [Maybe Attribute] -> [Html] -> Html
strong = normalTag Html.strong


style :: [Maybe Attribute] -> [Html] -> Html
style = normalTag Html.style


sub :: [Maybe Attribute] -> [Html] -> Html
sub = normalTag Html.sub


summary :: [Maybe Attribute] -> [Html] -> Html
summary = normalTag Html.summary


sup :: [Maybe Attribute] -> [Html] -> Html
sup = normalTag Html.sup


table :: [Maybe Attribute] -> [Html] -> Html
table = normalTag Html.table


tbody :: [Maybe Attribute] -> [Html] -> Html
tbody = normalTag Html.tbody


td :: [Maybe Attribute] -> [Html] -> Html
td = normalTag Html.td


text :: Text -> Html
text = Html.toHtml


textarea :: [Maybe Attribute] -> [Html] -> Html
textarea = normalTag Html.textarea


tfoot :: [Maybe Attribute] -> [Html] -> Html
tfoot = normalTag Html.tfoot


th :: [Maybe Attribute] -> [Html] -> Html
th = normalTag Html.th


thead :: [Maybe Attribute] -> [Html] -> Html
thead = normalTag Html.thead


time :: [Maybe Attribute] -> [Html] -> Html
time = normalTag Html.time


title :: [Maybe Attribute] -> [Html] -> Html
title = normalTag Html.title


tr :: [Maybe Attribute] -> [Html] -> Html
tr = normalTag Html.tr


track :: [Maybe Attribute] -> Html
track = voidTag Html.track


u :: [Maybe Attribute] -> [Html] -> Html
u = normalTag Html.u


ul :: [Maybe Attribute] -> [Html] -> Html
ul = normalTag Html.ul


var :: [Maybe Attribute] -> [Html] -> Html
var = normalTag Html.var


video :: [Maybe Attribute] -> [Html] -> Html
video = normalTag Html.video


wbr :: [Maybe Attribute] -> Html
wbr = voidTag Html.wbr
