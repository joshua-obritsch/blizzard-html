{-# LANGUAGE NoImplicitPrelude #-}

module Blizzard.Html
    ( Attribute
    , Html
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


import Text.Blaze.Html (Attribute, Html)

import qualified Text.Blaze.Html5 as Html

import Blizzard.Internal.Html (documentTag, normalTag, voidTag)


docType :: Html
docType = Html.docType


docTypeHtml :: [Html] -> Html
docTypeHtml = documentTag Html.docTypeHtml


a :: [Attribute] -> [Html] -> Html
a = normalTag Html.a


abbr :: [Attribute] -> [Html] -> Html
abbr = normalTag Html.abbr


address :: [Attribute] -> [Html] -> Html
address = normalTag Html.address


area :: [Attribute] -> Html
area = voidTag Html.area


article :: [Attribute] -> [Html] -> Html
article = normalTag Html.article


aside :: [Attribute] -> [Html] -> Html
aside = normalTag Html.aside


audio :: [Attribute] -> [Html] -> Html
audio = normalTag Html.audio


b :: [Attribute] -> [Html] -> Html
b = normalTag Html.b


base :: [Attribute] -> Html
base = voidTag Html.base


bdo :: [Attribute] -> [Html] -> Html
bdo = normalTag Html.bdo


blockquote :: [Attribute] -> [Html] -> Html
blockquote = normalTag Html.blockquote


body :: [Attribute] -> [Html] -> Html
body = normalTag Html.body


br :: [Attribute] -> Html
br = voidTag Html.br


button :: [Attribute] -> [Html] -> Html
button = normalTag Html.button


canvas :: [Attribute] -> [Html] -> Html
canvas = normalTag Html.canvas


caption :: [Attribute] -> [Html] -> Html
caption = normalTag Html.caption


cite :: [Attribute] -> [Html] -> Html
cite = normalTag Html.cite


code :: [Attribute] -> [Html] -> Html
code = normalTag Html.code


col :: [Attribute] -> Html
col = voidTag Html.col


colgroup :: [Attribute] -> [Html] -> Html
colgroup = normalTag Html.colgroup


command :: [Attribute] -> [Html] -> Html
command = normalTag Html.command


datalist :: [Attribute] -> [Html] -> Html
datalist = normalTag Html.datalist


dd :: [Attribute] -> [Html] -> Html
dd = normalTag Html.dd


del :: [Attribute] -> [Html] -> Html
del = normalTag Html.del


details :: [Attribute] -> [Html] -> Html
details = normalTag Html.details


dfn :: [Attribute] -> [Html] -> Html
dfn = normalTag Html.dfn


div :: [Attribute] -> [Html] -> Html
div = normalTag Html.div


dl :: [Attribute] -> [Html] -> Html
dl = normalTag Html.dl


dt :: [Attribute] -> [Html] -> Html
dt = normalTag Html.dt


em :: [Attribute] -> [Html] -> Html
em = normalTag Html.em


embed :: [Attribute] -> Html
embed = voidTag Html.embed


fieldset :: [Attribute] -> [Html] -> Html
fieldset = normalTag Html.fieldset


figcaption :: [Attribute] -> [Html] -> Html
figcaption = normalTag Html.figcaption


figure :: [Attribute] -> [Html] -> Html
figure = normalTag Html.figure


footer :: [Attribute] -> [Html] -> Html
footer = normalTag Html.footer


form :: [Attribute] -> [Html] -> Html
form = normalTag Html.form


h1 :: [Attribute] -> [Html] -> Html
h1 = normalTag Html.h1


h2 :: [Attribute] -> [Html] -> Html
h2 = normalTag Html.h2


h3 :: [Attribute] -> [Html] -> Html
h3 = normalTag Html.h3


h4 :: [Attribute] -> [Html] -> Html
h4 = normalTag Html.h4


h5 :: [Attribute] -> [Html] -> Html
h5 = normalTag Html.h5


h6 :: [Attribute] -> [Html] -> Html
h6 = normalTag Html.h6


head :: [Attribute] -> [Html] -> Html
head = normalTag Html.head


header :: [Attribute] -> [Html] -> Html
header = normalTag Html.header


hgroup :: [Attribute] -> [Html] -> Html
hgroup = normalTag Html.hgroup


hr :: [Attribute] -> Html
hr = voidTag Html.hr


html :: [Attribute] -> [Html] -> Html
html = normalTag Html.html


i :: [Attribute] -> [Html] -> Html
i = normalTag Html.i


iframe :: [Attribute] -> [Html] -> Html
iframe = normalTag Html.iframe


img :: [Attribute] -> Html
img = voidTag Html.img


input :: [Attribute] -> Html
input = voidTag Html.input


ins :: [Attribute] -> [Html] -> Html
ins = normalTag Html.ins


kbd :: [Attribute] -> [Html] -> Html
kbd = normalTag Html.kbd


keygen :: [Attribute] -> Html
keygen = voidTag Html.keygen


label :: [Attribute] -> [Html] -> Html
label = normalTag Html.label


legend :: [Attribute] -> [Html] -> Html
legend = normalTag Html.legend


li :: [Attribute] -> [Html] -> Html
li = normalTag Html.li


link :: [Attribute] -> Html
link = voidTag Html.link


main :: [Attribute] -> [Html] -> Html
main = normalTag Html.main


map :: [Attribute] -> [Html] -> Html
map = normalTag Html.map


mark :: [Attribute] -> [Html] -> Html
mark = normalTag Html.mark


menu :: [Attribute] -> [Html] -> Html
menu = normalTag Html.menu


menuitem :: [Attribute] -> Html
menuitem = voidTag Html.menuitem


meta :: [Attribute] -> Html
meta = voidTag Html.meta


meter :: [Attribute] -> [Html] -> Html
meter = normalTag Html.meter


nav :: [Attribute] -> [Html] -> Html
nav = normalTag Html.nav


noscript :: [Attribute] -> [Html] -> Html
noscript = normalTag Html.noscript


object :: [Attribute] -> [Html] -> Html
object = normalTag Html.object


ol :: [Attribute] -> [Html] -> Html
ol = normalTag Html.ol


optgroup :: [Attribute] -> [Html] -> Html
optgroup = normalTag Html.optgroup


option :: [Attribute] -> [Html] -> Html
option = normalTag Html.option


output :: [Attribute] -> [Html] -> Html
output = normalTag Html.output


p :: [Attribute] -> [Html] -> Html
p = normalTag Html.p


param :: [Attribute] -> Html
param = voidTag Html.param


pre :: [Attribute] -> [Html] -> Html
pre = normalTag Html.pre


progress :: [Attribute] -> [Html] -> Html
progress = normalTag Html.progress


q :: [Attribute] -> [Html] -> Html
q = normalTag Html.q


rp :: [Attribute] -> [Html] -> Html
rp = normalTag Html.rp


rt :: [Attribute] -> [Html] -> Html
rt = normalTag Html.rt


ruby :: [Attribute] -> [Html] -> Html
ruby = normalTag Html.ruby


samp :: [Attribute] -> [Html] -> Html
samp = normalTag Html.samp


script :: [Attribute] -> [Html] -> Html
script = normalTag Html.script


section :: [Attribute] -> [Html] -> Html
section = normalTag Html.section


select :: [Attribute] -> [Html] -> Html
select = normalTag Html.select


small :: [Attribute] -> [Html] -> Html
small = normalTag Html.small


source :: [Attribute] -> Html
source = voidTag Html.source


span :: [Attribute] -> [Html] -> Html
span = normalTag Html.span


strong :: [Attribute] -> [Html] -> Html
strong = normalTag Html.strong


style :: [Attribute] -> [Html] -> Html
style = normalTag Html.style


sub :: [Attribute] -> [Html] -> Html
sub = normalTag Html.sub


summary :: [Attribute] -> [Html] -> Html
summary = normalTag Html.summary


sup :: [Attribute] -> [Html] -> Html
sup = normalTag Html.sup


table :: [Attribute] -> [Html] -> Html
table = normalTag Html.table


tbody :: [Attribute] -> [Html] -> Html
tbody = normalTag Html.tbody


td :: [Attribute] -> [Html] -> Html
td = normalTag Html.td


textarea :: [Attribute] -> [Html] -> Html
textarea = normalTag Html.textarea


tfoot :: [Attribute] -> [Html] -> Html
tfoot = normalTag Html.tfoot


th :: [Attribute] -> [Html] -> Html
th = normalTag Html.th


thead :: [Attribute] -> [Html] -> Html
thead = normalTag Html.thead


time :: [Attribute] -> [Html] -> Html
time = normalTag Html.time


title :: [Attribute] -> [Html] -> Html
title = normalTag Html.title


tr :: [Attribute] -> [Html] -> Html
tr = normalTag Html.tr


track :: [Attribute] -> Html
track = voidTag Html.track


u :: [Attribute] -> [Html] -> Html
u = normalTag Html.u


ul :: [Attribute] -> [Html] -> Html
ul = normalTag Html.ul


var :: [Attribute] -> [Html] -> Html
var = normalTag Html.var


video :: [Attribute] -> [Html] -> Html
video = normalTag Html.video


wbr :: [Attribute] -> Html
wbr = voidTag Html.wbr
