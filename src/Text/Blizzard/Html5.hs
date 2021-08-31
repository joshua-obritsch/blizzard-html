{-# LANGUAGE NoImplicitPrelude #-}

module Text.Blizzard.Html5
    ( module Text.Blaze.Html
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


import Text.Blaze.Html
import Text.Blizzard.Html

import qualified Text.Blaze.Html5 as H


docType :: Html
docType = H.docType

docTypeHtml :: [Html] -> Html
docTypeHtml = documentTag H.docTypeHtml

a :: [Attribute] -> [Html] -> Html
a = normalTag H.a

abbr :: [Attribute] -> [Html] -> Html
abbr = normalTag H.abbr

address :: [Attribute] -> [Html] -> Html
address = normalTag H.address

area :: [Attribute] -> Html
area = voidTag H.area

article :: [Attribute] -> [Html] -> Html
article = normalTag H.article

aside :: [Attribute] -> [Html] -> Html
aside = normalTag H.aside

audio :: [Attribute] -> [Html] -> Html
audio = normalTag H.audio

b :: [Attribute] -> [Html] -> Html
b = normalTag H.b

base :: [Attribute] -> Html
base = voidTag H.base

bdo :: [Attribute] -> [Html] -> Html
bdo = normalTag H.bdo

blockquote :: [Attribute] -> [Html] -> Html
blockquote = normalTag H.blockquote

body :: [Attribute] -> [Html] -> Html
body = normalTag H.body

br :: [Attribute] -> Html
br = voidTag H.br

button :: [Attribute] -> [Html] -> Html
button = normalTag H.button

canvas :: [Attribute] -> [Html] -> Html
canvas = normalTag H.canvas

caption :: [Attribute] -> [Html] -> Html
caption = normalTag H.caption

cite :: [Attribute] -> [Html] -> Html
cite = normalTag H.cite

code :: [Attribute] -> [Html] -> Html
code = normalTag H.code

col :: [Attribute] -> Html
col = voidTag H.col

colgroup :: [Attribute] -> [Html] -> Html
colgroup = normalTag H.colgroup

command :: [Attribute] -> [Html] -> Html
command = normalTag H.command

datalist :: [Attribute] -> [Html] -> Html
datalist = normalTag H.datalist

dd :: [Attribute] -> [Html] -> Html
dd = normalTag H.dd

del :: [Attribute] -> [Html] -> Html
del = normalTag H.del

details :: [Attribute] -> [Html] -> Html
details = normalTag H.details

dfn :: [Attribute] -> [Html] -> Html
dfn = normalTag H.dfn

div :: [Attribute] -> [Html] -> Html
div = normalTag H.div

dl :: [Attribute] -> [Html] -> Html
dl = normalTag H.dl

dt :: [Attribute] -> [Html] -> Html
dt = normalTag H.dt

em :: [Attribute] -> [Html] -> Html
em = normalTag H.em

embed :: [Attribute] -> Html
embed = voidTag H.embed

fieldset :: [Attribute] -> [Html] -> Html
fieldset = normalTag H.fieldset

figcaption :: [Attribute] -> [Html] -> Html
figcaption = normalTag H.figcaption

figure :: [Attribute] -> [Html] -> Html
figure = normalTag H.figure

footer :: [Attribute] -> [Html] -> Html
footer = normalTag H.footer

form :: [Attribute] -> [Html] -> Html
form = normalTag H.form

h1 :: [Attribute] -> [Html] -> Html
h1 = normalTag H.h1

h2 :: [Attribute] -> [Html] -> Html
h2 = normalTag H.h2

h3 :: [Attribute] -> [Html] -> Html
h3 = normalTag H.h3

h4 :: [Attribute] -> [Html] -> Html
h4 = normalTag H.h4

h5 :: [Attribute] -> [Html] -> Html
h5 = normalTag H.h5

h6 :: [Attribute] -> [Html] -> Html
h6 = normalTag H.h6

head :: [Attribute] -> [Html] -> Html
head = normalTag H.head

header :: [Attribute] -> [Html] -> Html
header = normalTag H.header

hgroup :: [Attribute] -> [Html] -> Html
hgroup = normalTag H.hgroup

hr :: [Attribute] -> Html
hr = voidTag H.hr

html :: [Attribute] -> [Html] -> Html
html = normalTag H.html

i :: [Attribute] -> [Html] -> Html
i = normalTag H.i

iframe :: [Attribute] -> [Html] -> Html
iframe = normalTag H.iframe

img :: [Attribute] -> Html
img = voidTag H.img

input :: [Attribute] -> Html
input = voidTag H.input

ins :: [Attribute] -> [Html] -> Html
ins = normalTag H.ins

kbd :: [Attribute] -> [Html] -> Html
kbd = normalTag H.kbd

keygen :: [Attribute] -> Html
keygen = voidTag H.keygen

label :: [Attribute] -> [Html] -> Html
label = normalTag H.label

legend :: [Attribute] -> [Html] -> Html
legend = normalTag H.legend

li :: [Attribute] -> [Html] -> Html
li = normalTag H.li

link :: [Attribute] -> Html
link = voidTag H.link

main :: [Attribute] -> [Html] -> Html
main = normalTag H.main

map :: [Attribute] -> [Html] -> Html
map = normalTag H.map

mark :: [Attribute] -> [Html] -> Html
mark = normalTag H.mark

menu :: [Attribute] -> [Html] -> Html
menu = normalTag H.menu

menuitem :: [Attribute] -> Html
menuitem = voidTag H.menuitem

meta :: [Attribute] -> Html
meta = voidTag H.meta

meter :: [Attribute] -> [Html] -> Html
meter = normalTag H.meter

nav :: [Attribute] -> [Html] -> Html
nav = normalTag H.nav

noscript :: [Attribute] -> [Html] -> Html
noscript = normalTag H.noscript

object :: [Attribute] -> [Html] -> Html
object = normalTag H.object

ol :: [Attribute] -> [Html] -> Html
ol = normalTag H.ol

optgroup :: [Attribute] -> [Html] -> Html
optgroup = normalTag H.optgroup

option :: [Attribute] -> [Html] -> Html
option = normalTag H.option

output :: [Attribute] -> [Html] -> Html
output = normalTag H.output

p :: [Attribute] -> [Html] -> Html
p = normalTag H.p

param :: [Attribute] -> Html
param = voidTag H.param

pre :: [Attribute] -> [Html] -> Html
pre = normalTag H.pre

progress :: [Attribute] -> [Html] -> Html
progress = normalTag H.progress

q :: [Attribute] -> [Html] -> Html
q = normalTag H.q

rp :: [Attribute] -> [Html] -> Html
rp = normalTag H.rp

rt :: [Attribute] -> [Html] -> Html
rt = normalTag H.rt

ruby :: [Attribute] -> [Html] -> Html
ruby = normalTag H.ruby

samp :: [Attribute] -> [Html] -> Html
samp = normalTag H.samp

script :: [Attribute] -> [Html] -> Html
script = normalTag H.script

section :: [Attribute] -> [Html] -> Html
section = normalTag H.section

select :: [Attribute] -> [Html] -> Html
select = normalTag H.select

small :: [Attribute] -> [Html] -> Html
small = normalTag H.small

source :: [Attribute] -> Html
source = voidTag H.source

span :: [Attribute] -> [Html] -> Html
span = normalTag H.span

strong :: [Attribute] -> [Html] -> Html
strong = normalTag H.strong

style :: [Attribute] -> [Html] -> Html
style = normalTag H.style

sub :: [Attribute] -> [Html] -> Html
sub = normalTag H.sub

summary :: [Attribute] -> [Html] -> Html
summary = normalTag H.summary

sup :: [Attribute] -> [Html] -> Html
sup = normalTag H.sup

table :: [Attribute] -> [Html] -> Html
table = normalTag H.table

tbody :: [Attribute] -> [Html] -> Html
tbody = normalTag H.tbody

td :: [Attribute] -> [Html] -> Html
td = normalTag H.td

textarea :: [Attribute] -> [Html] -> Html
textarea = normalTag H.textarea

tfoot :: [Attribute] -> [Html] -> Html
tfoot = normalTag H.tfoot

th :: [Attribute] -> [Html] -> Html
th = normalTag H.th

thead :: [Attribute] -> [Html] -> Html
thead = normalTag H.thead

time :: [Attribute] -> [Html] -> Html
time = normalTag H.time

title :: [Attribute] -> [Html] -> Html
title = normalTag H.title

tr :: [Attribute] -> [Html] -> Html
tr = normalTag H.tr

track :: [Attribute] -> Html
track = voidTag H.track

u :: [Attribute] -> [Html] -> Html
u = normalTag H.u

ul :: [Attribute] -> [Html] -> Html
ul = normalTag H.ul

var :: [Attribute] -> [Html] -> Html
var = normalTag H.var

video :: [Attribute] -> [Html] -> Html
video = normalTag H.video

wbr :: [Attribute] -> Html
wbr = voidTag H.wbr
