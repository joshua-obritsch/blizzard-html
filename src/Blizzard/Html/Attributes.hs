{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Blizzard.Html.Attributes
    ( accept
    , acceptCharset
    , accesskey
    , action
    , allowfullscreen
    , alt
    , async
    , autocomplete
    , autofocus
    , autoplay
    , challenge
    , charset
    , checked
    , cite
    , css
    , cols
    , colspan
    , content
    , contenteditable
    , contextmenu
    , controls
    , coords
    , data_
    , datetime
    , default_
    , defer
    , dir
    , disabled
    , draggable
    , enctype
    , for
    , form
    , formaction
    , formenctype
    , formmethod
    , formnovalidate
    , formtarget
    , headers
    , height
    , hidden
    , high
    , href
    , hreflang
    , httpEquiv
    , icon
    , id
    , inert
    , ismap
    , item
    , itemprop
    , itemscope
    , itemtype
    , keytype
    , label
    , lang
    , list
    , loop
    , low
    , manifest
    , max
    , maxlength
    , media
    , method
    , min
    , multiple
    , muted
    , name
    , nomodule
    , novalidate
    , onbeforeonload
    , onbeforeprint
    , onblur
    , oncanplay
    , oncanplaythrough
    , onchange
    , onclick
    , oncontextmenu
    , ondblclick
    , ondrag
    , ondragend
    , ondragenter
    , ondragleave
    , ondragover
    , ondragstart
    , ondrop
    , ondurationchange
    , onemptied
    , onended
    , onerror
    , onfocus
    , onformchange
    , onforminput
    , onhaschange
    , oninput
    , oninvalid
    , onkeydown
    , onkeyup
    , onload
    , onloadeddata
    , onloadedmetadata
    , onloadstart
    , onmessage
    , onmousedown
    , onmousemove
    , onmouseout
    , onmouseover
    , onmouseup
    , onmousewheel
    , ononline
    , onpagehide
    , onpageshow
    , onpause
    , onplay
    , onplaying
    , onprogress
    , onpropstate
    , onratechange
    , onreadystatechange
    , onredo
    , onresize
    , onscroll
    , onseeked
    , onseeking
    , onselect
    , onstalled
    , onstorage
    , onsubmit
    , onsuspend
    , ontimeupdate
    , onundo
    , onunload
    , onvolumechange
    , onwaiting
    , open
    , optimum
    , pattern
    , ping
    , placeholder
    , playsinline
    , preload
    , pubdate
    , radiogroup
    , readonly
    , rel
    , required
    , reversed
    , role
    , rows
    , rowspan
    , sandbox
    , scope
    , scoped
    , seamless
    , selected
    , shape
    , size
    , sizes
    , span
    , spellcheck
    , src
    , srcdoc
    , start
    , step
    , style
    , subject
    , summary
    , tabindex
    , target
    , title
    , truespeed
    , type_
    , typemustmatch
    , usemap
    , value
    , width
    , wrap
    , xmlns
    ) where


import Data.Text (Text, unwords)
import Prelude hiding (id, max, min, span, unwords)
import Text.Blaze.Html (Attribute, customAttribute, textValue)

import qualified Text.Blaze.Html5.Attributes as Attr


accept :: Bool -> Maybe Attribute
accept True  = Just $ Attr.accept "accept"
accept False = Nothing


acceptCharset :: Text -> Maybe Attribute
acceptCharset = Just . Attr.acceptCharset . textValue


accesskey :: Text -> Maybe Attribute
accesskey = Just . Attr.accesskey . textValue


action :: Text -> Maybe Attribute
action = Just . Attr.action . textValue


allowfullscreen :: Bool -> Maybe Attribute
allowfullscreen True  = Just $ customAttribute "allowfullscreen" "allowfullscreen"
allowfullscreen False = Nothing


alt :: Text -> Maybe Attribute
alt = Just . Attr.alt . textValue


async :: Bool -> Maybe Attribute
async True  = Just $ Attr.async "async"
async False = Nothing


autocomplete :: Text -> Maybe Attribute
autocomplete = Just . Attr.autocomplete . textValue


autofocus :: Bool -> Maybe Attribute
autofocus True  = Just $ Attr.autofocus "autofocus"
autofocus False = Nothing


autoplay :: Bool -> Maybe Attribute
autoplay True  = Just $ Attr.autoplay "autoplay"
autoplay False = Nothing


challenge :: Text -> Maybe Attribute
challenge = Just . Attr.challenge . textValue


charset :: Text -> Maybe Attribute
charset = Just . Attr.charset . textValue


checked :: Bool -> Maybe Attribute
checked True  = Just $ Attr.checked "checked"
checked False = Nothing


cite :: Text -> Maybe Attribute
cite = Just . Attr.cite . textValue


css :: [Text] -> Maybe Attribute
css = Just . Attr.class_ . textValue . unwords


cols :: Text -> Maybe Attribute
cols = Just . Attr.cols . textValue


colspan :: Text -> Maybe Attribute
colspan = Just . Attr.colspan . textValue


content :: Text -> Maybe Attribute
content = Just . Attr.content . textValue


contenteditable :: Text -> Maybe Attribute
contenteditable = Just . Attr.contenteditable . textValue


contextmenu :: Text -> Maybe Attribute
contextmenu = Just . Attr.contextmenu . textValue


controls :: Bool -> Maybe Attribute
controls True  = Just $ Attr.controls "controls"
controls False = Nothing


coords :: Text -> Maybe Attribute
coords = Just . Attr.coords . textValue


data_ :: Text -> Maybe Attribute
data_ = Just . Attr.data_ . textValue


datetime :: Text -> Maybe Attribute
datetime = Just . Attr.datetime . textValue


default_ :: Bool -> Maybe Attribute
default_ True  = Just $ customAttribute "default" "default"
default_ False = Nothing


defer :: Bool -> Maybe Attribute
defer True  = Just $ Attr.defer "defer"
defer False = Nothing


dir :: Text -> Maybe Attribute
dir = Just . Attr.dir . textValue


disabled :: Bool -> Maybe Attribute
disabled True  = Just $ Attr.disabled "disabled"
disabled False = Nothing


draggable :: Text -> Maybe Attribute
draggable = Just . Attr.draggable . textValue


enctype :: Text -> Maybe Attribute
enctype = Just . Attr.enctype . textValue


for :: Text -> Maybe Attribute
for = Just . Attr.for . textValue


form :: Text -> Maybe Attribute
form = Just . Attr.form . textValue


formaction :: Text -> Maybe Attribute
formaction = Just . Attr.formaction . textValue


formenctype :: Text -> Maybe Attribute
formenctype = Just . Attr.formenctype . textValue


formmethod :: Text -> Maybe Attribute
formmethod = Just . Attr.formmethod . textValue


formnovalidate :: Bool -> Maybe Attribute
formnovalidate True  = Just $ Attr.formnovalidate "formnovalidate"
formnovalidate False = Nothing


formtarget :: Text -> Maybe Attribute
formtarget = Just . Attr.formtarget . textValue


headers :: Text -> Maybe Attribute
headers = Just . Attr.headers . textValue


height :: Text -> Maybe Attribute
height = Just . Attr.height . textValue


hidden :: Bool -> Maybe Attribute
hidden True  = Just $ Attr.hidden "hidden"
hidden False = Nothing


high :: Text -> Maybe Attribute
high = Just . Attr.high . textValue


href :: Text -> Maybe Attribute
href = Just . Attr.href . textValue


hreflang :: Text -> Maybe Attribute
hreflang = Just . Attr.hreflang . textValue


httpEquiv :: Text -> Maybe Attribute
httpEquiv = Just . Attr.httpEquiv . textValue


icon :: Text -> Maybe Attribute
icon = Just . Attr.icon . textValue


id :: Text -> Maybe Attribute
id = Just . Attr.id . textValue


inert :: Bool -> Maybe Attribute
inert True  = Just $ customAttribute "inert" "inert"
inert False = Nothing


ismap :: Bool -> Maybe Attribute
ismap True  = Just $ Attr.ismap "ismap"
ismap False = Nothing


item :: Text -> Maybe Attribute
item = Just . Attr.item . textValue


itemprop :: Text -> Maybe Attribute
itemprop = Just . Attr.itemprop . textValue


itemscope :: Bool -> Maybe Attribute
itemscope True  = Just $ Attr.itemscope "itemscope"
itemscope False = Nothing


itemtype :: Text -> Maybe Attribute
itemtype = Just . Attr.itemtype . textValue


keytype :: Text -> Maybe Attribute
keytype = Just . Attr.keytype . textValue


label :: Text -> Maybe Attribute
label = Just . Attr.label . textValue


lang :: Text -> Maybe Attribute
lang = Just . Attr.lang . textValue


list :: Text -> Maybe Attribute
list = Just . Attr.list . textValue


loop :: Bool -> Maybe Attribute
loop True  = Just $ Attr.loop "loop"
loop False = Nothing


low :: Text -> Maybe Attribute
low = Just . Attr.low . textValue


manifest :: Text -> Maybe Attribute
manifest = Just . Attr.manifest . textValue


max :: Text -> Maybe Attribute
max = Just . Attr.max . textValue


maxlength :: Text -> Maybe Attribute
maxlength = Just . Attr.maxlength . textValue


media :: Text -> Maybe Attribute
media = Just . Attr.media . textValue


method :: Text -> Maybe Attribute
method = Just . Attr.method . textValue


min :: Text -> Maybe Attribute
min = Just . Attr.min . textValue


multiple :: Bool -> Maybe Attribute
multiple True  = Just $ Attr.multiple "multiple"
multiple False = Nothing


muted :: Bool -> Maybe Attribute
muted True  = Just $ customAttribute "muted" "muted"
muted False = Nothing


name :: Text -> Maybe Attribute
name = Just . Attr.name . textValue


nomodule :: Bool -> Maybe Attribute
nomodule True  = Just $ customAttribute "nomodule" "nomodule"
nomodule False = Nothing


novalidate :: Bool -> Maybe Attribute
novalidate True  = Just $ Attr.novalidate "novalidate"
novalidate False = Nothing


onbeforeonload :: Text -> Maybe Attribute
onbeforeonload = Just . Attr.onbeforeonload . textValue


onbeforeprint :: Text -> Maybe Attribute
onbeforeprint = Just . Attr.onbeforeprint . textValue


onblur :: Text -> Maybe Attribute
onblur = Just . Attr.onblur . textValue


oncanplay :: Text -> Maybe Attribute
oncanplay = Just . Attr.oncanplay . textValue


oncanplaythrough :: Text -> Maybe Attribute
oncanplaythrough = Just . Attr.oncanplaythrough . textValue


onchange :: Text -> Maybe Attribute
onchange = Just . Attr.onchange . textValue


onclick :: Text -> Maybe Attribute
onclick = Just . Attr.onclick . textValue


oncontextmenu :: Text -> Maybe Attribute
oncontextmenu = Just . Attr.oncontextmenu . textValue


ondblclick :: Text -> Maybe Attribute
ondblclick = Just . Attr.ondblclick . textValue


ondrag :: Text -> Maybe Attribute
ondrag = Just . Attr.ondrag . textValue


ondragend :: Text -> Maybe Attribute
ondragend = Just . Attr.ondragend . textValue


ondragenter :: Text -> Maybe Attribute
ondragenter = Just . Attr.ondragenter . textValue


ondragleave :: Text -> Maybe Attribute
ondragleave = Just . Attr.ondragleave . textValue


ondragover :: Text -> Maybe Attribute
ondragover = Just . Attr.ondragover . textValue


ondragstart :: Text -> Maybe Attribute
ondragstart = Just . Attr.ondragstart . textValue


ondrop :: Text -> Maybe Attribute
ondrop = Just . Attr.ondrop . textValue


ondurationchange :: Text -> Maybe Attribute
ondurationchange = Just . Attr.ondurationchange . textValue


onemptied :: Text -> Maybe Attribute
onemptied = Just . Attr.onemptied . textValue


onended :: Text -> Maybe Attribute
onended = Just . Attr.onended . textValue


onerror :: Text -> Maybe Attribute
onerror = Just . Attr.onerror . textValue


onfocus :: Text -> Maybe Attribute
onfocus = Just . Attr.onfocus . textValue


onformchange :: Text -> Maybe Attribute
onformchange = Just . Attr.onformchange . textValue


onforminput :: Text -> Maybe Attribute
onforminput = Just . Attr.onforminput . textValue


onhaschange :: Text -> Maybe Attribute
onhaschange = Just . Attr.onhaschange . textValue


oninput :: Text -> Maybe Attribute
oninput = Just . Attr.oninput . textValue


oninvalid :: Text -> Maybe Attribute
oninvalid = Just . Attr.oninvalid . textValue


onkeydown :: Text -> Maybe Attribute
onkeydown = Just . Attr.onkeydown . textValue


onkeyup :: Text -> Maybe Attribute
onkeyup = Just . Attr.onkeyup . textValue


onload :: Text -> Maybe Attribute
onload = Just . Attr.onload . textValue


onloadeddata :: Text -> Maybe Attribute
onloadeddata = Just . Attr.onloadeddata . textValue


onloadedmetadata :: Text -> Maybe Attribute
onloadedmetadata = Just . Attr.onloadedmetadata . textValue


onloadstart :: Text -> Maybe Attribute
onloadstart = Just . Attr.onloadstart . textValue


onmessage :: Text -> Maybe Attribute
onmessage = Just . Attr.onmessage . textValue


onmousedown :: Text -> Maybe Attribute
onmousedown = Just . Attr.onmousedown . textValue


onmousemove :: Text -> Maybe Attribute
onmousemove = Just . Attr.onmousemove . textValue


onmouseout :: Text -> Maybe Attribute
onmouseout = Just . Attr.onmouseout . textValue


onmouseover :: Text -> Maybe Attribute
onmouseover = Just . Attr.onmouseover . textValue


onmouseup :: Text -> Maybe Attribute
onmouseup = Just . Attr.onmouseup . textValue


onmousewheel :: Text -> Maybe Attribute
onmousewheel = Just . Attr.onmousewheel . textValue


ononline :: Text -> Maybe Attribute
ononline = Just . Attr.ononline . textValue


onpagehide :: Text -> Maybe Attribute
onpagehide = Just . Attr.onpagehide . textValue


onpageshow :: Text -> Maybe Attribute
onpageshow = Just . Attr.onpageshow . textValue


onpause :: Text -> Maybe Attribute
onpause = Just . Attr.onpause . textValue


onplay :: Text -> Maybe Attribute
onplay = Just . Attr.onplay . textValue


onplaying :: Text -> Maybe Attribute
onplaying = Just . Attr.onplaying . textValue


onprogress :: Text -> Maybe Attribute
onprogress = Just . Attr.onprogress . textValue


onpropstate :: Text -> Maybe Attribute
onpropstate = Just . Attr.onpropstate . textValue


onratechange :: Text -> Maybe Attribute
onratechange = Just . Attr.onratechange . textValue


onreadystatechange :: Text -> Maybe Attribute
onreadystatechange = Just . Attr.onreadystatechange . textValue


onredo :: Text -> Maybe Attribute
onredo = Just . Attr.onredo . textValue


onresize :: Text -> Maybe Attribute
onresize = Just . Attr.onresize . textValue


onscroll :: Text -> Maybe Attribute
onscroll = Just . Attr.onscroll . textValue


onseeked :: Text -> Maybe Attribute
onseeked = Just . Attr.onseeked . textValue


onseeking :: Text -> Maybe Attribute
onseeking = Just . Attr.onseeking . textValue


onselect :: Text -> Maybe Attribute
onselect = Just . Attr.onselect . textValue


onstalled :: Text -> Maybe Attribute
onstalled = Just . Attr.onstalled . textValue


onstorage :: Text -> Maybe Attribute
onstorage = Just . Attr.onstorage . textValue


onsubmit :: Text -> Maybe Attribute
onsubmit = Just . Attr.onsubmit . textValue


onsuspend :: Text -> Maybe Attribute
onsuspend = Just . Attr.onsuspend . textValue


ontimeupdate :: Text -> Maybe Attribute
ontimeupdate = Just . Attr.ontimeupdate . textValue


onundo :: Text -> Maybe Attribute
onundo = Just . Attr.onundo . textValue


onunload :: Text -> Maybe Attribute
onunload = Just . Attr.onunload . textValue


onvolumechange :: Text -> Maybe Attribute
onvolumechange = Just . Attr.onvolumechange . textValue


onwaiting :: Text -> Maybe Attribute
onwaiting = Just . Attr.onwaiting . textValue


open :: Bool -> Maybe Attribute
open True  = Just $ Attr.open "open"
open False = Nothing


optimum :: Text -> Maybe Attribute
optimum = Just . Attr.optimum . textValue


pattern :: Text -> Maybe Attribute
pattern = Just . Attr.pattern . textValue


ping :: Text -> Maybe Attribute
ping = Just . Attr.ping . textValue


placeholder :: Text -> Maybe Attribute
placeholder = Just . Attr.placeholder . textValue


playsinline :: Bool -> Maybe Attribute
playsinline True  = Just $ customAttribute "playsinline" "playsinline"
playsinline False = Nothing


preload :: Text -> Maybe Attribute
preload = Just . Attr.preload . textValue


pubdate :: Text -> Maybe Attribute
pubdate = Just . Attr.pubdate . textValue


radiogroup :: Text -> Maybe Attribute
radiogroup = Just . Attr.radiogroup . textValue


readonly :: Bool -> Maybe Attribute
readonly True  = Just $ Attr.readonly "readonly"
readonly False = Nothing


rel :: Text -> Maybe Attribute
rel = Just . Attr.rel . textValue


required :: Bool -> Maybe Attribute
required True  = Just $ Attr.required "required"
required False = Nothing


reversed :: Bool -> Maybe Attribute
reversed True  = Just $ Attr.reversed "reversed"
reversed False = Nothing


role :: Text -> Maybe Attribute
role = Just . Attr.role . textValue


rows :: Text -> Maybe Attribute
rows = Just . Attr.rows . textValue


rowspan :: Text -> Maybe Attribute
rowspan = Just . Attr.rowspan . textValue


sandbox :: Text -> Maybe Attribute
sandbox = Just . Attr.sandbox . textValue


scope :: Text -> Maybe Attribute
scope = Just . Attr.scope . textValue


scoped :: Bool -> Maybe Attribute
scoped True  = Just $ Attr.scoped "scoped"
scoped False = Nothing


seamless :: Bool -> Maybe Attribute
seamless True  = Just $ Attr.seamless "seamless"
seamless False = Nothing


selected :: Bool -> Maybe Attribute
selected True  = Just $ Attr.selected "selected"
selected False = Nothing


shape :: Text -> Maybe Attribute
shape = Just . Attr.shape . textValue


size :: Text -> Maybe Attribute
size = Just . Attr.size . textValue


sizes :: Text -> Maybe Attribute
sizes = Just . Attr.sizes . textValue


span :: Text -> Maybe Attribute
span = Just . Attr.span . textValue


spellcheck :: Text -> Maybe Attribute
spellcheck = Just . Attr.spellcheck . textValue


src :: Text -> Maybe Attribute
src = Just . Attr.src . textValue


srcdoc :: Text -> Maybe Attribute
srcdoc = Just . Attr.srcdoc . textValue


start :: Text -> Maybe Attribute
start = Just . Attr.start . textValue


step :: Text -> Maybe Attribute
step = Just . Attr.step . textValue


style :: [Text] -> Maybe Attribute
style = Just . Attr.style . textValue . unwords


subject :: Text -> Maybe Attribute
subject = Just . Attr.subject . textValue


summary :: Text -> Maybe Attribute
summary = Just . Attr.summary . textValue


tabindex :: Text -> Maybe Attribute
tabindex = Just . Attr.tabindex . textValue


target :: Text -> Maybe Attribute
target = Just . Attr.target . textValue


title :: Text -> Maybe Attribute
title = Just . Attr.title . textValue


truespeed :: Bool -> Maybe Attribute
truespeed True  = Just $ customAttribute "truespeed" "truespeed"
truespeed False = Nothing


type_ :: Text -> Maybe Attribute
type_ = Just . Attr.type_ . textValue


typemustmatch :: Bool -> Maybe Attribute
typemustmatch True  = Just $ customAttribute "typemustmatch" "typemustmatch"
typemustmatch False = Nothing


usemap :: Text -> Maybe Attribute
usemap = Just . Attr.usemap . textValue


value :: Text -> Maybe Attribute
value = Just . Attr.value . textValue


width :: Text -> Maybe Attribute
width = Just . Attr.width . textValue


wrap :: Text -> Maybe Attribute
wrap = Just . Attr.wrap . textValue


xmlns :: Text -> Maybe Attribute
xmlns = Just . Attr.xmlns . textValue
