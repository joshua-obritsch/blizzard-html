{-# LANGUAGE OverloadedStrings #-}

module Blizzard.Html.Attributes
    ( abbr
    , accept
    , acceptCharset
    , accesskey
    , action
    , allow
    , allowfullscreen
    , alt
    , as
    , async
    , autocapitalize
    , autocomplete
    , autofocus
    , autoplay
    , blocking
    , charset
    , checked
    , cite
    , class_
    , color
    , cols
    , colspan
    , content
    , contenteditable
    , controls
    , coords
    , crossorigin
    , data_
    , datetime
    , decoding
    , default_
    , defer
    , dir
    , disabled
    , download
    , draggable
    , enctype
    , enterkeyhint
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
    , id
    , imagesizes
    , imagesrcset
    , inert
    , inputmode
    , integrity
    , is
    , ismap
    , itemid
    , itemprop
    , itemref
    , itemscope
    , itemtype
    , kind
    , label
    , lang
    , list
    , loading
    , loop
    , low
    , max
    , maxlength
    , media
    , method
    , min
    , minlength
    , multiple
    , muted
    , name
    , nomodule
    , nonce
    , novalidate
    , open
    , optimum
    , pattern
    , ping
    , placeholder
    , playsinline
    , poster
    , preload
    , readonly
    , referrerpolicy
    , rel
    , required
    , reversed
    , rows
    , rowspan
    , sandbox
    , scope
    , selected
    , shape
    , size
    , sizes
    , slot
    , span
    , spellcheck
    , src
    , srcdoc
    , srclang
    , srcset
    , start
    , step
    , style
    , tabindex
    , target
    , title
    , translate
    , type_
    , usemap
    , value
    , width
    , wrap

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

    , css
    ) where


import Data.Text (Text, unwords)
import Prelude hiding (id, max, min, span, unwords)
import Text.Blaze.Html (Attribute, customAttribute, textValue)

import qualified Text.Blaze.Html5.Attributes as Attr

import Blizzard.Internal.Html (boolAttribute, textAttribute)


abbr :: Text -> Maybe Attribute
abbr = textAttribute $ customAttribute "abbr"


accept :: Text -> Maybe Attribute
accept = textAttribute Attr.accept


acceptCharset :: Text -> Maybe Attribute
acceptCharset = textAttribute Attr.acceptCharset


accesskey :: Text -> Maybe Attribute
accesskey = textAttribute Attr.accesskey


action :: Text -> Maybe Attribute
action = textAttribute Attr.action


allow :: Text -> Maybe Attribute
allow = textAttribute $ customAttribute "allow"


allowfullscreen :: Bool -> Maybe Attribute
allowfullscreen = boolAttribute $ customAttribute "allowfullscreen" "allowfullscreen"


alt :: Text -> Maybe Attribute
alt = textAttribute Attr.alt


as :: Text -> Maybe Attribute
as = textAttribute $ customAttribute "as"


async :: Bool -> Maybe Attribute
async = boolAttribute $ Attr.async "async"


autocapitalize :: Text -> Maybe Attribute
autocapitalize = textAttribute $ customAttribute "autocapitalize"


autocomplete :: Text -> Maybe Attribute
autocomplete = textAttribute Attr.autocomplete


autofocus :: Bool -> Maybe Attribute
autofocus = boolAttribute $ Attr.autofocus "autofocus"


autoplay :: Bool -> Maybe Attribute
autoplay = boolAttribute $ Attr.autoplay "autoplay"


blocking :: Text -> Maybe Attribute
blocking = textAttribute $ customAttribute "blocking"


charset :: Text -> Maybe Attribute
charset = textAttribute Attr.charset


checked :: Bool -> Maybe Attribute
checked = boolAttribute $ Attr.checked "checked"


cite :: Text -> Maybe Attribute
cite = textAttribute Attr.cite


class_ :: Text -> Maybe Attribute
class_ = textAttribute Attr.class_


color :: Text -> Maybe Attribute
color = textAttribute $ customAttribute "color"


cols :: Text -> Maybe Attribute
cols = textAttribute Attr.cols


colspan :: Text -> Maybe Attribute
colspan = textAttribute Attr.colspan


content :: Text -> Maybe Attribute
content = textAttribute Attr.content


contenteditable :: Text -> Maybe Attribute
contenteditable = textAttribute Attr.contenteditable


controls :: Bool -> Maybe Attribute
controls = boolAttribute $ Attr.controls "controls"


coords :: Text -> Maybe Attribute
coords = textAttribute Attr.coords


crossorigin :: Text -> Maybe Attribute
crossorigin = textAttribute $ customAttribute "crossorigin"


data_ :: Text -> Maybe Attribute
data_ = textAttribute Attr.data_


datetime :: Text -> Maybe Attribute
datetime = textAttribute Attr.datetime


decoding :: Text -> Maybe Attribute
decoding = textAttribute $ customAttribute "decoding"


default_ :: Bool -> Maybe Attribute
default_ = boolAttribute $ customAttribute "default" "default"


defer :: Bool -> Maybe Attribute
defer = boolAttribute $ Attr.defer "defer"


dir :: Text -> Maybe Attribute
dir = textAttribute Attr.dir


disabled :: Bool -> Maybe Attribute
disabled = boolAttribute $ Attr.disabled "disabled"


download :: Text -> Maybe Attribute
download = textAttribute $ customAttribute "download"


draggable :: Text -> Maybe Attribute
draggable = textAttribute Attr.draggable


enctype :: Text -> Maybe Attribute
enctype = textAttribute Attr.enctype


enterkeyhint :: Text -> Maybe Attribute
enterkeyhint = textAttribute $ customAttribute "enterkeyhint"


for :: Text -> Maybe Attribute
for = textAttribute Attr.for


form :: Text -> Maybe Attribute
form = textAttribute Attr.form


formaction :: Text -> Maybe Attribute
formaction = textAttribute Attr.formaction


formenctype :: Text -> Maybe Attribute
formenctype = textAttribute Attr.formenctype


formmethod :: Text -> Maybe Attribute
formmethod = textAttribute Attr.formmethod


formnovalidate :: Bool -> Maybe Attribute
formnovalidate = boolAttribute $ Attr.formnovalidate "formnovalidate"


formtarget :: Text -> Maybe Attribute
formtarget = textAttribute Attr.formtarget


headers :: Text -> Maybe Attribute
headers = textAttribute Attr.headers


height :: Text -> Maybe Attribute
height = textAttribute Attr.height


hidden :: Bool -> Maybe Attribute
hidden = boolAttribute $ Attr.hidden "hidden"


high :: Text -> Maybe Attribute
high = textAttribute Attr.high


href :: Text -> Maybe Attribute
href = textAttribute Attr.href


hreflang :: Text -> Maybe Attribute
hreflang = textAttribute Attr.hreflang


httpEquiv :: Text -> Maybe Attribute
httpEquiv = textAttribute Attr.httpEquiv


id :: Text -> Maybe Attribute
id = textAttribute Attr.id


imagesizes :: Text -> Maybe Attribute
imagesizes = textAttribute $ customAttribute "imagesizes"


imagesrcset :: Text -> Maybe Attribute
imagesrcset = textAttribute $ customAttribute "imagesrcset"


inert :: Bool -> Maybe Attribute
inert = boolAttribute $ customAttribute "inert" "inert"


inputmode :: Text -> Maybe Attribute
inputmode = textAttribute $ customAttribute "inputmode"


integrity :: Text -> Maybe Attribute
integrity = textAttribute $ customAttribute "integrity"


is :: Text -> Maybe Attribute
is = textAttribute $ customAttribute "is"


ismap :: Bool -> Maybe Attribute
ismap = boolAttribute $ Attr.ismap "ismap"


itemid :: Text -> Maybe Attribute
itemid = textAttribute $ customAttribute "itemid"


itemprop :: Text -> Maybe Attribute
itemprop = textAttribute Attr.itemprop


itemref :: Text -> Maybe Attribute
itemref = textAttribute $ customAttribute "itemref"


itemscope :: Bool -> Maybe Attribute
itemscope = boolAttribute $ Attr.itemscope "itemscope"


itemtype :: Text -> Maybe Attribute
itemtype = textAttribute Attr.itemtype


kind :: Text -> Maybe Attribute
kind = textAttribute $ customAttribute "kind"


label :: Text -> Maybe Attribute
label = textAttribute Attr.label


lang :: Text -> Maybe Attribute
lang = textAttribute Attr.lang


list :: Text -> Maybe Attribute
list = textAttribute Attr.list


loading :: Text -> Maybe Attribute
loading = textAttribute $ customAttribute "loading"


loop :: Bool -> Maybe Attribute
loop = boolAttribute $ Attr.loop "loop"


low :: Text -> Maybe Attribute
low = textAttribute Attr.low


max :: Text -> Maybe Attribute
max = textAttribute Attr.max


maxlength :: Text -> Maybe Attribute
maxlength = textAttribute Attr.maxlength


media :: Text -> Maybe Attribute
media = textAttribute Attr.media


method :: Text -> Maybe Attribute
method = textAttribute Attr.method


min :: Text -> Maybe Attribute
min = textAttribute Attr.min


minlength :: Text -> Maybe Attribute
minlength = textAttribute $ customAttribute "minlength"


multiple :: Bool -> Maybe Attribute
multiple = boolAttribute $ Attr.multiple "multiple"


muted :: Bool -> Maybe Attribute
muted = boolAttribute $ customAttribute "muted" "muted"


name :: Text -> Maybe Attribute
name = textAttribute Attr.name


nomodule :: Bool -> Maybe Attribute
nomodule = boolAttribute $ customAttribute "nomodule" "nomodule"


nonce :: Text -> Maybe Attribute
nonce = textAttribute $ customAttribute "nonce"


novalidate :: Bool -> Maybe Attribute
novalidate = boolAttribute $ Attr.novalidate "novalidate"


open :: Bool -> Maybe Attribute
open = boolAttribute $ Attr.open "open"


optimum :: Text -> Maybe Attribute
optimum = textAttribute Attr.optimum


pattern :: Text -> Maybe Attribute
pattern = textAttribute Attr.pattern


ping :: Text -> Maybe Attribute
ping = textAttribute Attr.ping


placeholder :: Text -> Maybe Attribute
placeholder = textAttribute Attr.placeholder


playsinline :: Bool -> Maybe Attribute
playsinline = boolAttribute $ customAttribute "playsinline" "playsinline"


poster :: Text -> Maybe Attribute
poster = textAttribute $ customAttribute "poster"


preload :: Text -> Maybe Attribute
preload = textAttribute Attr.preload


readonly :: Bool -> Maybe Attribute
readonly = boolAttribute $ Attr.readonly "readonly"


referrerpolicy :: Text -> Maybe Attribute
referrerpolicy = textAttribute $ customAttribute "referrerpolicy"


rel :: Text -> Maybe Attribute
rel = textAttribute Attr.rel


required :: Bool -> Maybe Attribute
required = boolAttribute $ Attr.required "required"


reversed :: Bool -> Maybe Attribute
reversed = boolAttribute $ Attr.reversed "reversed"


rows :: Text -> Maybe Attribute
rows = textAttribute Attr.rows


rowspan :: Text -> Maybe Attribute
rowspan = textAttribute Attr.rowspan


sandbox :: Text -> Maybe Attribute
sandbox = textAttribute Attr.sandbox


scope :: Text -> Maybe Attribute
scope = textAttribute Attr.scope


selected :: Bool -> Maybe Attribute
selected = boolAttribute $ Attr.selected "selected"


shape :: Text -> Maybe Attribute
shape = textAttribute Attr.shape


size :: Text -> Maybe Attribute
size = textAttribute Attr.size


sizes :: Text -> Maybe Attribute
sizes = textAttribute Attr.sizes


slot :: Text -> Maybe Attribute
slot = textAttribute $ customAttribute "slot"


span :: Text -> Maybe Attribute
span = textAttribute Attr.span


spellcheck :: Text -> Maybe Attribute
spellcheck = textAttribute Attr.spellcheck


src :: Text -> Maybe Attribute
src = textAttribute Attr.src


srcdoc :: Text -> Maybe Attribute
srcdoc = textAttribute Attr.srcdoc


srclang :: Text -> Maybe Attribute
srclang = textAttribute $ customAttribute "srclang"


srcset :: Text -> Maybe Attribute
srcset = textAttribute $ customAttribute "srcset"


start :: Text -> Maybe Attribute
start = textAttribute Attr.start


step :: Text -> Maybe Attribute
step = textAttribute Attr.step


style :: [Text] -> Maybe Attribute
style = textAttribute Attr.style . unwords


tabindex :: Text -> Maybe Attribute
tabindex = textAttribute Attr.tabindex


target :: Text -> Maybe Attribute
target = textAttribute Attr.target


title :: Text -> Maybe Attribute
title = textAttribute Attr.title


translate :: Text -> Maybe Attribute
translate = textAttribute $ customAttribute "translate"


type_ :: Text -> Maybe Attribute
type_ = textAttribute Attr.type_


usemap :: Text -> Maybe Attribute
usemap = textAttribute Attr.usemap


value :: Text -> Maybe Attribute
value = textAttribute Attr.value


width :: Text -> Maybe Attribute
width = textAttribute Attr.width


wrap :: Text -> Maybe Attribute
wrap = textAttribute Attr.wrap



onbeforeonload :: Text -> Maybe Attribute
onbeforeonload = textAttribute Attr.onbeforeonload


onbeforeprint :: Text -> Maybe Attribute
onbeforeprint = textAttribute Attr.onbeforeprint


onblur :: Text -> Maybe Attribute
onblur = textAttribute Attr.onblur


oncanplay :: Text -> Maybe Attribute
oncanplay = textAttribute Attr.oncanplay


oncanplaythrough :: Text -> Maybe Attribute
oncanplaythrough = textAttribute Attr.oncanplaythrough


onchange :: Text -> Maybe Attribute
onchange = textAttribute Attr.onchange


onclick :: Text -> Maybe Attribute
onclick = textAttribute Attr.onclick


oncontextmenu :: Text -> Maybe Attribute
oncontextmenu = textAttribute Attr.oncontextmenu


ondblclick :: Text -> Maybe Attribute
ondblclick = textAttribute Attr.ondblclick


ondrag :: Text -> Maybe Attribute
ondrag = textAttribute Attr.ondrag


ondragend :: Text -> Maybe Attribute
ondragend = textAttribute Attr.ondragend


ondragenter :: Text -> Maybe Attribute
ondragenter = textAttribute Attr.ondragenter


ondragleave :: Text -> Maybe Attribute
ondragleave = textAttribute Attr.ondragleave


ondragover :: Text -> Maybe Attribute
ondragover = textAttribute Attr.ondragover


ondragstart :: Text -> Maybe Attribute
ondragstart = textAttribute Attr.ondragstart


ondrop :: Text -> Maybe Attribute
ondrop = textAttribute Attr.ondrop


ondurationchange :: Text -> Maybe Attribute
ondurationchange = textAttribute Attr.ondurationchange


onemptied :: Text -> Maybe Attribute
onemptied = textAttribute Attr.onemptied


onended :: Text -> Maybe Attribute
onended = textAttribute Attr.onended


onerror :: Text -> Maybe Attribute
onerror = textAttribute Attr.onerror


onfocus :: Text -> Maybe Attribute
onfocus = textAttribute Attr.onfocus


onformchange :: Text -> Maybe Attribute
onformchange = textAttribute Attr.onformchange


onforminput :: Text -> Maybe Attribute
onforminput = textAttribute Attr.onforminput


onhaschange :: Text -> Maybe Attribute
onhaschange = textAttribute Attr.onhaschange


oninput :: Text -> Maybe Attribute
oninput = textAttribute Attr.oninput


oninvalid :: Text -> Maybe Attribute
oninvalid = textAttribute Attr.oninvalid


onkeydown :: Text -> Maybe Attribute
onkeydown = textAttribute Attr.onkeydown


onkeyup :: Text -> Maybe Attribute
onkeyup = textAttribute Attr.onkeyup


onload :: Text -> Maybe Attribute
onload = textAttribute Attr.onload


onloadeddata :: Text -> Maybe Attribute
onloadeddata = textAttribute Attr.onloadeddata


onloadedmetadata :: Text -> Maybe Attribute
onloadedmetadata = textAttribute Attr.onloadedmetadata


onloadstart :: Text -> Maybe Attribute
onloadstart = textAttribute Attr.onloadstart


onmessage :: Text -> Maybe Attribute
onmessage = textAttribute Attr.onmessage


onmousedown :: Text -> Maybe Attribute
onmousedown = textAttribute Attr.onmousedown


onmousemove :: Text -> Maybe Attribute
onmousemove = textAttribute Attr.onmousemove


onmouseout :: Text -> Maybe Attribute
onmouseout = textAttribute Attr.onmouseout


onmouseover :: Text -> Maybe Attribute
onmouseover = textAttribute Attr.onmouseover


onmouseup :: Text -> Maybe Attribute
onmouseup = textAttribute Attr.onmouseup


onmousewheel :: Text -> Maybe Attribute
onmousewheel = textAttribute Attr.onmousewheel


ononline :: Text -> Maybe Attribute
ononline = textAttribute Attr.ononline


onpagehide :: Text -> Maybe Attribute
onpagehide = textAttribute Attr.onpagehide


onpageshow :: Text -> Maybe Attribute
onpageshow = textAttribute Attr.onpageshow


onpause :: Text -> Maybe Attribute
onpause = textAttribute Attr.onpause


onplay :: Text -> Maybe Attribute
onplay = textAttribute Attr.onplay


onplaying :: Text -> Maybe Attribute
onplaying = textAttribute Attr.onplaying


onprogress :: Text -> Maybe Attribute
onprogress = textAttribute Attr.onprogress


onpropstate :: Text -> Maybe Attribute
onpropstate = textAttribute Attr.onpropstate


onratechange :: Text -> Maybe Attribute
onratechange = textAttribute Attr.onratechange


onreadystatechange :: Text -> Maybe Attribute
onreadystatechange = textAttribute Attr.onreadystatechange


onredo :: Text -> Maybe Attribute
onredo = textAttribute Attr.onredo


onresize :: Text -> Maybe Attribute
onresize = textAttribute Attr.onresize


onscroll :: Text -> Maybe Attribute
onscroll = textAttribute Attr.onscroll


onseeked :: Text -> Maybe Attribute
onseeked = textAttribute Attr.onseeked


onseeking :: Text -> Maybe Attribute
onseeking = textAttribute Attr.onseeking


onselect :: Text -> Maybe Attribute
onselect = textAttribute Attr.onselect


onstalled :: Text -> Maybe Attribute
onstalled = textAttribute Attr.onstalled


onstorage :: Text -> Maybe Attribute
onstorage = textAttribute Attr.onstorage


onsubmit :: Text -> Maybe Attribute
onsubmit = textAttribute Attr.onsubmit


onsuspend :: Text -> Maybe Attribute
onsuspend = textAttribute Attr.onsuspend


ontimeupdate :: Text -> Maybe Attribute
ontimeupdate = textAttribute Attr.ontimeupdate


onundo :: Text -> Maybe Attribute
onundo = textAttribute Attr.onundo


onunload :: Text -> Maybe Attribute
onunload = textAttribute Attr.onunload


onvolumechange :: Text -> Maybe Attribute
onvolumechange = textAttribute Attr.onvolumechange


onwaiting :: Text -> Maybe Attribute
onwaiting = textAttribute Attr.onwaiting



css :: [Text] -> Maybe Attribute
css = textAttribute Attr.class_ . unwords
