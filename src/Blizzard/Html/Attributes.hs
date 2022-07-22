{-# LANGUAGE NoImplicitPrelude #-}

module Blizzard.Html.Attributes
    ( accept
    , acceptCharset
    , accesskey
    , action
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
    , name
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
    , type_
    , usemap
    , value
    , width
    , wrap
    , xmlns
    ) where


import Data.Text (Text, unwords)
import Prelude ((.))
import Text.Blaze.Html5 (Attribute, textValue)

import qualified Text.Blaze.Html5.Attributes as Attr


accept :: Text -> Attribute
accept = Attr.accept . textValue


acceptCharset :: Text -> Attribute
acceptCharset = Attr.acceptCharset . textValue


accesskey :: Text -> Attribute
accesskey = Attr.accesskey . textValue


action :: Text -> Attribute
action = Attr.action . textValue


alt :: Text -> Attribute
alt = Attr.alt . textValue


async :: Text -> Attribute
async = Attr.async . textValue


autocomplete :: Text -> Attribute
autocomplete = Attr.autocomplete . textValue


autofocus :: Text -> Attribute
autofocus = Attr.autofocus . textValue


autoplay :: Text -> Attribute
autoplay = Attr.autoplay . textValue


challenge :: Text -> Attribute
challenge = Attr.challenge . textValue


charset :: Text -> Attribute
charset = Attr.charset . textValue


checked :: Text -> Attribute
checked = Attr.checked . textValue


cite :: Text -> Attribute
cite = Attr.cite . textValue


css :: [Text] -> Attribute
css = Attr.class_ . textValue . unwords


cols :: Text -> Attribute
cols = Attr.cols . textValue


colspan :: Text -> Attribute
colspan = Attr.colspan . textValue


content :: Text -> Attribute
content = Attr.content . textValue


contenteditable :: Text -> Attribute
contenteditable = Attr.contenteditable . textValue


contextmenu :: Text -> Attribute
contextmenu = Attr.contextmenu . textValue


controls :: Text -> Attribute
controls = Attr.controls . textValue


coords :: Text -> Attribute
coords = Attr.coords . textValue


data_ :: Text -> Attribute
data_ = Attr.data_ . textValue


datetime :: Text -> Attribute
datetime = Attr.datetime . textValue


defer :: Text -> Attribute
defer = Attr.defer . textValue


dir :: Text -> Attribute
dir = Attr.dir . textValue


disabled :: Text -> Attribute
disabled = Attr.disabled . textValue


draggable :: Text -> Attribute
draggable = Attr.draggable . textValue


enctype :: Text -> Attribute
enctype = Attr.enctype . textValue


for :: Text -> Attribute
for = Attr.for . textValue


form :: Text -> Attribute
form = Attr.form . textValue


formaction :: Text -> Attribute
formaction = Attr.formaction . textValue


formenctype :: Text -> Attribute
formenctype = Attr.formenctype . textValue


formmethod :: Text -> Attribute
formmethod = Attr.formmethod . textValue


formnovalidate :: Text -> Attribute
formnovalidate = Attr.formnovalidate . textValue


formtarget :: Text -> Attribute
formtarget = Attr.formtarget . textValue


headers :: Text -> Attribute
headers = Attr.headers . textValue


height :: Text -> Attribute
height = Attr.height . textValue


hidden :: Text -> Attribute
hidden = Attr.hidden . textValue


high :: Text -> Attribute
high = Attr.high . textValue


href :: Text -> Attribute
href = Attr.href . textValue


hreflang :: Text -> Attribute
hreflang = Attr.hreflang . textValue


httpEquiv :: Text -> Attribute
httpEquiv = Attr.httpEquiv . textValue


icon :: Text -> Attribute
icon = Attr.icon . textValue


id :: Text -> Attribute
id = Attr.id . textValue


ismap :: Text -> Attribute
ismap = Attr.ismap . textValue


item :: Text -> Attribute
item = Attr.item . textValue


itemprop :: Text -> Attribute
itemprop = Attr.itemprop . textValue


itemscope :: Text -> Attribute
itemscope = Attr.itemscope . textValue


itemtype :: Text -> Attribute
itemtype = Attr.itemtype . textValue


keytype :: Text -> Attribute
keytype = Attr.keytype . textValue


label :: Text -> Attribute
label = Attr.label . textValue


lang :: Text -> Attribute
lang = Attr.lang . textValue


list :: Text -> Attribute
list = Attr.list . textValue


loop :: Text -> Attribute
loop = Attr.loop . textValue


low :: Text -> Attribute
low = Attr.low . textValue


manifest :: Text -> Attribute
manifest = Attr.manifest . textValue


max :: Text -> Attribute
max = Attr.max . textValue


maxlength :: Text -> Attribute
maxlength = Attr.maxlength . textValue


media :: Text -> Attribute
media = Attr.media . textValue


method :: Text -> Attribute
method = Attr.method . textValue


min :: Text -> Attribute
min = Attr.min . textValue


multiple :: Text -> Attribute
multiple = Attr.multiple . textValue


name :: Text -> Attribute
name = Attr.name . textValue


novalidate :: Text -> Attribute
novalidate = Attr.novalidate . textValue


onbeforeonload :: Text -> Attribute
onbeforeonload = Attr.onbeforeonload . textValue


onbeforeprint :: Text -> Attribute
onbeforeprint = Attr.onbeforeprint . textValue


onblur :: Text -> Attribute
onblur = Attr.onblur . textValue


oncanplay :: Text -> Attribute
oncanplay = Attr.oncanplay . textValue


oncanplaythrough :: Text -> Attribute
oncanplaythrough = Attr.oncanplaythrough . textValue


onchange :: Text -> Attribute
onchange = Attr.onchange . textValue


onclick :: Text -> Attribute
onclick = Attr.onclick . textValue


oncontextmenu :: Text -> Attribute
oncontextmenu = Attr.oncontextmenu . textValue


ondblclick :: Text -> Attribute
ondblclick = Attr.ondblclick . textValue


ondrag :: Text -> Attribute
ondrag = Attr.ondrag . textValue


ondragend :: Text -> Attribute
ondragend = Attr.ondragend . textValue


ondragenter :: Text -> Attribute
ondragenter = Attr.ondragenter . textValue


ondragleave :: Text -> Attribute
ondragleave = Attr.ondragleave . textValue


ondragover :: Text -> Attribute
ondragover = Attr.ondragover . textValue


ondragstart :: Text -> Attribute
ondragstart = Attr.ondragstart . textValue


ondrop :: Text -> Attribute
ondrop = Attr.ondrop . textValue


ondurationchange :: Text -> Attribute
ondurationchange = Attr.ondurationchange . textValue


onemptied :: Text -> Attribute
onemptied = Attr.onemptied . textValue


onended :: Text -> Attribute
onended = Attr.onended . textValue


onerror :: Text -> Attribute
onerror = Attr.onerror . textValue


onfocus :: Text -> Attribute
onfocus = Attr.onfocus . textValue


onformchange :: Text -> Attribute
onformchange = Attr.onformchange . textValue


onforminput :: Text -> Attribute
onforminput = Attr.onforminput . textValue


onhaschange :: Text -> Attribute
onhaschange = Attr.onhaschange . textValue


oninput :: Text -> Attribute
oninput = Attr.oninput . textValue


oninvalid :: Text -> Attribute
oninvalid = Attr.oninvalid . textValue


onkeydown :: Text -> Attribute
onkeydown = Attr.onkeydown . textValue


onkeyup :: Text -> Attribute
onkeyup = Attr.onkeyup . textValue


onload :: Text -> Attribute
onload = Attr.onload . textValue


onloadeddata :: Text -> Attribute
onloadeddata = Attr.onloadeddata . textValue


onloadedmetadata :: Text -> Attribute
onloadedmetadata = Attr.onloadedmetadata . textValue


onloadstart :: Text -> Attribute
onloadstart = Attr.onloadstart . textValue


onmessage :: Text -> Attribute
onmessage = Attr.onmessage . textValue


onmousedown :: Text -> Attribute
onmousedown = Attr.onmousedown . textValue


onmousemove :: Text -> Attribute
onmousemove = Attr.onmousemove . textValue


onmouseout :: Text -> Attribute
onmouseout = Attr.onmouseout . textValue


onmouseover :: Text -> Attribute
onmouseover = Attr.onmouseover . textValue


onmouseup :: Text -> Attribute
onmouseup = Attr.onmouseup . textValue


onmousewheel :: Text -> Attribute
onmousewheel = Attr.onmousewheel . textValue


ononline :: Text -> Attribute
ononline = Attr.ononline . textValue


onpagehide :: Text -> Attribute
onpagehide = Attr.onpagehide . textValue


onpageshow :: Text -> Attribute
onpageshow = Attr.onpageshow . textValue


onpause :: Text -> Attribute
onpause = Attr.onpause . textValue


onplay :: Text -> Attribute
onplay = Attr.onplay . textValue


onplaying :: Text -> Attribute
onplaying = Attr.onplaying . textValue


onprogress :: Text -> Attribute
onprogress = Attr.onprogress . textValue


onpropstate :: Text -> Attribute
onpropstate = Attr.onpropstate . textValue


onratechange :: Text -> Attribute
onratechange = Attr.onratechange . textValue


onreadystatechange :: Text -> Attribute
onreadystatechange = Attr.onreadystatechange . textValue


onredo :: Text -> Attribute
onredo = Attr.onredo . textValue


onresize :: Text -> Attribute
onresize = Attr.onresize . textValue


onscroll :: Text -> Attribute
onscroll = Attr.onscroll . textValue


onseeked :: Text -> Attribute
onseeked = Attr.onseeked . textValue


onseeking :: Text -> Attribute
onseeking = Attr.onseeking . textValue


onselect :: Text -> Attribute
onselect = Attr.onselect . textValue


onstalled :: Text -> Attribute
onstalled = Attr.onstalled . textValue


onstorage :: Text -> Attribute
onstorage = Attr.onstorage . textValue


onsubmit :: Text -> Attribute
onsubmit = Attr.onsubmit . textValue


onsuspend :: Text -> Attribute
onsuspend = Attr.onsuspend . textValue


ontimeupdate :: Text -> Attribute
ontimeupdate = Attr.ontimeupdate . textValue


onundo :: Text -> Attribute
onundo = Attr.onundo . textValue


onunload :: Text -> Attribute
onunload = Attr.onunload . textValue


onvolumechange :: Text -> Attribute
onvolumechange = Attr.onvolumechange . textValue


onwaiting :: Text -> Attribute
onwaiting = Attr.onwaiting . textValue


open :: Text -> Attribute
open = Attr.open . textValue


optimum :: Text -> Attribute
optimum = Attr.optimum . textValue


pattern :: Text -> Attribute
pattern = Attr.pattern . textValue


ping :: Text -> Attribute
ping = Attr.ping . textValue


placeholder :: Text -> Attribute
placeholder = Attr.placeholder . textValue


preload :: Text -> Attribute
preload = Attr.preload . textValue


pubdate :: Text -> Attribute
pubdate = Attr.pubdate . textValue


radiogroup :: Text -> Attribute
radiogroup = Attr.radiogroup . textValue


readonly :: Text -> Attribute
readonly = Attr.readonly . textValue


rel :: Text -> Attribute
rel = Attr.rel . textValue


required :: Text -> Attribute
required = Attr.required . textValue


reversed :: Text -> Attribute
reversed = Attr.reversed . textValue


role :: Text -> Attribute
role = Attr.role . textValue


rows :: Text -> Attribute
rows = Attr.rows . textValue


rowspan :: Text -> Attribute
rowspan = Attr.rowspan . textValue


sandbox :: Text -> Attribute
sandbox = Attr.sandbox . textValue


scope :: Text -> Attribute
scope = Attr.scope . textValue


scoped :: Text -> Attribute
scoped = Attr.scoped . textValue


seamless :: Text -> Attribute
seamless = Attr.seamless . textValue


selected :: Text -> Attribute
selected = Attr.selected . textValue


shape :: Text -> Attribute
shape = Attr.shape . textValue


size :: Text -> Attribute
size = Attr.size . textValue


sizes :: Text -> Attribute
sizes = Attr.sizes . textValue


span :: Text -> Attribute
span = Attr.span . textValue


spellcheck :: Text -> Attribute
spellcheck = Attr.spellcheck . textValue


src :: Text -> Attribute
src = Attr.src . textValue


srcdoc :: Text -> Attribute
srcdoc = Attr.srcdoc . textValue


start :: Text -> Attribute
start = Attr.start . textValue


step :: Text -> Attribute
step = Attr.step . textValue


style :: [Text] -> Attribute
style = Attr.style . textValue . unwords


subject :: Text -> Attribute
subject = Attr.subject . textValue


summary :: Text -> Attribute
summary = Attr.summary . textValue


tabindex :: Text -> Attribute
tabindex = Attr.tabindex . textValue


target :: Text -> Attribute
target = Attr.target . textValue


title :: Text -> Attribute
title = Attr.title . textValue


type_ :: Text -> Attribute
type_ = Attr.type_ . textValue


usemap :: Text -> Attribute
usemap = Attr.usemap . textValue


value :: Text -> Attribute
value = Attr.value . textValue


width :: Text -> Attribute
width = Attr.width . textValue


wrap :: Text -> Attribute
wrap = Attr.wrap . textValue


xmlns :: Text -> Attribute
xmlns = Attr.xmlns . textValue
