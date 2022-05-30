{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

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
    , class_
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


import Data.Text (Text)

import Blizzard.Internal (Attribute(..))


accept :: Text -> Attribute
accept = Attr "accept"


acceptCharset :: Text -> Attribute
acceptCharset = Attr "accept-charset"


accesskey :: Text -> Attribute
accesskey = Attr "accesskey"


action :: Text -> Attribute
action = Attr "action"


alt :: Text -> Attribute
alt = Attr "alt"


async :: Text -> Attribute
async = Attr "async"


autocomplete :: Text -> Attribute
autocomplete = Attr "autocomplete"


autofocus :: Text -> Attribute
autofocus = Attr "autofocus"


autoplay :: Text -> Attribute
autoplay = Attr "autoplay"


challenge :: Text -> Attribute
challenge = Attr "challenge"


charset :: Text -> Attribute
charset = Attr "charset"


checked :: Text -> Attribute
checked = Attr "checked"


cite :: Text -> Attribute
cite = Attr "cite"


class_ :: Text -> Attribute
class_ = Attr "class"


cols :: Text -> Attribute
cols = Attr "cols"


colspan :: Text -> Attribute
colspan = Attr "colspan"


content :: Text -> Attribute
content = Attr "content"


contenteditable :: Text -> Attribute
contenteditable = Attr "contenteditable"


contextmenu :: Text -> Attribute
contextmenu = Attr "contextmenu"


controls :: Text -> Attribute
controls = Attr "controls"


coords :: Text -> Attribute
coords = Attr "coords"


data_ :: Text -> Attribute
data_ = Attr "data"


datetime :: Text -> Attribute
datetime = Attr "datetime"


defer :: Text -> Attribute
defer = Attr "defer"


dir :: Text -> Attribute
dir = Attr "dir"


disabled :: Text -> Attribute
disabled = Attr "disabled"


draggable :: Text -> Attribute
draggable = Attr "draggable"


enctype :: Text -> Attribute
enctype = Attr "enctype"


for :: Text -> Attribute
for = Attr "for"


form :: Text -> Attribute
form = Attr "form"


formaction :: Text -> Attribute
formaction = Attr "formaction"


formenctype :: Text -> Attribute
formenctype = Attr "formenctype"


formmethod :: Text -> Attribute
formmethod = Attr "formmethod"


formnovalidate :: Text -> Attribute
formnovalidate = Attr "formnovalidate"


formtarget :: Text -> Attribute
formtarget = Attr "formtarget"


headers :: Text -> Attribute
headers = Attr "headers"


height :: Text -> Attribute
height = Attr "height"


hidden :: Text -> Attribute
hidden = Attr "hidden"


high :: Text -> Attribute
high = Attr "high"


href :: Text -> Attribute
href = Attr "href"


hreflang :: Text -> Attribute
hreflang = Attr "hreflang"


httpEquiv :: Text -> Attribute
httpEquiv = Attr "http-equiv"


icon :: Text -> Attribute
icon = Attr "icon"


id :: Text -> Attribute
id = Attr "id"


ismap :: Text -> Attribute
ismap = Attr "ismap"


item :: Text -> Attribute
item = Attr "item"


itemprop :: Text -> Attribute
itemprop = Attr "itemprop"


itemscope :: Text -> Attribute
itemscope = Attr "itemscope"


itemtype :: Text -> Attribute
itemtype = Attr "itemtype"


keytype :: Text -> Attribute
keytype = Attr "keytype"


label :: Text -> Attribute
label = Attr "label"


lang :: Text -> Attribute
lang = Attr "lang"


list :: Text -> Attribute
list = Attr "list"


loop :: Text -> Attribute
loop = Attr "loop"


low :: Text -> Attribute
low = Attr "low"


manifest :: Text -> Attribute
manifest = Attr "manifest"


max :: Text -> Attribute
max = Attr "max"


maxlength :: Text -> Attribute
maxlength = Attr "maxlength"


media :: Text -> Attribute
media = Attr "media"


method :: Text -> Attribute
method = Attr "method"


min :: Text -> Attribute
min = Attr "min"


multiple :: Text -> Attribute
multiple = Attr "multiple"


name :: Text -> Attribute
name = Attr "name"


novalidate :: Text -> Attribute
novalidate = Attr "novalidate"


onbeforeonload :: Text -> Attribute
onbeforeonload = Attr "onbeforeonload"


onbeforeprint :: Text -> Attribute
onbeforeprint = Attr "onbeforeprint"


onblur :: Text -> Attribute
onblur = Attr "onblur"


oncanplay :: Text -> Attribute
oncanplay = Attr "oncanplay"


oncanplaythrough :: Text -> Attribute
oncanplaythrough = Attr "oncanplaythrough"


onchange :: Text -> Attribute
onchange = Attr "onchange"


onclick :: Text -> Attribute
onclick = Attr "onclick"


oncontextmenu :: Text -> Attribute
oncontextmenu = Attr "oncontextmenu"


ondblclick :: Text -> Attribute
ondblclick = Attr "ondblclick"


ondrag :: Text -> Attribute
ondrag = Attr "ondrag"


ondragend :: Text -> Attribute
ondragend = Attr "ondragend"


ondragenter :: Text -> Attribute
ondragenter = Attr "ondragenter"


ondragleave :: Text -> Attribute
ondragleave = Attr "ondragleave"


ondragover :: Text -> Attribute
ondragover = Attr "ondragover"


ondragstart :: Text -> Attribute
ondragstart = Attr "ondragstart"


ondrop :: Text -> Attribute
ondrop = Attr "ondrop"


ondurationchange :: Text -> Attribute
ondurationchange = Attr "ondurationchange"


onemptied :: Text -> Attribute
onemptied = Attr "onemptied"


onended :: Text -> Attribute
onended = Attr "onended"


onerror :: Text -> Attribute
onerror = Attr "onerror"


onfocus :: Text -> Attribute
onfocus = Attr "onfocus"


onformchange :: Text -> Attribute
onformchange = Attr "onformchange"


onforminput :: Text -> Attribute
onforminput = Attr "onforminput"


onhaschange :: Text -> Attribute
onhaschange = Attr "onhaschange"


oninput :: Text -> Attribute
oninput = Attr "oninput"


oninvalid :: Text -> Attribute
oninvalid = Attr "oninvalid"


onkeydown :: Text -> Attribute
onkeydown = Attr "onkeydown"


onkeyup :: Text -> Attribute
onkeyup = Attr "onkeyup"


onload :: Text -> Attribute
onload = Attr "onload"


onloadeddata :: Text -> Attribute
onloadeddata = Attr "onloadeddata"


onloadedmetadata :: Text -> Attribute
onloadedmetadata = Attr "onloadedmetadata"


onloadstart :: Text -> Attribute
onloadstart = Attr "onloadstart"


onmessage :: Text -> Attribute
onmessage = Attr "onmessage"


onmousedown :: Text -> Attribute
onmousedown = Attr "onmousedown"


onmousemove :: Text -> Attribute
onmousemove = Attr "onmousemove"


onmouseout :: Text -> Attribute
onmouseout = Attr "onmouseout"


onmouseover :: Text -> Attribute
onmouseover = Attr "onmouseover"


onmouseup :: Text -> Attribute
onmouseup = Attr "onmouseup"


onmousewheel :: Text -> Attribute
onmousewheel = Attr "onmousewheel"


ononline :: Text -> Attribute
ononline = Attr "ononline"


onpagehide :: Text -> Attribute
onpagehide = Attr "onpagehide"


onpageshow :: Text -> Attribute
onpageshow = Attr "onpageshow"


onpause :: Text -> Attribute
onpause = Attr "onpause"


onplay :: Text -> Attribute
onplay = Attr "onplay"


onplaying :: Text -> Attribute
onplaying = Attr "onplaying"


onprogress :: Text -> Attribute
onprogress = Attr "onprogress"


onpropstate :: Text -> Attribute
onpropstate = Attr "onpropstate"


onratechange :: Text -> Attribute
onratechange = Attr "onratechange"


onreadystatechange :: Text -> Attribute
onreadystatechange = Attr "onreadystatechange"


onredo :: Text -> Attribute
onredo = Attr "onredo"


onresize :: Text -> Attribute
onresize = Attr "onresize"


onscroll :: Text -> Attribute
onscroll = Attr "onscroll"


onseeked :: Text -> Attribute
onseeked = Attr "onseeked"


onseeking :: Text -> Attribute
onseeking = Attr "onseeking"


onselect :: Text -> Attribute
onselect = Attr "onselect"


onstalled :: Text -> Attribute
onstalled = Attr "onstalled"


onstorage :: Text -> Attribute
onstorage = Attr "onstorage"


onsubmit :: Text -> Attribute
onsubmit = Attr "onsubmit"


onsuspend :: Text -> Attribute
onsuspend = Attr "onsuspend"


ontimeupdate :: Text -> Attribute
ontimeupdate = Attr "ontimeupdate"


onundo :: Text -> Attribute
onundo = Attr "onundo"


onunload :: Text -> Attribute
onunload = Attr "onunload"


onvolumechange :: Text -> Attribute
onvolumechange = Attr "onvolumechange"


onwaiting :: Text -> Attribute
onwaiting = Attr "onwaiting"


open :: Text -> Attribute
open = Attr "open"


optimum :: Text -> Attribute
optimum = Attr "optimum"


pattern :: Text -> Attribute
pattern = Attr "pattern"


ping :: Text -> Attribute
ping = Attr "ping"


placeholder :: Text -> Attribute
placeholder = Attr "placeholder"


preload :: Text -> Attribute
preload = Attr "preload"


pubdate :: Text -> Attribute
pubdate = Attr "pubdate"


radiogroup :: Text -> Attribute
radiogroup = Attr "radiogroup"


readonly :: Text -> Attribute
readonly = Attr "readonly"


rel :: Text -> Attribute
rel = Attr "rel"


required :: Text -> Attribute
required = Attr "required"


reversed :: Text -> Attribute
reversed = Attr "reversed"


role :: Text -> Attribute
role = Attr "role"


rows :: Text -> Attribute
rows = Attr "rows"


rowspan :: Text -> Attribute
rowspan = Attr "rowspan"


sandbox :: Text -> Attribute
sandbox = Attr "sandbox"


scope :: Text -> Attribute
scope = Attr "scope"


scoped :: Text -> Attribute
scoped = Attr "scoped"


seamless :: Text -> Attribute
seamless = Attr "seamless"


selected :: Text -> Attribute
selected = Attr "selected"


shape :: Text -> Attribute
shape = Attr "shape"


size :: Text -> Attribute
size = Attr "size"


sizes :: Text -> Attribute
sizes = Attr "sizes"


span :: Text -> Attribute
span = Attr "span"


spellcheck :: Text -> Attribute
spellcheck = Attr "spellcheck"


src :: Text -> Attribute
src = Attr "src"


srcdoc :: Text -> Attribute
srcdoc = Attr "srcdoc"


start :: Text -> Attribute
start = Attr "start"


step :: Text -> Attribute
step = Attr "step"


style :: Text -> Attribute
style = Attr "style"


subject :: Text -> Attribute
subject = Attr "subject"


summary :: Text -> Attribute
summary = Attr "summary"


tabindex :: Text -> Attribute
tabindex = Attr "tabindex"


target :: Text -> Attribute
target = Attr "target"


title :: Text -> Attribute
title = Attr "title"


type_ :: Text -> Attribute
type_ = Attr "type"


usemap :: Text -> Attribute
usemap = Attr "usemap"


value :: Text -> Attribute
value = Attr "value"


width :: Text -> Attribute
width = Attr "width"


wrap :: Text -> Attribute
wrap = Attr "wrap"


xmlns :: Text -> Attribute
xmlns = Attr "xmlns"
