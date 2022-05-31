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
accept = AttrRaw "accept"


acceptCharset :: Text -> Attribute
acceptCharset = AttrRaw "accept-charset"


accesskey :: Text -> Attribute
accesskey = AttrRaw "accesskey"


action :: Text -> Attribute
action = AttrRaw "action"


alt :: Text -> Attribute
alt = AttrRaw "alt"


async :: Text -> Attribute
async = AttrRaw "async"


autocomplete :: Text -> Attribute
autocomplete = AttrRaw "autocomplete"


autofocus :: Text -> Attribute
autofocus = AttrRaw "autofocus"


autoplay :: Text -> Attribute
autoplay = AttrRaw "autoplay"


challenge :: Text -> Attribute
challenge = AttrRaw "challenge"


charset :: Text -> Attribute
charset = AttrRaw "charset"


checked :: Text -> Attribute
checked = AttrRaw "checked"


cite :: Text -> Attribute
cite = AttrRaw "cite"


class_ :: Text -> Attribute
class_ = AttrRaw "class"


cols :: Text -> Attribute
cols = AttrRaw "cols"


colspan :: Text -> Attribute
colspan = AttrRaw "colspan"


content :: Text -> Attribute
content = AttrRaw "content"


contenteditable :: Text -> Attribute
contenteditable = AttrRaw "contenteditable"


contextmenu :: Text -> Attribute
contextmenu = AttrRaw "contextmenu"


controls :: Text -> Attribute
controls = AttrRaw "controls"


coords :: Text -> Attribute
coords = AttrRaw "coords"


data_ :: Text -> Attribute
data_ = AttrRaw "data"


datetime :: Text -> Attribute
datetime = AttrRaw "datetime"


defer :: Text -> Attribute
defer = AttrRaw "defer"


dir :: Text -> Attribute
dir = AttrRaw "dir"


disabled :: Text -> Attribute
disabled = AttrRaw "disabled"


draggable :: Text -> Attribute
draggable = AttrRaw "draggable"


enctype :: Text -> Attribute
enctype = AttrRaw "enctype"


for :: Text -> Attribute
for = AttrRaw "for"


form :: Text -> Attribute
form = AttrRaw "form"


formaction :: Text -> Attribute
formaction = AttrRaw "formaction"


formenctype :: Text -> Attribute
formenctype = AttrRaw "formenctype"


formmethod :: Text -> Attribute
formmethod = AttrRaw "formmethod"


formnovalidate :: Text -> Attribute
formnovalidate = AttrRaw "formnovalidate"


formtarget :: Text -> Attribute
formtarget = AttrRaw "formtarget"


headers :: Text -> Attribute
headers = AttrRaw "headers"


height :: Text -> Attribute
height = AttrRaw "height"


hidden :: Text -> Attribute
hidden = AttrRaw "hidden"


high :: Text -> Attribute
high = AttrRaw "high"


href :: Text -> Attribute
href = AttrRaw "href"


hreflang :: Text -> Attribute
hreflang = AttrRaw "hreflang"


httpEquiv :: Text -> Attribute
httpEquiv = AttrRaw "http-equiv"


icon :: Text -> Attribute
icon = AttrRaw "icon"


id :: Text -> Attribute
id = AttrRaw "id"


ismap :: Text -> Attribute
ismap = AttrRaw "ismap"


item :: Text -> Attribute
item = AttrRaw "item"


itemprop :: Text -> Attribute
itemprop = AttrRaw "itemprop"


itemscope :: Text -> Attribute
itemscope = AttrRaw "itemscope"


itemtype :: Text -> Attribute
itemtype = AttrRaw "itemtype"


keytype :: Text -> Attribute
keytype = AttrRaw "keytype"


label :: Text -> Attribute
label = AttrRaw "label"


lang :: Text -> Attribute
lang = AttrRaw "lang"


list :: Text -> Attribute
list = AttrRaw "list"


loop :: Text -> Attribute
loop = AttrRaw "loop"


low :: Text -> Attribute
low = AttrRaw "low"


manifest :: Text -> Attribute
manifest = AttrRaw "manifest"


max :: Text -> Attribute
max = AttrRaw "max"


maxlength :: Text -> Attribute
maxlength = AttrRaw "maxlength"


media :: Text -> Attribute
media = AttrRaw "media"


method :: Text -> Attribute
method = AttrRaw "method"


min :: Text -> Attribute
min = AttrRaw "min"


multiple :: Text -> Attribute
multiple = AttrRaw "multiple"


name :: Text -> Attribute
name = AttrRaw "name"


novalidate :: Text -> Attribute
novalidate = AttrRaw "novalidate"


onbeforeonload :: Text -> Attribute
onbeforeonload = AttrRaw "onbeforeonload"


onbeforeprint :: Text -> Attribute
onbeforeprint = AttrRaw "onbeforeprint"


onblur :: Text -> Attribute
onblur = AttrRaw "onblur"


oncanplay :: Text -> Attribute
oncanplay = AttrRaw "oncanplay"


oncanplaythrough :: Text -> Attribute
oncanplaythrough = AttrRaw "oncanplaythrough"


onchange :: Text -> Attribute
onchange = AttrRaw "onchange"


onclick :: Text -> Attribute
onclick = AttrRaw "onclick"


oncontextmenu :: Text -> Attribute
oncontextmenu = AttrRaw "oncontextmenu"


ondblclick :: Text -> Attribute
ondblclick = AttrRaw "ondblclick"


ondrag :: Text -> Attribute
ondrag = AttrRaw "ondrag"


ondragend :: Text -> Attribute
ondragend = AttrRaw "ondragend"


ondragenter :: Text -> Attribute
ondragenter = AttrRaw "ondragenter"


ondragleave :: Text -> Attribute
ondragleave = AttrRaw "ondragleave"


ondragover :: Text -> Attribute
ondragover = AttrRaw "ondragover"


ondragstart :: Text -> Attribute
ondragstart = AttrRaw "ondragstart"


ondrop :: Text -> Attribute
ondrop = AttrRaw "ondrop"


ondurationchange :: Text -> Attribute
ondurationchange = AttrRaw "ondurationchange"


onemptied :: Text -> Attribute
onemptied = AttrRaw "onemptied"


onended :: Text -> Attribute
onended = AttrRaw "onended"


onerror :: Text -> Attribute
onerror = AttrRaw "onerror"


onfocus :: Text -> Attribute
onfocus = AttrRaw "onfocus"


onformchange :: Text -> Attribute
onformchange = AttrRaw "onformchange"


onforminput :: Text -> Attribute
onforminput = AttrRaw "onforminput"


onhaschange :: Text -> Attribute
onhaschange = AttrRaw "onhaschange"


oninput :: Text -> Attribute
oninput = AttrRaw "oninput"


oninvalid :: Text -> Attribute
oninvalid = AttrRaw "oninvalid"


onkeydown :: Text -> Attribute
onkeydown = AttrRaw "onkeydown"


onkeyup :: Text -> Attribute
onkeyup = AttrRaw "onkeyup"


onload :: Text -> Attribute
onload = AttrRaw "onload"


onloadeddata :: Text -> Attribute
onloadeddata = AttrRaw "onloadeddata"


onloadedmetadata :: Text -> Attribute
onloadedmetadata = AttrRaw "onloadedmetadata"


onloadstart :: Text -> Attribute
onloadstart = AttrRaw "onloadstart"


onmessage :: Text -> Attribute
onmessage = AttrRaw "onmessage"


onmousedown :: Text -> Attribute
onmousedown = AttrRaw "onmousedown"


onmousemove :: Text -> Attribute
onmousemove = AttrRaw "onmousemove"


onmouseout :: Text -> Attribute
onmouseout = AttrRaw "onmouseout"


onmouseover :: Text -> Attribute
onmouseover = AttrRaw "onmouseover"


onmouseup :: Text -> Attribute
onmouseup = AttrRaw "onmouseup"


onmousewheel :: Text -> Attribute
onmousewheel = AttrRaw "onmousewheel"


ononline :: Text -> Attribute
ononline = AttrRaw "ononline"


onpagehide :: Text -> Attribute
onpagehide = AttrRaw "onpagehide"


onpageshow :: Text -> Attribute
onpageshow = AttrRaw "onpageshow"


onpause :: Text -> Attribute
onpause = AttrRaw "onpause"


onplay :: Text -> Attribute
onplay = AttrRaw "onplay"


onplaying :: Text -> Attribute
onplaying = AttrRaw "onplaying"


onprogress :: Text -> Attribute
onprogress = AttrRaw "onprogress"


onpropstate :: Text -> Attribute
onpropstate = AttrRaw "onpropstate"


onratechange :: Text -> Attribute
onratechange = AttrRaw "onratechange"


onreadystatechange :: Text -> Attribute
onreadystatechange = AttrRaw "onreadystatechange"


onredo :: Text -> Attribute
onredo = AttrRaw "onredo"


onresize :: Text -> Attribute
onresize = AttrRaw "onresize"


onscroll :: Text -> Attribute
onscroll = AttrRaw "onscroll"


onseeked :: Text -> Attribute
onseeked = AttrRaw "onseeked"


onseeking :: Text -> Attribute
onseeking = AttrRaw "onseeking"


onselect :: Text -> Attribute
onselect = AttrRaw "onselect"


onstalled :: Text -> Attribute
onstalled = AttrRaw "onstalled"


onstorage :: Text -> Attribute
onstorage = AttrRaw "onstorage"


onsubmit :: Text -> Attribute
onsubmit = AttrRaw "onsubmit"


onsuspend :: Text -> Attribute
onsuspend = AttrRaw "onsuspend"


ontimeupdate :: Text -> Attribute
ontimeupdate = AttrRaw "ontimeupdate"


onundo :: Text -> Attribute
onundo = AttrRaw "onundo"


onunload :: Text -> Attribute
onunload = AttrRaw "onunload"


onvolumechange :: Text -> Attribute
onvolumechange = AttrRaw "onvolumechange"


onwaiting :: Text -> Attribute
onwaiting = AttrRaw "onwaiting"


open :: Text -> Attribute
open = AttrRaw "open"


optimum :: Text -> Attribute
optimum = AttrRaw "optimum"


pattern :: Text -> Attribute
pattern = AttrRaw "pattern"


ping :: Text -> Attribute
ping = AttrRaw "ping"


placeholder :: Text -> Attribute
placeholder = AttrRaw "placeholder"


preload :: Text -> Attribute
preload = AttrRaw "preload"


pubdate :: Text -> Attribute
pubdate = AttrRaw "pubdate"


radiogroup :: Text -> Attribute
radiogroup = AttrRaw "radiogroup"


readonly :: Text -> Attribute
readonly = AttrRaw "readonly"


rel :: Text -> Attribute
rel = AttrRaw "rel"


required :: Text -> Attribute
required = AttrRaw "required"


reversed :: Text -> Attribute
reversed = AttrRaw "reversed"


role :: Text -> Attribute
role = AttrRaw "role"


rows :: Text -> Attribute
rows = AttrRaw "rows"


rowspan :: Text -> Attribute
rowspan = AttrRaw "rowspan"


sandbox :: Text -> Attribute
sandbox = AttrRaw "sandbox"


scope :: Text -> Attribute
scope = AttrRaw "scope"


scoped :: Text -> Attribute
scoped = AttrRaw "scoped"


seamless :: Text -> Attribute
seamless = AttrRaw "seamless"


selected :: Text -> Attribute
selected = AttrRaw "selected"


shape :: Text -> Attribute
shape = AttrRaw "shape"


size :: Text -> Attribute
size = AttrRaw "size"


sizes :: Text -> Attribute
sizes = AttrRaw "sizes"


span :: Text -> Attribute
span = AttrRaw "span"


spellcheck :: Text -> Attribute
spellcheck = AttrRaw "spellcheck"


src :: Text -> Attribute
src = AttrRaw "src"


srcdoc :: Text -> Attribute
srcdoc = AttrRaw "srcdoc"


start :: Text -> Attribute
start = AttrRaw "start"


step :: Text -> Attribute
step = AttrRaw "step"


style :: Text -> Attribute
style = AttrRaw "style"


subject :: Text -> Attribute
subject = AttrRaw "subject"


summary :: Text -> Attribute
summary = AttrRaw "summary"


tabindex :: Text -> Attribute
tabindex = AttrRaw "tabindex"


target :: Text -> Attribute
target = AttrRaw "target"


title :: Text -> Attribute
title = AttrRaw "title"


type_ :: Text -> Attribute
type_ = AttrRaw "type"


usemap :: Text -> Attribute
usemap = AttrRaw "usemap"


value :: Text -> Attribute
value = AttrRaw "value"


width :: Text -> Attribute
width = AttrRaw "width"


wrap :: Text -> Attribute
wrap = AttrRaw "wrap"


xmlns :: Text -> Attribute
xmlns = AttrRaw "xmlns"
