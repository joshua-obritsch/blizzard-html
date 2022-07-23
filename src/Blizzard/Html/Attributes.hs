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
import Text.Blaze.Internal (attribute)

import qualified Text.Blaze.Html5.Attributes as Attr

import Blizzard.Internal.Html (boolAttribute, textAttribute)


abbr :: Text -> Maybe Attribute
abbr = textAttribute $ attribute "abbr" " abbr=\""


accept :: Text -> Maybe Attribute
accept = textAttribute $ attribute "accept" " accept=\""


acceptCharset :: Text -> Maybe Attribute
acceptCharset = textAttribute $ attribute "accept-charset" " accept-charset=\""


accesskey :: Text -> Maybe Attribute
accesskey = textAttribute $ attribute "accesskey" " accesskey=\""


action :: Text -> Maybe Attribute
action = textAttribute $ attribute "action" " action=\""


allow :: Text -> Maybe Attribute
allow = textAttribute $ attribute "allow" " allow=\""


allowfullscreen :: Bool -> Maybe Attribute
allowfullscreen = boolAttribute $ attribute "allowfullscreen" " allowfullscreen=\"" "allowfullscreen"


alt :: Text -> Maybe Attribute
alt = textAttribute $ attribute "alt" " alt=\""


as :: Text -> Maybe Attribute
as = textAttribute $ attribute "as" " as=\""


async :: Bool -> Maybe Attribute
async = boolAttribute $ attribute "async" " async=\"" "async"


autocapitalize :: Text -> Maybe Attribute
autocapitalize = textAttribute $ attribute "autocapitalize" " autocapitalize=\""


autocomplete :: Text -> Maybe Attribute
autocomplete = textAttribute $ attribute "autocomplete" " autocomplete=\""


autofocus :: Bool -> Maybe Attribute
autofocus = boolAttribute $ attribute "autofocus" " autofocus=\"" "autofocus"


autoplay :: Bool -> Maybe Attribute
autoplay = boolAttribute $ attribute "autoplay" " autoplay=\"" "autoplay"


blocking :: Text -> Maybe Attribute
blocking = textAttribute $ attribute "blocking" " blocking=\""


charset :: Text -> Maybe Attribute
charset = textAttribute $ attribute "charset" " charset=\""


checked :: Bool -> Maybe Attribute
checked = boolAttribute $ attribute "checked" " checked=\"" "checked"


cite :: Text -> Maybe Attribute
cite = textAttribute $ attribute "cite" " cite=\""


class_ :: Text -> Maybe Attribute
class_ = textAttribute $ attribute "class" " class=\""


color :: Text -> Maybe Attribute
color = textAttribute $ attribute "color" " color=\""


cols :: Text -> Maybe Attribute
cols = textAttribute $ attribute "cols" " cols=\""


colspan :: Text -> Maybe Attribute
colspan = textAttribute $ attribute "colspan" " colspan=\""


content :: Text -> Maybe Attribute
content = textAttribute $ attribute "content" " content=\""


contenteditable :: Text -> Maybe Attribute
contenteditable = textAttribute $ attribute "contenteditable" " contenteditable=\""


controls :: Bool -> Maybe Attribute
controls = boolAttribute $ attribute "controls" " controls=\"" "controls"


coords :: Text -> Maybe Attribute
coords = textAttribute $ attribute "coords" " coords=\""


crossorigin :: Text -> Maybe Attribute
crossorigin = textAttribute $ attribute "crossorigin" " crossorigin=\""


data_ :: Text -> Maybe Attribute
data_ = textAttribute $ attribute "data" " data=\""


datetime :: Text -> Maybe Attribute
datetime = textAttribute $ attribute "datetime" " datetime=\""


decoding :: Text -> Maybe Attribute
decoding = textAttribute $ attribute "decoding" " decoding=\""


default_ :: Bool -> Maybe Attribute
default_ = boolAttribute $ attribute "default" " default=\"" "default"


defer :: Bool -> Maybe Attribute
defer = boolAttribute $ attribute "defer" " defer=\"" "defer"


dir :: Text -> Maybe Attribute
dir = textAttribute $ attribute "dir" " dir=\""


disabled :: Bool -> Maybe Attribute
disabled = boolAttribute $ attribute "disabled" " disabled=\"" "disabled"


download :: Text -> Maybe Attribute
download = textAttribute $ attribute "download" " download=\""


draggable :: Text -> Maybe Attribute
draggable = textAttribute $ attribute "draggable" " draggable=\""


enctype :: Text -> Maybe Attribute
enctype = textAttribute $ attribute "enctype" " enctype=\""


enterkeyhint :: Text -> Maybe Attribute
enterkeyhint = textAttribute $ attribute "enterkeyhint" " enterkeyhint=\""


for :: Text -> Maybe Attribute
for = textAttribute $ attribute "for" " for=\""


form :: Text -> Maybe Attribute
form = textAttribute $ attribute "form" " form=\""


formaction :: Text -> Maybe Attribute
formaction = textAttribute $ attribute "formaction" " formaction=\""


formenctype :: Text -> Maybe Attribute
formenctype = textAttribute $ attribute "formenctype" " formenctype=\""


formmethod :: Text -> Maybe Attribute
formmethod = textAttribute $ attribute "formmethod" " formmethod=\""


formnovalidate :: Bool -> Maybe Attribute
formnovalidate = boolAttribute $ attribute "formnovalidate" " formnovalidate=\"" "formnovalidate"


formtarget :: Text -> Maybe Attribute
formtarget = textAttribute $ attribute "formtarget" " formtarget=\""


headers :: Text -> Maybe Attribute
headers = textAttribute $ attribute "headers" " headers=\""


height :: Text -> Maybe Attribute
height = textAttribute $ attribute "height" " height=\""


hidden :: Bool -> Maybe Attribute
hidden = boolAttribute $ attribute "hidden" " hidden=\"" "hidden"


high :: Text -> Maybe Attribute
high = textAttribute $ attribute "high" " high=\""


href :: Text -> Maybe Attribute
href = textAttribute $ attribute "href" " href=\""


hreflang :: Text -> Maybe Attribute
hreflang = textAttribute $ attribute "hreflang" " hreflang=\""


httpEquiv :: Text -> Maybe Attribute
httpEquiv = textAttribute $ attribute "http-equiv" " http-equiv=\""


id :: Text -> Maybe Attribute
id = textAttribute $ attribute "id" " id=\""


imagesizes :: Text -> Maybe Attribute
imagesizes = textAttribute $ attribute "imagesizes" " imagesizes=\""


imagesrcset :: Text -> Maybe Attribute
imagesrcset = textAttribute $ attribute "imagesrcset" " imagesrcset=\""


inert :: Bool -> Maybe Attribute
inert = boolAttribute $ attribute "inert" " inert=\"" "inert"


inputmode :: Text -> Maybe Attribute
inputmode = textAttribute $ attribute "inputmode" " inputmode=\""


integrity :: Text -> Maybe Attribute
integrity = textAttribute $ attribute "integrity" " integrity=\""


is :: Text -> Maybe Attribute
is = textAttribute $ attribute "is" " is=\""


ismap :: Bool -> Maybe Attribute
ismap = boolAttribute $ attribute "ismap" " ismap=\"" "ismap"


itemid :: Text -> Maybe Attribute
itemid = textAttribute $ attribute "itemid" " itemid=\""


itemprop :: Text -> Maybe Attribute
itemprop = textAttribute $ attribute "itemprop" " itemprop=\""


itemref :: Text -> Maybe Attribute
itemref = textAttribute $ attribute "itemref" " itemref=\""


itemscope :: Bool -> Maybe Attribute
itemscope = boolAttribute $ attribute "itemscope" " itemscope=\"" "itemscope"


itemtype :: Text -> Maybe Attribute
itemtype = textAttribute $ attribute "itemtype" " itemtype=\""


kind :: Text -> Maybe Attribute
kind = textAttribute $ attribute "kind" " kind=\""


label :: Text -> Maybe Attribute
label = textAttribute $ attribute "label" " label=\""


lang :: Text -> Maybe Attribute
lang = textAttribute $ attribute "lang" " lang=\""


list :: Text -> Maybe Attribute
list = textAttribute $ attribute "list" " list=\""


loading :: Text -> Maybe Attribute
loading = textAttribute $ attribute "loading" " loading=\""


loop :: Bool -> Maybe Attribute
loop = boolAttribute $ attribute "loop" " loop=\"" "loop"


low :: Text -> Maybe Attribute
low = textAttribute $ attribute "low" " low=\""


max :: Text -> Maybe Attribute
max = textAttribute $ attribute "max" " max=\""


maxlength :: Text -> Maybe Attribute
maxlength = textAttribute $ attribute "maxlength" " maxlength=\""


media :: Text -> Maybe Attribute
media = textAttribute $ attribute "media" " media=\""


method :: Text -> Maybe Attribute
method = textAttribute $ attribute "method" " method=\""


min :: Text -> Maybe Attribute
min = textAttribute $ attribute "min" " min=\""


minlength :: Text -> Maybe Attribute
minlength = textAttribute $ attribute "minlength" " minlength=\""


multiple :: Bool -> Maybe Attribute
multiple = boolAttribute $ attribute "multiple" " multiple=\"" "multiple"


muted :: Bool -> Maybe Attribute
muted = boolAttribute $ attribute "muted" " muted=\"" "muted"


name :: Text -> Maybe Attribute
name = textAttribute $ attribute "name" " name=\""


nomodule :: Bool -> Maybe Attribute
nomodule = boolAttribute $ attribute "nomodule" " nomodule=\"" "nomodule"


nonce :: Text -> Maybe Attribute
nonce = textAttribute $ attribute "nonce" " nonce=\""


novalidate :: Bool -> Maybe Attribute
novalidate = boolAttribute $ attribute "novalidate" " novalidate=\"" "novalidate"


open :: Bool -> Maybe Attribute
open = boolAttribute $ attribute "open" " open=\"" "open"


optimum :: Text -> Maybe Attribute
optimum = textAttribute $ attribute "optimum" " optimum=\""


pattern :: Text -> Maybe Attribute
pattern = textAttribute $ attribute "pattern" " pattern=\""


ping :: Text -> Maybe Attribute
ping = textAttribute $ attribute "ping" " ping=\""


placeholder :: Text -> Maybe Attribute
placeholder = textAttribute $ attribute "placeholder" " placeholder=\""


playsinline :: Bool -> Maybe Attribute
playsinline = boolAttribute $ attribute "playsinline" " playsinline=\"" "playsinline"


poster :: Text -> Maybe Attribute
poster = textAttribute $ attribute "poster" " poster=\""


preload :: Text -> Maybe Attribute
preload = textAttribute $ attribute "preload" " preload=\""


readonly :: Bool -> Maybe Attribute
readonly = boolAttribute $ attribute "readonly" " readonly=\"" "readonly"


referrerpolicy :: Text -> Maybe Attribute
referrerpolicy = textAttribute $ attribute "referrerpolicy" " referrerpolicy=\""


rel :: Text -> Maybe Attribute
rel = textAttribute $ attribute "rel" " rel=\""


required :: Bool -> Maybe Attribute
required = boolAttribute $ attribute "required" " required=\"" "required"


reversed :: Bool -> Maybe Attribute
reversed = boolAttribute $ attribute "reversed" " reversed=\"" "reversed"


rows :: Text -> Maybe Attribute
rows = textAttribute $ attribute "rows" " rows=\""


rowspan :: Text -> Maybe Attribute
rowspan = textAttribute $ attribute "rowspan" " rowspan=\""


sandbox :: Text -> Maybe Attribute
sandbox = textAttribute $ attribute "sandbox" " sandbox=\""


scope :: Text -> Maybe Attribute
scope = textAttribute $ attribute "scope" " scope=\""


selected :: Bool -> Maybe Attribute
selected = boolAttribute $ attribute "selected" " selected=\"" "selected"


shape :: Text -> Maybe Attribute
shape = textAttribute $ attribute "shape" " shape=\""


size :: Text -> Maybe Attribute
size = textAttribute $ attribute "size" " size=\""


sizes :: Text -> Maybe Attribute
sizes = textAttribute $ attribute "sizes" " sizes=\""


slot :: Text -> Maybe Attribute
slot = textAttribute $ attribute "slot" " slot=\""


span :: Text -> Maybe Attribute
span = textAttribute $ attribute "span" " span=\""


spellcheck :: Text -> Maybe Attribute
spellcheck = textAttribute $ attribute "spellcheck" " spellcheck=\""


src :: Text -> Maybe Attribute
src = textAttribute $ attribute "src" " src=\""


srcdoc :: Text -> Maybe Attribute
srcdoc = textAttribute $ attribute "srcdoc" " srcdoc=\""


srclang :: Text -> Maybe Attribute
srclang = textAttribute $ attribute "srclang" " srclang=\""


srcset :: Text -> Maybe Attribute
srcset = textAttribute $ attribute "srcset" " srcset=\""


start :: Text -> Maybe Attribute
start = textAttribute $ attribute "start" " start=\""


step :: Text -> Maybe Attribute
step = textAttribute $ attribute "step" " step=\""


style :: [Text] -> Maybe Attribute
style = textAttribute (attribute "style" " style=\"") . unwords


tabindex :: Text -> Maybe Attribute
tabindex = textAttribute $ attribute "tabindex" " tabindex=\""


target :: Text -> Maybe Attribute
target = textAttribute $ attribute "target" " target=\""


title :: Text -> Maybe Attribute
title = textAttribute $ attribute "title" " title=\""


translate :: Text -> Maybe Attribute
translate = textAttribute $ attribute "translate" " translate=\""


type_ :: Text -> Maybe Attribute
type_ = textAttribute $ attribute "type" " type=\""


usemap :: Text -> Maybe Attribute
usemap = textAttribute $ attribute "usemap" " usemap=\""


value :: Text -> Maybe Attribute
value = textAttribute $ attribute "value" " value=\""


width :: Text -> Maybe Attribute
width = textAttribute $ attribute "width" " width=\""


wrap :: Text -> Maybe Attribute
wrap = textAttribute $ attribute "wrap" " wrap=\""



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
