{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Blizzard.Html.Attributes
    ( -- * Attributes (Excluding Event Handler Content Attributes)
      abbr
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

      -- * Event Handler Content Attributes
    , onauxclick
    , onafterprint
    , onbeforematch
    , onbeforeprint
    , onbeforeunload
    , onblur
    , oncancel
    , oncanplay
    , oncanplaythrough
    , onchange
    , onclick
    , onclose
    , oncontextlost
    , oncontextmenu
    , oncontextrestored
    , oncopy
    , oncuechange
    , oncut
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
    , onformdata
    , onhashchange
    , oninput
    , oninvalid
    , onkeydown
    , onkeypress
    , onkeyup
    , onlanguagechange
    , onload
    , onloadeddata
    , onloadedmetadata
    , onloadstart
    , onmessage
    , onmessageerror
    , onmousedown
    , onmouseenter
    , onmouseleave
    , onmousemove
    , onmouseout
    , onmouseover
    , onmouseup
    , onoffline
    , ononline
    , onpagehide
    , onpageshow
    , onpaste
    , onpause
    , onplay
    , onplaying
    , onpopstate
    , onprogress
    , onratechange
    , onreset
    , onresize
    , onrejectionhandled
    , onscroll
    , onsecuritypolicyviolation
    , onseeked
    , onseeking
    , onselect
    , onslotchange
    , onstalled
    , onstorage
    , onsubmit
    , onsuspend
    , ontimeupdate
    , ontoggle
    , onunhandledrejection
    , onunload
    , onvolumechange
    , onwaiting
    , onwheel

    -- * Tailwind CSS
    , tailwind
    ) where


import Data.Text (Text, unwords)
import Prelude (($), (.), Bool, Maybe)
import Text.Blaze.Internal (Attribute, attribute)

import Blizzard.Internal.Html (boolAttribute, textAttribute)


abbr :: Text -> Maybe Attribute
abbr = textAttribute $ attribute "abbr" " abbr=\""
{-# INLINE abbr #-}


accept :: Text -> Maybe Attribute
accept = textAttribute $ attribute "accept" " accept=\""
{-# INLINE accept #-}


acceptCharset :: Text -> Maybe Attribute
acceptCharset = textAttribute $ attribute "accept-charset" " accept-charset=\""
{-# INLINE acceptCharset #-}


accesskey :: Text -> Maybe Attribute
accesskey = textAttribute $ attribute "accesskey" " accesskey=\""
{-# INLINE accesskey #-}


action :: Text -> Maybe Attribute
action = textAttribute $ attribute "action" " action=\""
{-# INLINE action #-}


allow :: Text -> Maybe Attribute
allow = textAttribute $ attribute "allow" " allow=\""
{-# INLINE allow #-}


allowfullscreen :: Bool -> Maybe Attribute
allowfullscreen = boolAttribute $ attribute "allowfullscreen" " allowfullscreen=\"" "allowfullscreen"
{-# INLINE allowfullscreen #-}


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


style :: Text -> Maybe Attribute
style = textAttribute $ attribute "style" " style=\""


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


onauxclick :: Text -> Maybe Attribute
onauxclick = textAttribute $ attribute "onauxclick" " onauxclick=\""


onafterprint :: Text -> Maybe Attribute
onafterprint = textAttribute $ attribute "onafterprint" " onafterprint=\""


onbeforematch :: Text -> Maybe Attribute
onbeforematch = textAttribute $ attribute "onbeforematch" " onbeforematch=\""


onbeforeprint :: Text -> Maybe Attribute
onbeforeprint = textAttribute $ attribute "onbeforeprint" " onbeforeprint=\""


onbeforeunload :: Text -> Maybe Attribute
onbeforeunload = textAttribute $ attribute "onbeforeunload" " onbeforeunload=\""


onblur :: Text -> Maybe Attribute
onblur = textAttribute $ attribute "onblur" " onblur=\""


oncancel :: Text -> Maybe Attribute
oncancel = textAttribute $ attribute "oncancel" " oncancel=\""


oncanplay :: Text -> Maybe Attribute
oncanplay = textAttribute $ attribute "oncanplay" " oncanplay=\""


oncanplaythrough :: Text -> Maybe Attribute
oncanplaythrough = textAttribute $ attribute "oncanplaythrough" " oncanplaythrough=\""


onchange :: Text -> Maybe Attribute
onchange = textAttribute $ attribute "onchange" " onchange=\""


onclick :: Text -> Maybe Attribute
onclick = textAttribute $ attribute "onclick" " onclick=\""


onclose :: Text -> Maybe Attribute
onclose = textAttribute $ attribute "onclose" " onclose=\""


oncontextlost :: Text -> Maybe Attribute
oncontextlost = textAttribute $ attribute "oncontextlost" " oncontextlost=\""


oncontextmenu :: Text -> Maybe Attribute
oncontextmenu = textAttribute $ attribute "oncontextmenu" " oncontextmenu=\""


oncontextrestored :: Text -> Maybe Attribute
oncontextrestored = textAttribute $ attribute "oncontextrestored" " oncontextrestored=\""


oncopy :: Text -> Maybe Attribute
oncopy = textAttribute $ attribute "oncopy" " oncopy=\""


oncuechange :: Text -> Maybe Attribute
oncuechange = textAttribute $ attribute "oncuechange" " oncuechange=\""


oncut :: Text -> Maybe Attribute
oncut = textAttribute $ attribute "oncut" " oncut=\""


ondblclick :: Text -> Maybe Attribute
ondblclick = textAttribute $ attribute "ondblclick" " ondblclick=\""


ondrag :: Text -> Maybe Attribute
ondrag = textAttribute $ attribute "ondrag" " ondrag=\""


ondragend :: Text -> Maybe Attribute
ondragend = textAttribute $ attribute "ondragend" " ondragend=\""


ondragenter :: Text -> Maybe Attribute
ondragenter = textAttribute $ attribute "ondragenter" " ondragenter=\""


ondragleave :: Text -> Maybe Attribute
ondragleave = textAttribute $ attribute "ondragleave" " ondragleave=\""


ondragover :: Text -> Maybe Attribute
ondragover = textAttribute $ attribute "ondragover" " ondragover=\""


ondragstart :: Text -> Maybe Attribute
ondragstart = textAttribute $ attribute "ondragstart" " ondragstart=\""


ondrop :: Text -> Maybe Attribute
ondrop = textAttribute $ attribute "ondrop" " ondrop=\""


ondurationchange :: Text -> Maybe Attribute
ondurationchange = textAttribute $ attribute "ondurationchange" " ondurationchange=\""


onemptied :: Text -> Maybe Attribute
onemptied = textAttribute $ attribute "onemptied" " onemptied=\""


onended :: Text -> Maybe Attribute
onended = textAttribute $ attribute "onended" " onended=\""


onerror :: Text -> Maybe Attribute
onerror = textAttribute $ attribute "onerror" " onerror=\""


onfocus :: Text -> Maybe Attribute
onfocus = textAttribute $ attribute "onfocus" " onfocus=\""


onformdata :: Text -> Maybe Attribute
onformdata = textAttribute $ attribute "onformdata" " onformdata=\""


onhashchange :: Text -> Maybe Attribute
onhashchange = textAttribute $ attribute "onhashchange" " onhashchange=\""


oninput :: Text -> Maybe Attribute
oninput = textAttribute $ attribute "oninput" " oninput=\""


oninvalid :: Text -> Maybe Attribute
oninvalid = textAttribute $ attribute "oninvalid" " oninvalid=\""


onkeydown :: Text -> Maybe Attribute
onkeydown = textAttribute $ attribute "onkeydown" " onkeydown=\""


onkeypress :: Text -> Maybe Attribute
onkeypress = textAttribute $ attribute "onkeypress" " onkeypress=\""


onkeyup :: Text -> Maybe Attribute
onkeyup = textAttribute $ attribute "onkeyup" " onkeyup=\""


onlanguagechange :: Text -> Maybe Attribute
onlanguagechange = textAttribute $ attribute "onlanguagechange" " onlanguagechange=\""


onload :: Text -> Maybe Attribute
onload = textAttribute $ attribute "onload" " onload=\""


onloadeddata :: Text -> Maybe Attribute
onloadeddata = textAttribute $ attribute "onloadeddata" " onloadeddata=\""


onloadedmetadata :: Text -> Maybe Attribute
onloadedmetadata = textAttribute $ attribute "onloadedmetadata" " onloadedmetadata=\""


onloadstart :: Text -> Maybe Attribute
onloadstart = textAttribute $ attribute "onloadstart" " onloadstart=\""


onmessage :: Text -> Maybe Attribute
onmessage = textAttribute $ attribute "onmessage" " onmessage=\""


onmessageerror :: Text -> Maybe Attribute
onmessageerror = textAttribute $ attribute "onmessageerror" " onmessageerror=\""


onmousedown :: Text -> Maybe Attribute
onmousedown = textAttribute $ attribute "onmousedown" " onmousedown=\""


onmouseenter :: Text -> Maybe Attribute
onmouseenter = textAttribute $ attribute "onmouseenter" " onmouseenter=\""


onmouseleave :: Text -> Maybe Attribute
onmouseleave = textAttribute $ attribute "onmouseleave" " onmouseleave=\""


onmousemove :: Text -> Maybe Attribute
onmousemove = textAttribute $ attribute "onmousemove" " onmousemove=\""


onmouseout :: Text -> Maybe Attribute
onmouseout = textAttribute $ attribute "onmouseout" " onmouseout=\""


onmouseover :: Text -> Maybe Attribute
onmouseover = textAttribute $ attribute "onmouseover" " onmouseover=\""


onmouseup :: Text -> Maybe Attribute
onmouseup = textAttribute $ attribute "onmouseup" " onmouseup=\""


onoffline :: Text -> Maybe Attribute
onoffline = textAttribute $ attribute "onoffline" " onoffline=\""


ononline :: Text -> Maybe Attribute
ononline = textAttribute $ attribute "ononline" " ononline=\""


onpagehide :: Text -> Maybe Attribute
onpagehide = textAttribute $ attribute "onpagehide" " onpagehide=\""


onpageshow :: Text -> Maybe Attribute
onpageshow = textAttribute $ attribute "onpageshow" " onpageshow=\""


onpaste :: Text -> Maybe Attribute
onpaste = textAttribute $ attribute "onpaste" " onpaste=\""


onpause :: Text -> Maybe Attribute
onpause = textAttribute $ attribute "onpause" " onpause=\""


onplay :: Text -> Maybe Attribute
onplay = textAttribute $ attribute "onplay" " onplay=\""


onplaying :: Text -> Maybe Attribute
onplaying = textAttribute $ attribute "onplaying" " onplaying=\""


onpopstate :: Text -> Maybe Attribute
onpopstate = textAttribute $ attribute "onpopstate" " onpopstate=\""


onprogress :: Text -> Maybe Attribute
onprogress = textAttribute $ attribute "onprogress" " onprogress=\""


onratechange :: Text -> Maybe Attribute
onratechange = textAttribute $ attribute "onratechange" " onratechange=\""


onreset :: Text -> Maybe Attribute
onreset = textAttribute $ attribute "onreset" " onreset=\""


onresize :: Text -> Maybe Attribute
onresize = textAttribute $ attribute "onresize" " onresize=\""


onrejectionhandled :: Text -> Maybe Attribute
onrejectionhandled = textAttribute $ attribute "onrejectionhandled" " onrejectionhandled=\""


onscroll :: Text -> Maybe Attribute
onscroll = textAttribute $ attribute "onscroll" " onscroll=\""


onsecuritypolicyviolation :: Text -> Maybe Attribute
onsecuritypolicyviolation = textAttribute $ attribute "onsecuritypolicyviolation" " onsecuritypolicyviolation=\""


onseeked :: Text -> Maybe Attribute
onseeked = textAttribute $ attribute "onseeked" " onseeked=\""


onseeking :: Text -> Maybe Attribute
onseeking = textAttribute $ attribute "onseeking" " onseeking=\""


onselect :: Text -> Maybe Attribute
onselect = textAttribute $ attribute "onselect" " onselect=\""


onslotchange :: Text -> Maybe Attribute
onslotchange = textAttribute $ attribute "onslotchange" " onslotchange=\""


onstalled :: Text -> Maybe Attribute
onstalled = textAttribute $ attribute "onstalled" " onstalled=\""


onstorage :: Text -> Maybe Attribute
onstorage = textAttribute $ attribute "onstorage" " onstorage=\""


onsubmit :: Text -> Maybe Attribute
onsubmit = textAttribute $ attribute "onsubmit" " onsubmit=\""


onsuspend :: Text -> Maybe Attribute
onsuspend = textAttribute $ attribute "onsuspend" " onsuspend=\""


ontimeupdate :: Text -> Maybe Attribute
ontimeupdate = textAttribute $ attribute "ontimeupdate" " ontimeupdate=\""


ontoggle :: Text -> Maybe Attribute
ontoggle = textAttribute $ attribute "ontoggle" " ontoggle=\""


onunhandledrejection :: Text -> Maybe Attribute
onunhandledrejection = textAttribute $ attribute "onunhandledrejection" " onunhandledrejection=\""


onunload :: Text -> Maybe Attribute
onunload = textAttribute $ attribute "onunload" " onunload=\""


onvolumechange :: Text -> Maybe Attribute
onvolumechange = textAttribute $ attribute "onvolumechange" " onvolumechange=\""


onwaiting :: Text -> Maybe Attribute
onwaiting = textAttribute $ attribute "onwaiting" " onwaiting=\""


onwheel :: Text -> Maybe Attribute
onwheel = textAttribute $ attribute "onwheel" " onwheel=\""


tailwind :: [Text] -> Maybe Attribute
tailwind = textAttribute (attribute "class" " class=\"") . unwords
