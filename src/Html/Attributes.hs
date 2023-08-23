{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The "Html.Attributes" module provides a set of functions for generating HTML attributes.
module Html.Attributes
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
    ) where


import Prelude (Bool(..))

import Data.Text.Lazy.Builder (Builder)
import Html (Attribute(..))


abbr :: Builder -> Attribute
abbr = TextAttribute " abbr=\""
{-# INLINE abbr #-}


accept :: Builder -> Attribute
accept = TextAttribute " accept=\""
{-# INLINE accept #-}


acceptCharset :: Builder -> Attribute
acceptCharset = TextAttribute " accept-charset=\""
{-# INLINE acceptCharset #-}


accesskey :: Builder -> Attribute
accesskey = TextAttribute " accesskey=\""
{-# INLINE accesskey #-}


action :: Builder -> Attribute
action = TextAttribute " action=\""
{-# INLINE action #-}


allow :: Builder -> Attribute
allow = TextAttribute " allow=\""
{-# INLINE allow #-}


allowfullscreen :: Bool -> Attribute
allowfullscreen = BoolAttribute " allowfullscreen"
{-# INLINE allowfullscreen #-}


alt :: Builder -> Attribute
alt = TextAttribute " alt=\""
{-# INLINE alt #-}


as :: Builder -> Attribute
as = TextAttribute " as=\""
{-# INLINE as #-}


async :: Bool -> Attribute
async = BoolAttribute " async"
{-# INLINE async #-}


autocapitalize :: Builder -> Attribute
autocapitalize = TextAttribute " autocapitalize=\""
{-# INLINE autocapitalize #-}


autocomplete :: Builder -> Attribute
autocomplete = TextAttribute " autocomplete=\""
{-# INLINE autocomplete #-}


autofocus :: Bool -> Attribute
autofocus = BoolAttribute " autofocus"
{-# INLINE autofocus #-}


autoplay :: Bool -> Attribute
autoplay = BoolAttribute " autoplay"
{-# INLINE autoplay #-}


blocking :: Builder -> Attribute
blocking = TextAttribute " blocking=\""
{-# INLINE blocking #-}


charset :: Builder -> Attribute
charset = TextAttribute " charset=\""
{-# INLINE charset #-}


checked :: Bool -> Attribute
checked = BoolAttribute " checked"
{-# INLINE checked #-}


cite :: Builder -> Attribute
cite = TextAttribute " cite=\""
{-# INLINE cite #-}


class_ :: Builder -> Attribute
class_ = TextAttribute " class=\""
{-# INLINE class_ #-}


color :: Builder -> Attribute
color = TextAttribute " color=\""
{-# INLINE color #-}


cols :: Builder -> Attribute
cols = TextAttribute " cols=\""
{-# INLINE cols #-}


colspan :: Builder -> Attribute
colspan = TextAttribute " colspan=\""
{-# INLINE colspan #-}


content :: Builder -> Attribute
content = TextAttribute " content=\""
{-# INLINE content #-}


contenteditable :: Builder -> Attribute
contenteditable = TextAttribute " contenteditable=\""
{-# INLINE contenteditable #-}


controls :: Bool -> Attribute
controls = BoolAttribute " controls"
{-# INLINE controls #-}


coords :: Builder -> Attribute
coords = TextAttribute " coords=\""
{-# INLINE coords #-}


crossorigin :: Builder -> Attribute
crossorigin = TextAttribute " crossorigin=\""
{-# INLINE crossorigin #-}


data_ :: Builder -> Attribute
data_ = TextAttribute " data=\""
{-# INLINE data_ #-}


datetime :: Builder -> Attribute
datetime = TextAttribute " datetime=\""
{-# INLINE datetime #-}


decoding :: Builder -> Attribute
decoding = TextAttribute " decoding=\""
{-# INLINE decoding #-}


default_ :: Bool -> Attribute
default_ = BoolAttribute " default"
{-# INLINE default_ #-}


defer :: Bool -> Attribute
defer = BoolAttribute " defer"
{-# INLINE defer #-}


dir :: Builder -> Attribute
dir = TextAttribute " dir=\""
{-# INLINE dir #-}


disabled :: Bool -> Attribute
disabled = BoolAttribute " disabled"
{-# INLINE disabled #-}


download :: Builder -> Attribute
download = TextAttribute " download=\""
{-# INLINE download #-}


draggable :: Builder -> Attribute
draggable = TextAttribute " draggable=\""
{-# INLINE draggable #-}


enctype :: Builder -> Attribute
enctype = TextAttribute " enctype=\""
{-# INLINE enctype #-}


enterkeyhint :: Builder -> Attribute
enterkeyhint = TextAttribute " enterkeyhint=\""
{-# INLINE enterkeyhint #-}


for :: Builder -> Attribute
for = TextAttribute " for=\""
{-# INLINE for #-}


form :: Builder -> Attribute
form = TextAttribute " form=\""
{-# INLINE form #-}


formaction :: Builder -> Attribute
formaction = TextAttribute " formaction=\""
{-# INLINE formaction #-}


formenctype :: Builder -> Attribute
formenctype = TextAttribute " formenctype=\""
{-# INLINE formenctype #-}


formmethod :: Builder -> Attribute
formmethod = TextAttribute " formmethod=\""
{-# INLINE formmethod #-}


formnovalidate :: Bool -> Attribute
formnovalidate = BoolAttribute " formnovalidate"
{-# INLINE formnovalidate #-}


formtarget :: Builder -> Attribute
formtarget = TextAttribute " formtarget=\""
{-# INLINE formtarget #-}


headers :: Builder -> Attribute
headers = TextAttribute " headers=\""
{-# INLINE headers #-}


height :: Builder -> Attribute
height = TextAttribute " height=\""
{-# INLINE height #-}


hidden :: Bool -> Attribute
hidden = BoolAttribute " hidden"
{-# INLINE hidden #-}


high :: Builder -> Attribute
high = TextAttribute " high=\""
{-# INLINE high #-}


href :: Builder -> Attribute
href = TextAttribute " href=\""
{-# INLINE href #-}


hreflang :: Builder -> Attribute
hreflang = TextAttribute " hreflang=\""
{-# INLINE hreflang #-}


httpEquiv :: Builder -> Attribute
httpEquiv = TextAttribute " http-equiv=\""
{-# INLINE httpEquiv #-}


id :: Builder -> Attribute
id = TextAttribute " id=\""
{-# INLINE id #-}


imagesizes :: Builder -> Attribute
imagesizes = TextAttribute " imagesizes=\""
{-# INLINE imagesizes #-}


imagesrcset :: Builder -> Attribute
imagesrcset = TextAttribute " imagesrcset=\""
{-# INLINE imagesrcset #-}


inert :: Bool -> Attribute
inert = BoolAttribute " inert"
{-# INLINE inert #-}


inputmode :: Builder -> Attribute
inputmode = TextAttribute " inputmode=\""
{-# INLINE inputmode #-}


integrity :: Builder -> Attribute
integrity = TextAttribute " integrity=\""
{-# INLINE integrity #-}


is :: Builder -> Attribute
is = TextAttribute " is=\""
{-# INLINE is #-}


ismap :: Bool -> Attribute
ismap = BoolAttribute " ismap"
{-# INLINE ismap #-}


itemid :: Builder -> Attribute
itemid = TextAttribute " itemid=\""
{-# INLINE itemid #-}


itemprop :: Builder -> Attribute
itemprop = TextAttribute " itemprop=\""
{-# INLINE itemprop #-}


itemref :: Builder -> Attribute
itemref = TextAttribute " itemref=\""
{-# INLINE itemref #-}


itemscope :: Bool -> Attribute
itemscope = BoolAttribute " itemscope"
{-# INLINE itemscope #-}


itemtype :: Builder -> Attribute
itemtype = TextAttribute " itemtype=\""
{-# INLINE itemtype #-}


kind :: Builder -> Attribute
kind = TextAttribute " kind=\""
{-# INLINE kind #-}


label :: Builder -> Attribute
label = TextAttribute " label=\""
{-# INLINE label #-}


lang :: Builder -> Attribute
lang = TextAttribute " lang=\""
{-# INLINE lang #-}


list :: Builder -> Attribute
list = TextAttribute " list=\""
{-# INLINE list #-}


loading :: Builder -> Attribute
loading = TextAttribute " loading=\""
{-# INLINE loading #-}


loop :: Bool -> Attribute
loop = BoolAttribute " loop"
{-# INLINE loop #-}


low :: Builder -> Attribute
low = TextAttribute " low=\""
{-# INLINE low #-}


max :: Builder -> Attribute
max = TextAttribute " max=\""
{-# INLINE max #-}


maxlength :: Builder -> Attribute
maxlength = TextAttribute " maxlength=\""
{-# INLINE maxlength #-}


media :: Builder -> Attribute
media = TextAttribute " media=\""
{-# INLINE media #-}


method :: Builder -> Attribute
method = TextAttribute " method=\""
{-# INLINE method #-}


min :: Builder -> Attribute
min = TextAttribute " min=\""
{-# INLINE min #-}


minlength :: Builder -> Attribute
minlength = TextAttribute " minlength=\""
{-# INLINE minlength #-}


multiple :: Bool -> Attribute
multiple = BoolAttribute " multiple"
{-# INLINE multiple #-}


muted :: Bool -> Attribute
muted = BoolAttribute " muted"
{-# INLINE muted #-}


name :: Builder -> Attribute
name = TextAttribute " name=\""
{-# INLINE name #-}


nomodule :: Bool -> Attribute
nomodule = BoolAttribute " nomodule"
{-# INLINE nomodule #-}


nonce :: Builder -> Attribute
nonce = TextAttribute " nonce=\""
{-# INLINE nonce #-}


novalidate :: Bool -> Attribute
novalidate = BoolAttribute " novalidate"
{-# INLINE novalidate #-}


open :: Bool -> Attribute
open = BoolAttribute " open"
{-# INLINE open #-}


optimum :: Builder -> Attribute
optimum = TextAttribute " optimum=\""
{-# INLINE optimum #-}


pattern :: Builder -> Attribute
pattern = TextAttribute " pattern=\""
{-# INLINE pattern #-}


ping :: Builder -> Attribute
ping = TextAttribute " ping=\""
{-# INLINE ping #-}


placeholder :: Builder -> Attribute
placeholder = TextAttribute " placeholder=\""
{-# INLINE placeholder #-}


playsinline :: Bool -> Attribute
playsinline = BoolAttribute " playsinline"
{-# INLINE playsinline #-}


poster :: Builder -> Attribute
poster = TextAttribute " poster=\""
{-# INLINE poster #-}


preload :: Builder -> Attribute
preload = TextAttribute " preload=\""
{-# INLINE preload #-}


readonly :: Bool -> Attribute
readonly = BoolAttribute " readonly"
{-# INLINE readonly #-}


referrerpolicy :: Builder -> Attribute
referrerpolicy = TextAttribute " referrerpolicy=\""
{-# INLINE referrerpolicy #-}


rel :: Builder -> Attribute
rel = TextAttribute " rel=\""
{-# INLINE rel #-}


required :: Bool -> Attribute
required = BoolAttribute " required"
{-# INLINE required #-}


reversed :: Bool -> Attribute
reversed = BoolAttribute " reversed"
{-# INLINE reversed #-}


rows :: Builder -> Attribute
rows = TextAttribute " rows=\""
{-# INLINE rows #-}


rowspan :: Builder -> Attribute
rowspan = TextAttribute " rowspan=\""
{-# INLINE rowspan #-}


sandbox :: Builder -> Attribute
sandbox = TextAttribute " sandbox=\""
{-# INLINE sandbox #-}


scope :: Builder -> Attribute
scope = TextAttribute " scope=\""
{-# INLINE scope #-}


selected :: Bool -> Attribute
selected = BoolAttribute " selected"
{-# INLINE selected #-}


shape :: Builder -> Attribute
shape = TextAttribute " shape=\""
{-# INLINE shape #-}


size :: Builder -> Attribute
size = TextAttribute " size=\""
{-# INLINE size #-}


sizes :: Builder -> Attribute
sizes = TextAttribute " sizes=\""
{-# INLINE sizes #-}


slot :: Builder -> Attribute
slot = TextAttribute " slot=\""
{-# INLINE slot #-}


span :: Builder -> Attribute
span = TextAttribute " span=\""
{-# INLINE span #-}


spellcheck :: Builder -> Attribute
spellcheck = TextAttribute " spellcheck=\""
{-# INLINE spellcheck #-}


src :: Builder -> Attribute
src = TextAttribute " src=\""
{-# INLINE src #-}


srcdoc :: Builder -> Attribute
srcdoc = TextAttribute " srcdoc=\""
{-# INLINE srcdoc #-}


srclang :: Builder -> Attribute
srclang = TextAttribute " srclang=\""
{-# INLINE srclang #-}


srcset :: Builder -> Attribute
srcset = TextAttribute " srcset=\""
{-# INLINE srcset #-}


start :: Builder -> Attribute
start = TextAttribute " start=\""
{-# INLINE start #-}


step :: Builder -> Attribute
step = TextAttribute " step=\""
{-# INLINE step #-}


style :: Builder -> Attribute
style = TextAttribute " style=\""
{-# INLINE style #-}


tabindex :: Builder -> Attribute
tabindex = TextAttribute " tabindex=\""
{-# INLINE tabindex #-}


target :: Builder -> Attribute
target = TextAttribute " target=\""
{-# INLINE target #-}


title :: Builder -> Attribute
title = TextAttribute " title=\""
{-# INLINE title #-}


translate :: Builder -> Attribute
translate = TextAttribute " translate=\""
{-# INLINE translate #-}


type_ :: Builder -> Attribute
type_ = TextAttribute " type=\""
{-# INLINE type_ #-}


usemap :: Builder -> Attribute
usemap = TextAttribute " usemap=\""
{-# INLINE usemap #-}


value :: Builder -> Attribute
value = TextAttribute " value=\""
{-# INLINE value #-}


width :: Builder -> Attribute
width = TextAttribute " width=\""
{-# INLINE width #-}


wrap :: Builder -> Attribute
wrap = TextAttribute " wrap=\""
{-# INLINE wrap #-}


onauxclick :: Builder -> Attribute
onauxclick = TextAttribute " onauxclick=\""
{-# INLINE onauxclick #-}


onafterprint :: Builder -> Attribute
onafterprint = TextAttribute " onafterprint=\""
{-# INLINE onafterprint #-}


onbeforematch :: Builder -> Attribute
onbeforematch = TextAttribute " onbeforematch=\""
{-# INLINE onbeforematch #-}


onbeforeprint :: Builder -> Attribute
onbeforeprint = TextAttribute " onbeforeprint=\""
{-# INLINE onbeforeprint #-}


onbeforeunload :: Builder -> Attribute
onbeforeunload = TextAttribute " onbeforeunload=\""
{-# INLINE onbeforeunload #-}


onblur :: Builder -> Attribute
onblur = TextAttribute " onblur=\""
{-# INLINE onblur #-}


oncancel :: Builder -> Attribute
oncancel = TextAttribute " oncancel=\""
{-# INLINE oncancel #-}


oncanplay :: Builder -> Attribute
oncanplay = TextAttribute " oncanplay=\""
{-# INLINE oncanplay #-}


oncanplaythrough :: Builder -> Attribute
oncanplaythrough = TextAttribute " oncanplaythrough=\""
{-# INLINE oncanplaythrough #-}


onchange :: Builder -> Attribute
onchange = TextAttribute " onchange=\""
{-# INLINE onchange #-}


onclick :: Builder -> Attribute
onclick = TextAttribute " onclick=\""
{-# INLINE onclick #-}


onclose :: Builder -> Attribute
onclose = TextAttribute " onclose=\""
{-# INLINE onclose #-}


oncontextlost :: Builder -> Attribute
oncontextlost = TextAttribute " oncontextlost=\""
{-# INLINE oncontextlost #-}


oncontextmenu :: Builder -> Attribute
oncontextmenu = TextAttribute " oncontextmenu=\""
{-# INLINE oncontextmenu #-}


oncontextrestored :: Builder -> Attribute
oncontextrestored = TextAttribute " oncontextrestored=\""
{-# INLINE oncontextrestored #-}


oncopy :: Builder -> Attribute
oncopy = TextAttribute " oncopy=\""
{-# INLINE oncopy #-}


oncuechange :: Builder -> Attribute
oncuechange = TextAttribute " oncuechange=\""
{-# INLINE oncuechange #-}


oncut :: Builder -> Attribute
oncut = TextAttribute " oncut=\""
{-# INLINE oncut #-}


ondblclick :: Builder -> Attribute
ondblclick = TextAttribute " ondblclick=\""
{-# INLINE ondblclick #-}


ondrag :: Builder -> Attribute
ondrag = TextAttribute " ondrag=\""
{-# INLINE ondrag #-}


ondragend :: Builder -> Attribute
ondragend = TextAttribute " ondragend=\""
{-# INLINE ondragend #-}


ondragenter :: Builder -> Attribute
ondragenter = TextAttribute " ondragenter=\""
{-# INLINE ondragenter #-}


ondragleave :: Builder -> Attribute
ondragleave = TextAttribute " ondragleave=\""
{-# INLINE ondragleave #-}


ondragover :: Builder -> Attribute
ondragover = TextAttribute " ondragover=\""
{-# INLINE ondragover #-}


ondragstart :: Builder -> Attribute
ondragstart = TextAttribute " ondragstart=\""
{-# INLINE ondragstart #-}


ondrop :: Builder -> Attribute
ondrop = TextAttribute " ondrop=\""
{-# INLINE ondrop #-}


ondurationchange :: Builder -> Attribute
ondurationchange = TextAttribute " ondurationchange=\""
{-# INLINE ondurationchange #-}


onemptied :: Builder -> Attribute
onemptied = TextAttribute " onemptied=\""
{-# INLINE onemptied #-}


onended :: Builder -> Attribute
onended = TextAttribute " onended=\""
{-# INLINE onended #-}


onerror :: Builder -> Attribute
onerror = TextAttribute " onerror=\""
{-# INLINE onerror #-}


onfocus :: Builder -> Attribute
onfocus = TextAttribute " onfocus=\""
{-# INLINE onfocus #-}


onformdata :: Builder -> Attribute
onformdata = TextAttribute " onformdata=\""
{-# INLINE onformdata #-}


onhashchange :: Builder -> Attribute
onhashchange = TextAttribute " onhashchange=\""
{-# INLINE onhashchange #-}


oninput :: Builder -> Attribute
oninput = TextAttribute " oninput=\""
{-# INLINE oninput #-}


oninvalid :: Builder -> Attribute
oninvalid = TextAttribute " oninvalid=\""
{-# INLINE oninvalid #-}


onkeydown :: Builder -> Attribute
onkeydown = TextAttribute " onkeydown=\""
{-# INLINE onkeydown #-}


onkeypress :: Builder -> Attribute
onkeypress = TextAttribute " onkeypress=\""
{-# INLINE onkeypress #-}


onkeyup :: Builder -> Attribute
onkeyup = TextAttribute " onkeyup=\""
{-# INLINE onkeyup #-}


onlanguagechange :: Builder -> Attribute
onlanguagechange = TextAttribute " onlanguagechange=\""
{-# INLINE onlanguagechange #-}


onload :: Builder -> Attribute
onload = TextAttribute " onload=\""
{-# INLINE onload #-}


onloadeddata :: Builder -> Attribute
onloadeddata = TextAttribute " onloadeddata=\""
{-# INLINE onloadeddata #-}


onloadedmetadata :: Builder -> Attribute
onloadedmetadata = TextAttribute " onloadedmetadata=\""
{-# INLINE onloadedmetadata #-}


onloadstart :: Builder -> Attribute
onloadstart = TextAttribute " onloadstart=\""
{-# INLINE onloadstart #-}


onmessage :: Builder -> Attribute
onmessage = TextAttribute " onmessage=\""
{-# INLINE onmessage #-}


onmessageerror :: Builder -> Attribute
onmessageerror = TextAttribute " onmessageerror=\""
{-# INLINE onmessageerror #-}


onmousedown :: Builder -> Attribute
onmousedown = TextAttribute " onmousedown=\""
{-# INLINE onmousedown #-}


onmouseenter :: Builder -> Attribute
onmouseenter = TextAttribute " onmouseenter=\""
{-# INLINE onmouseenter #-}


onmouseleave :: Builder -> Attribute
onmouseleave = TextAttribute " onmouseleave=\""
{-# INLINE onmouseleave #-}


onmousemove :: Builder -> Attribute
onmousemove = TextAttribute " onmousemove=\""
{-# INLINE onmousemove #-}


onmouseout :: Builder -> Attribute
onmouseout = TextAttribute " onmouseout=\""
{-# INLINE onmouseout #-}


onmouseover :: Builder -> Attribute
onmouseover = TextAttribute " onmouseover=\""
{-# INLINE onmouseover #-}


onmouseup :: Builder -> Attribute
onmouseup = TextAttribute " onmouseup=\""
{-# INLINE onmouseup #-}


onoffline :: Builder -> Attribute
onoffline = TextAttribute " onoffline=\""
{-# INLINE onoffline #-}


ononline :: Builder -> Attribute
ononline = TextAttribute " ononline=\""
{-# INLINE ononline #-}


onpagehide :: Builder -> Attribute
onpagehide = TextAttribute " onpagehide=\""
{-# INLINE onpagehide #-}


onpageshow :: Builder -> Attribute
onpageshow = TextAttribute " onpageshow=\""
{-# INLINE onpageshow #-}


onpaste :: Builder -> Attribute
onpaste = TextAttribute " onpaste=\""
{-# INLINE onpaste #-}


onpause :: Builder -> Attribute
onpause = TextAttribute " onpause=\""
{-# INLINE onpause #-}


onplay :: Builder -> Attribute
onplay = TextAttribute " onplay=\""
{-# INLINE onplay #-}


onplaying :: Builder -> Attribute
onplaying = TextAttribute " onplaying=\""
{-# INLINE onplaying #-}


onpopstate :: Builder -> Attribute
onpopstate = TextAttribute " onpopstate=\""
{-# INLINE onpopstate #-}


onprogress :: Builder -> Attribute
onprogress = TextAttribute " onprogress=\""
{-# INLINE onprogress #-}


onratechange :: Builder -> Attribute
onratechange = TextAttribute " onratechange=\""
{-# INLINE onratechange #-}


onreset :: Builder -> Attribute
onreset = TextAttribute " onreset=\""
{-# INLINE onreset #-}


onresize :: Builder -> Attribute
onresize = TextAttribute " onresize=\""
{-# INLINE onresize #-}


onrejectionhandled :: Builder -> Attribute
onrejectionhandled = TextAttribute " onrejectionhandled=\""
{-# INLINE onrejectionhandled #-}


onscroll :: Builder -> Attribute
onscroll = TextAttribute " onscroll=\""
{-# INLINE onscroll #-}


onsecuritypolicyviolation :: Builder -> Attribute
onsecuritypolicyviolation = TextAttribute " onsecuritypolicyviolation=\""
{-# INLINE onsecuritypolicyviolation #-}


onseeked :: Builder -> Attribute
onseeked = TextAttribute " onseeked=\""
{-# INLINE onseeked #-}


onseeking :: Builder -> Attribute
onseeking = TextAttribute " onseeking=\""
{-# INLINE onseeking #-}


onselect :: Builder -> Attribute
onselect = TextAttribute " onselect=\""
{-# INLINE onselect #-}


onslotchange :: Builder -> Attribute
onslotchange = TextAttribute " onslotchange=\""
{-# INLINE onslotchange #-}


onstalled :: Builder -> Attribute
onstalled = TextAttribute " onstalled=\""
{-# INLINE onstalled #-}


onstorage :: Builder -> Attribute
onstorage = TextAttribute " onstorage=\""
{-# INLINE onstorage #-}


onsubmit :: Builder -> Attribute
onsubmit = TextAttribute " onsubmit=\""
{-# INLINE onsubmit #-}


onsuspend :: Builder -> Attribute
onsuspend = TextAttribute " onsuspend=\""
{-# INLINE onsuspend #-}


ontimeupdate :: Builder -> Attribute
ontimeupdate = TextAttribute " ontimeupdate=\""
{-# INLINE ontimeupdate #-}


ontoggle :: Builder -> Attribute
ontoggle = TextAttribute " ontoggle=\""
{-# INLINE ontoggle #-}


onunhandledrejection :: Builder -> Attribute
onunhandledrejection = TextAttribute " onunhandledrejection=\""
{-# INLINE onunhandledrejection #-}


onunload :: Builder -> Attribute
onunload = TextAttribute " onunload=\""
{-# INLINE onunload #-}


onvolumechange :: Builder -> Attribute
onvolumechange = TextAttribute " onvolumechange=\""
{-# INLINE onvolumechange #-}


onwaiting :: Builder -> Attribute
onwaiting = TextAttribute " onwaiting=\""
{-# INLINE onwaiting #-}


onwheel :: Builder -> Attribute
onwheel = TextAttribute " onwheel=\""
{-# INLINE onwheel #-}
