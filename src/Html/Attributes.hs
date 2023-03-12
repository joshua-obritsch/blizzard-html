{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Html.Attributes
    ( Attribute
      -- * Attributes (Excluding Event Handler Content Attributes)
    , abbr
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

import Prelude ((.), Bool(..), map, mconcat)

import Data.Text.Lazy.Builder (Builder)
import Html.Internal (Buildable(..))


data Attribute
    = BoolAttr Builder Bool
    | TextAttr Builder Builder


instance Buildable Attribute where
    build (BoolAttr _   False) = ""
    build (BoolAttr key True ) = key
    build (TextAttr _   ""   ) = ""
    build (TextAttr key value) = mconcat [ key, value, "\"" ]


instance Buildable [Attribute] where
    build = mconcat . map build


abbr :: Builder -> Attribute
abbr = TextAttr " abbr=\""
{-# INLINE abbr #-}


accept :: Builder -> Attribute
accept = TextAttr " accept=\""
{-# INLINE accept #-}


acceptCharset :: Builder -> Attribute
acceptCharset = TextAttr " accept-charset=\""
{-# INLINE acceptCharset #-}


accesskey :: Builder -> Attribute
accesskey = TextAttr " accesskey=\""
{-# INLINE accesskey #-}


action :: Builder -> Attribute
action = TextAttr " action=\""
{-# INLINE action #-}


allow :: Builder -> Attribute
allow = TextAttr " allow=\""
{-# INLINE allow #-}


allowfullscreen :: Bool -> Attribute
allowfullscreen = BoolAttr " allowfullscreen=\""
{-# INLINE allowfullscreen #-}


alt :: Builder -> Attribute
alt = TextAttr " alt=\""
{-# INLINE alt #-}


as :: Builder -> Attribute
as = TextAttr " as=\""
{-# INLINE as #-}


async :: Bool -> Attribute
async = BoolAttr " async=\""
{-# INLINE async #-}


autocapitalize :: Builder -> Attribute
autocapitalize = TextAttr " autocapitalize=\""
{-# INLINE autocapitalize #-}


autocomplete :: Builder -> Attribute
autocomplete = TextAttr " autocomplete=\""
{-# INLINE autocomplete #-}


autofocus :: Bool -> Attribute
autofocus = BoolAttr " autofocus=\""
{-# INLINE autofocus #-}


autoplay :: Bool -> Attribute
autoplay = BoolAttr " autoplay=\""
{-# INLINE autoplay #-}


blocking :: Builder -> Attribute
blocking = TextAttr " blocking=\""


charset :: Builder -> Attribute
charset = TextAttr " charset=\""


checked :: Bool -> Attribute
checked = BoolAttr " checked=\""


cite :: Builder -> Attribute
cite = TextAttr " cite=\""


class_ :: Builder -> Attribute
class_ = TextAttr " class=\""


color :: Builder -> Attribute
color = TextAttr " color=\""


cols :: Builder -> Attribute
cols = TextAttr " cols=\""


colspan :: Builder -> Attribute
colspan = TextAttr " colspan=\""


content :: Builder -> Attribute
content = TextAttr " content=\""


contenteditable :: Builder -> Attribute
contenteditable = TextAttr " contenteditable=\""


controls :: Bool -> Attribute
controls = BoolAttr " controls=\""


coords :: Builder -> Attribute
coords = TextAttr " coords=\""


crossorigin :: Builder -> Attribute
crossorigin = TextAttr " crossorigin=\""


data_ :: Builder -> Attribute
data_ = TextAttr " data=\""


datetime :: Builder -> Attribute
datetime = TextAttr " datetime=\""


decoding :: Builder -> Attribute
decoding = TextAttr " decoding=\""


default_ :: Bool -> Attribute
default_ = BoolAttr " default=\""


defer :: Bool -> Attribute
defer = BoolAttr " defer=\""


dir :: Builder -> Attribute
dir = TextAttr " dir=\""


disabled :: Bool -> Attribute
disabled = BoolAttr " disabled=\""


download :: Builder -> Attribute
download = TextAttr " download=\""


draggable :: Builder -> Attribute
draggable = TextAttr " draggable=\""


enctype :: Builder -> Attribute
enctype = TextAttr " enctype=\""


enterkeyhint :: Builder -> Attribute
enterkeyhint = TextAttr " enterkeyhint=\""


for :: Builder -> Attribute
for = TextAttr " for=\""


form :: Builder -> Attribute
form = TextAttr " form=\""


formaction :: Builder -> Attribute
formaction = TextAttr " formaction=\""


formenctype :: Builder -> Attribute
formenctype = TextAttr " formenctype=\""


formmethod :: Builder -> Attribute
formmethod = TextAttr " formmethod=\""


formnovalidate :: Bool -> Attribute
formnovalidate = BoolAttr " formnovalidate=\""


formtarget :: Builder -> Attribute
formtarget = TextAttr " formtarget=\""


headers :: Builder -> Attribute
headers = TextAttr " headers=\""


height :: Builder -> Attribute
height = TextAttr " height=\""


hidden :: Bool -> Attribute
hidden = BoolAttr " hidden=\""


high :: Builder -> Attribute
high = TextAttr " high=\""


href :: Builder -> Attribute
href = TextAttr " href=\""


hreflang :: Builder -> Attribute
hreflang = TextAttr " hreflang=\""


httpEquiv :: Builder -> Attribute
httpEquiv = TextAttr " http-equiv=\""


id :: Builder -> Attribute
id = TextAttr " id=\""


imagesizes :: Builder -> Attribute
imagesizes = TextAttr " imagesizes=\""


imagesrcset :: Builder -> Attribute
imagesrcset = TextAttr " imagesrcset=\""


inert :: Bool -> Attribute
inert = BoolAttr " inert=\""


inputmode :: Builder -> Attribute
inputmode = TextAttr " inputmode=\""


integrity :: Builder -> Attribute
integrity = TextAttr " integrity=\""


is :: Builder -> Attribute
is = TextAttr " is=\""


ismap :: Bool -> Attribute
ismap = BoolAttr " ismap=\""


itemid :: Builder -> Attribute
itemid = TextAttr " itemid=\""


itemprop :: Builder -> Attribute
itemprop = TextAttr " itemprop=\""


itemref :: Builder -> Attribute
itemref = TextAttr " itemref=\""


itemscope :: Bool -> Attribute
itemscope = BoolAttr " itemscope=\""


itemtype :: Builder -> Attribute
itemtype = TextAttr " itemtype=\""


kind :: Builder -> Attribute
kind = TextAttr " kind=\""


label :: Builder -> Attribute
label = TextAttr " label=\""


lang :: Builder -> Attribute
lang = TextAttr " lang=\""


list :: Builder -> Attribute
list = TextAttr " list=\""


loading :: Builder -> Attribute
loading = TextAttr " loading=\""


loop :: Bool -> Attribute
loop = BoolAttr " loop=\""


low :: Builder -> Attribute
low = TextAttr " low=\""


max :: Builder -> Attribute
max = TextAttr " max=\""


maxlength :: Builder -> Attribute
maxlength = TextAttr " maxlength=\""


media :: Builder -> Attribute
media = TextAttr " media=\""


method :: Builder -> Attribute
method = TextAttr " method=\""


min :: Builder -> Attribute
min = TextAttr " min=\""


minlength :: Builder -> Attribute
minlength = TextAttr " minlength=\""


multiple :: Bool -> Attribute
multiple = BoolAttr " multiple=\""


muted :: Bool -> Attribute
muted = BoolAttr " muted=\""


name :: Builder -> Attribute
name = TextAttr " name=\""


nomodule :: Bool -> Attribute
nomodule = BoolAttr " nomodule=\""


nonce :: Builder -> Attribute
nonce = TextAttr " nonce=\""


novalidate :: Bool -> Attribute
novalidate = BoolAttr " novalidate=\""


open :: Bool -> Attribute
open = BoolAttr " open=\""


optimum :: Builder -> Attribute
optimum = TextAttr " optimum=\""


pattern :: Builder -> Attribute
pattern = TextAttr " pattern=\""


ping :: Builder -> Attribute
ping = TextAttr " ping=\""


placeholder :: Builder -> Attribute
placeholder = TextAttr " placeholder=\""


playsinline :: Bool -> Attribute
playsinline = BoolAttr " playsinline=\""


poster :: Builder -> Attribute
poster = TextAttr " poster=\""


preload :: Builder -> Attribute
preload = TextAttr " preload=\""


readonly :: Bool -> Attribute
readonly = BoolAttr " readonly=\""


referrerpolicy :: Builder -> Attribute
referrerpolicy = TextAttr " referrerpolicy=\""


rel :: Builder -> Attribute
rel = TextAttr " rel=\""


required :: Bool -> Attribute
required = BoolAttr " required=\""


reversed :: Bool -> Attribute
reversed = BoolAttr " reversed=\""


rows :: Builder -> Attribute
rows = TextAttr " rows=\""


rowspan :: Builder -> Attribute
rowspan = TextAttr " rowspan=\""


sandbox :: Builder -> Attribute
sandbox = TextAttr " sandbox=\""


scope :: Builder -> Attribute
scope = TextAttr " scope=\""


selected :: Bool -> Attribute
selected = BoolAttr " selected=\""


shape :: Builder -> Attribute
shape = TextAttr " shape=\""


size :: Builder -> Attribute
size = TextAttr " size=\""


sizes :: Builder -> Attribute
sizes = TextAttr " sizes=\""


slot :: Builder -> Attribute
slot = TextAttr " slot=\""


span :: Builder -> Attribute
span = TextAttr " span=\""


spellcheck :: Builder -> Attribute
spellcheck = TextAttr " spellcheck=\""


src :: Builder -> Attribute
src = TextAttr " src=\""


srcdoc :: Builder -> Attribute
srcdoc = TextAttr " srcdoc=\""


srclang :: Builder -> Attribute
srclang = TextAttr " srclang=\""


srcset :: Builder -> Attribute
srcset = TextAttr " srcset=\""


start :: Builder -> Attribute
start = TextAttr " start=\""


step :: Builder -> Attribute
step = TextAttr " step=\""


style :: Builder -> Attribute
style = TextAttr " style=\""


tabindex :: Builder -> Attribute
tabindex = TextAttr " tabindex=\""


target :: Builder -> Attribute
target = TextAttr " target=\""


title :: Builder -> Attribute
title = TextAttr " title=\""


translate :: Builder -> Attribute
translate = TextAttr " translate=\""


type_ :: Builder -> Attribute
type_ = TextAttr " type=\""


usemap :: Builder -> Attribute
usemap = TextAttr " usemap=\""


value :: Builder -> Attribute
value = TextAttr " value=\""


width :: Builder -> Attribute
width = TextAttr " width=\""


wrap :: Builder -> Attribute
wrap = TextAttr " wrap=\""


onauxclick :: Builder -> Attribute
onauxclick = TextAttr " onauxclick=\""


onafterprint :: Builder -> Attribute
onafterprint = TextAttr " onafterprint=\""


onbeforematch :: Builder -> Attribute
onbeforematch = TextAttr " onbeforematch=\""


onbeforeprint :: Builder -> Attribute
onbeforeprint = TextAttr " onbeforeprint=\""


onbeforeunload :: Builder -> Attribute
onbeforeunload = TextAttr " onbeforeunload=\""


onblur :: Builder -> Attribute
onblur = TextAttr " onblur=\""


oncancel :: Builder -> Attribute
oncancel = TextAttr " oncancel=\""


oncanplay :: Builder -> Attribute
oncanplay = TextAttr " oncanplay=\""


oncanplaythrough :: Builder -> Attribute
oncanplaythrough = TextAttr " oncanplaythrough=\""


onchange :: Builder -> Attribute
onchange = TextAttr " onchange=\""


onclick :: Builder -> Attribute
onclick = TextAttr " onclick=\""


onclose :: Builder -> Attribute
onclose = TextAttr " onclose=\""


oncontextlost :: Builder -> Attribute
oncontextlost = TextAttr " oncontextlost=\""


oncontextmenu :: Builder -> Attribute
oncontextmenu = TextAttr " oncontextmenu=\""


oncontextrestored :: Builder -> Attribute
oncontextrestored = TextAttr " oncontextrestored=\""


oncopy :: Builder -> Attribute
oncopy = TextAttr " oncopy=\""


oncuechange :: Builder -> Attribute
oncuechange = TextAttr " oncuechange=\""


oncut :: Builder -> Attribute
oncut = TextAttr " oncut=\""


ondblclick :: Builder -> Attribute
ondblclick = TextAttr " ondblclick=\""


ondrag :: Builder -> Attribute
ondrag = TextAttr " ondrag=\""


ondragend :: Builder -> Attribute
ondragend = TextAttr " ondragend=\""


ondragenter :: Builder -> Attribute
ondragenter = TextAttr " ondragenter=\""


ondragleave :: Builder -> Attribute
ondragleave = TextAttr " ondragleave=\""


ondragover :: Builder -> Attribute
ondragover = TextAttr " ondragover=\""


ondragstart :: Builder -> Attribute
ondragstart = TextAttr " ondragstart=\""


ondrop :: Builder -> Attribute
ondrop = TextAttr " ondrop=\""


ondurationchange :: Builder -> Attribute
ondurationchange = TextAttr " ondurationchange=\""


onemptied :: Builder -> Attribute
onemptied = TextAttr " onemptied=\""


onended :: Builder -> Attribute
onended = TextAttr " onended=\""


onerror :: Builder -> Attribute
onerror = TextAttr " onerror=\""


onfocus :: Builder -> Attribute
onfocus = TextAttr " onfocus=\""


onformdata :: Builder -> Attribute
onformdata = TextAttr " onformdata=\""


onhashchange :: Builder -> Attribute
onhashchange = TextAttr " onhashchange=\""


oninput :: Builder -> Attribute
oninput = TextAttr " oninput=\""


oninvalid :: Builder -> Attribute
oninvalid = TextAttr " oninvalid=\""


onkeydown :: Builder -> Attribute
onkeydown = TextAttr " onkeydown=\""


onkeypress :: Builder -> Attribute
onkeypress = TextAttr " onkeypress=\""


onkeyup :: Builder -> Attribute
onkeyup = TextAttr " onkeyup=\""


onlanguagechange :: Builder -> Attribute
onlanguagechange = TextAttr " onlanguagechange=\""


onload :: Builder -> Attribute
onload = TextAttr " onload=\""


onloadeddata :: Builder -> Attribute
onloadeddata = TextAttr " onloadeddata=\""


onloadedmetadata :: Builder -> Attribute
onloadedmetadata = TextAttr " onloadedmetadata=\""


onloadstart :: Builder -> Attribute
onloadstart = TextAttr " onloadstart=\""


onmessage :: Builder -> Attribute
onmessage = TextAttr " onmessage=\""


onmessageerror :: Builder -> Attribute
onmessageerror = TextAttr " onmessageerror=\""


onmousedown :: Builder -> Attribute
onmousedown = TextAttr " onmousedown=\""


onmouseenter :: Builder -> Attribute
onmouseenter = TextAttr " onmouseenter=\""


onmouseleave :: Builder -> Attribute
onmouseleave = TextAttr " onmouseleave=\""


onmousemove :: Builder -> Attribute
onmousemove = TextAttr " onmousemove=\""


onmouseout :: Builder -> Attribute
onmouseout = TextAttr " onmouseout=\""


onmouseover :: Builder -> Attribute
onmouseover = TextAttr " onmouseover=\""


onmouseup :: Builder -> Attribute
onmouseup = TextAttr " onmouseup=\""


onoffline :: Builder -> Attribute
onoffline = TextAttr " onoffline=\""


ononline :: Builder -> Attribute
ononline = TextAttr " ononline=\""


onpagehide :: Builder -> Attribute
onpagehide = TextAttr " onpagehide=\""


onpageshow :: Builder -> Attribute
onpageshow = TextAttr " onpageshow=\""


onpaste :: Builder -> Attribute
onpaste = TextAttr " onpaste=\""


onpause :: Builder -> Attribute
onpause = TextAttr " onpause=\""


onplay :: Builder -> Attribute
onplay = TextAttr " onplay=\""


onplaying :: Builder -> Attribute
onplaying = TextAttr " onplaying=\""


onpopstate :: Builder -> Attribute
onpopstate = TextAttr " onpopstate=\""


onprogress :: Builder -> Attribute
onprogress = TextAttr " onprogress=\""


onratechange :: Builder -> Attribute
onratechange = TextAttr " onratechange=\""


onreset :: Builder -> Attribute
onreset = TextAttr " onreset=\""


onresize :: Builder -> Attribute
onresize = TextAttr " onresize=\""


onrejectionhandled :: Builder -> Attribute
onrejectionhandled = TextAttr " onrejectionhandled=\""


onscroll :: Builder -> Attribute
onscroll = TextAttr " onscroll=\""


onsecuritypolicyviolation :: Builder -> Attribute
onsecuritypolicyviolation = TextAttr " onsecuritypolicyviolation=\""


onseeked :: Builder -> Attribute
onseeked = TextAttr " onseeked=\""


onseeking :: Builder -> Attribute
onseeking = TextAttr " onseeking=\""


onselect :: Builder -> Attribute
onselect = TextAttr " onselect=\""


onslotchange :: Builder -> Attribute
onslotchange = TextAttr " onslotchange=\""


onstalled :: Builder -> Attribute
onstalled = TextAttr " onstalled=\""


onstorage :: Builder -> Attribute
onstorage = TextAttr " onstorage=\""


onsubmit :: Builder -> Attribute
onsubmit = TextAttr " onsubmit=\""


onsuspend :: Builder -> Attribute
onsuspend = TextAttr " onsuspend=\""


ontimeupdate :: Builder -> Attribute
ontimeupdate = TextAttr " ontimeupdate=\""


ontoggle :: Builder -> Attribute
ontoggle = TextAttr " ontoggle=\""


onunhandledrejection :: Builder -> Attribute
onunhandledrejection = TextAttr " onunhandledrejection=\""


onunload :: Builder -> Attribute
onunload = TextAttr " onunload=\""


onvolumechange :: Builder -> Attribute
onvolumechange = TextAttr " onvolumechange=\""


onwaiting :: Builder -> Attribute
onwaiting = TextAttr " onwaiting=\""


onwheel :: Builder -> Attribute
onwheel = TextAttr " onwheel=\""
