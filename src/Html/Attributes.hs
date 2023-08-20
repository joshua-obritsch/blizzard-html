{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Html.Attributes
    ( Attribute
    , build

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

import Prelude ((.), Bool(..), Show(..))

import Data.Foldable (fold, foldr)
import Data.Monoid ((<>), mempty)
import Data.Text.Lazy (unpack)
import Data.Text.Lazy.Builder (Builder, singleton, toLazyText)
import Internal (Buildable(..))


data Attribute
    = BoolAttr Builder Bool
    | TextAttr Builder Builder


instance Show Attribute where
    show = unpack . toLazyText . build


instance {-# OVERLAPPING #-} Show [Attribute] where
    show = unpack . toLazyText . build


instance Buildable Attribute where
    build (BoolAttr _   False ) = mempty
    build (BoolAttr key True  ) = key
    build (TextAttr _   ""    ) = mempty
    build (TextAttr key value ) = key <> value <> singleton '"'


instance Buildable [Attribute] where
    build = foldr ((<>) . build) mempty


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
allowfullscreen = BoolAttr " allowfullscreen"
{-# INLINE allowfullscreen #-}


alt :: Builder -> Attribute
alt = TextAttr " alt=\""
{-# INLINE alt #-}


as :: Builder -> Attribute
as = TextAttr " as=\""
{-# INLINE as #-}


async :: Bool -> Attribute
async = BoolAttr " async"
{-# INLINE async #-}


autocapitalize :: Builder -> Attribute
autocapitalize = TextAttr " autocapitalize=\""
{-# INLINE autocapitalize #-}


autocomplete :: Builder -> Attribute
autocomplete = TextAttr " autocomplete=\""
{-# INLINE autocomplete #-}


autofocus :: Bool -> Attribute
autofocus = BoolAttr " autofocus"
{-# INLINE autofocus #-}


autoplay :: Bool -> Attribute
autoplay = BoolAttr " autoplay"
{-# INLINE autoplay #-}


blocking :: Builder -> Attribute
blocking = TextAttr " blocking=\""
{-# INLINE blocking #-}


charset :: Builder -> Attribute
charset = TextAttr " charset=\""
{-# INLINE charset #-}


checked :: Bool -> Attribute
checked = BoolAttr " checked"
{-# INLINE checked #-}


cite :: Builder -> Attribute
cite = TextAttr " cite=\""
{-# INLINE cite #-}


class_ :: Builder -> Attribute
class_ = TextAttr " class=\""
{-# INLINE class_ #-}


color :: Builder -> Attribute
color = TextAttr " color=\""
{-# INLINE color #-}


cols :: Builder -> Attribute
cols = TextAttr " cols=\""
{-# INLINE cols #-}


colspan :: Builder -> Attribute
colspan = TextAttr " colspan=\""
{-# INLINE colspan #-}


content :: Builder -> Attribute
content = TextAttr " content=\""
{-# INLINE content #-}


contenteditable :: Builder -> Attribute
contenteditable = TextAttr " contenteditable=\""
{-# INLINE contenteditable #-}


controls :: Bool -> Attribute
controls = BoolAttr " controls"
{-# INLINE controls #-}


coords :: Builder -> Attribute
coords = TextAttr " coords=\""
{-# INLINE coords #-}


crossorigin :: Builder -> Attribute
crossorigin = TextAttr " crossorigin=\""
{-# INLINE crossorigin #-}


data_ :: Builder -> Attribute
data_ = TextAttr " data=\""
{-# INLINE data_ #-}


datetime :: Builder -> Attribute
datetime = TextAttr " datetime=\""
{-# INLINE datetime #-}


decoding :: Builder -> Attribute
decoding = TextAttr " decoding=\""
{-# INLINE decoding #-}


default_ :: Bool -> Attribute
default_ = BoolAttr " default"
{-# INLINE default_ #-}


defer :: Bool -> Attribute
defer = BoolAttr " defer"
{-# INLINE defer #-}


dir :: Builder -> Attribute
dir = TextAttr " dir=\""
{-# INLINE dir #-}


disabled :: Bool -> Attribute
disabled = BoolAttr " disabled"
{-# INLINE disabled #-}


download :: Builder -> Attribute
download = TextAttr " download=\""
{-# INLINE download #-}


draggable :: Builder -> Attribute
draggable = TextAttr " draggable=\""
{-# INLINE draggable #-}


enctype :: Builder -> Attribute
enctype = TextAttr " enctype=\""
{-# INLINE enctype #-}


enterkeyhint :: Builder -> Attribute
enterkeyhint = TextAttr " enterkeyhint=\""
{-# INLINE enterkeyhint #-}


for :: Builder -> Attribute
for = TextAttr " for=\""
{-# INLINE for #-}


form :: Builder -> Attribute
form = TextAttr " form=\""
{-# INLINE form #-}


formaction :: Builder -> Attribute
formaction = TextAttr " formaction=\""
{-# INLINE formaction #-}


formenctype :: Builder -> Attribute
formenctype = TextAttr " formenctype=\""
{-# INLINE formenctype #-}


formmethod :: Builder -> Attribute
formmethod = TextAttr " formmethod=\""
{-# INLINE formmethod #-}


formnovalidate :: Bool -> Attribute
formnovalidate = BoolAttr " formnovalidate"
{-# INLINE formnovalidate #-}


formtarget :: Builder -> Attribute
formtarget = TextAttr " formtarget=\""
{-# INLINE formtarget #-}


headers :: Builder -> Attribute
headers = TextAttr " headers=\""
{-# INLINE headers #-}


height :: Builder -> Attribute
height = TextAttr " height=\""
{-# INLINE height #-}


hidden :: Bool -> Attribute
hidden = BoolAttr " hidden"
{-# INLINE hidden #-}


high :: Builder -> Attribute
high = TextAttr " high=\""
{-# INLINE high #-}


href :: Builder -> Attribute
href = TextAttr " href=\""
{-# INLINE href #-}


hreflang :: Builder -> Attribute
hreflang = TextAttr " hreflang=\""
{-# INLINE hreflang #-}


httpEquiv :: Builder -> Attribute
httpEquiv = TextAttr " http-equiv=\""
{-# INLINE httpEquiv #-}


id :: Builder -> Attribute
id = TextAttr " id=\""
{-# INLINE id #-}


imagesizes :: Builder -> Attribute
imagesizes = TextAttr " imagesizes=\""
{-# INLINE imagesizes #-}


imagesrcset :: Builder -> Attribute
imagesrcset = TextAttr " imagesrcset=\""
{-# INLINE imagesrcset #-}


inert :: Bool -> Attribute
inert = BoolAttr " inert"
{-# INLINE inert #-}


inputmode :: Builder -> Attribute
inputmode = TextAttr " inputmode=\""
{-# INLINE inputmode #-}


integrity :: Builder -> Attribute
integrity = TextAttr " integrity=\""
{-# INLINE integrity #-}


is :: Builder -> Attribute
is = TextAttr " is=\""
{-# INLINE is #-}


ismap :: Bool -> Attribute
ismap = BoolAttr " ismap"
{-# INLINE ismap #-}


itemid :: Builder -> Attribute
itemid = TextAttr " itemid=\""
{-# INLINE itemid #-}


itemprop :: Builder -> Attribute
itemprop = TextAttr " itemprop=\""
{-# INLINE itemprop #-}


itemref :: Builder -> Attribute
itemref = TextAttr " itemref=\""
{-# INLINE itemref #-}


itemscope :: Bool -> Attribute
itemscope = BoolAttr " itemscope"
{-# INLINE itemscope #-}


itemtype :: Builder -> Attribute
itemtype = TextAttr " itemtype=\""
{-# INLINE itemtype #-}


kind :: Builder -> Attribute
kind = TextAttr " kind=\""
{-# INLINE kind #-}


label :: Builder -> Attribute
label = TextAttr " label=\""
{-# INLINE label #-}


lang :: Builder -> Attribute
lang = TextAttr " lang=\""
{-# INLINE lang #-}


list :: Builder -> Attribute
list = TextAttr " list=\""
{-# INLINE list #-}


loading :: Builder -> Attribute
loading = TextAttr " loading=\""
{-# INLINE loading #-}


loop :: Bool -> Attribute
loop = BoolAttr " loop"
{-# INLINE loop #-}


low :: Builder -> Attribute
low = TextAttr " low=\""
{-# INLINE low #-}


max :: Builder -> Attribute
max = TextAttr " max=\""
{-# INLINE max #-}


maxlength :: Builder -> Attribute
maxlength = TextAttr " maxlength=\""
{-# INLINE maxlength #-}


media :: Builder -> Attribute
media = TextAttr " media=\""
{-# INLINE media #-}


method :: Builder -> Attribute
method = TextAttr " method=\""
{-# INLINE method #-}


min :: Builder -> Attribute
min = TextAttr " min=\""
{-# INLINE min #-}


minlength :: Builder -> Attribute
minlength = TextAttr " minlength=\""
{-# INLINE minlength #-}


multiple :: Bool -> Attribute
multiple = BoolAttr " multiple"
{-# INLINE multiple #-}


muted :: Bool -> Attribute
muted = BoolAttr " muted"
{-# INLINE muted #-}


name :: Builder -> Attribute
name = TextAttr " name=\""
{-# INLINE name #-}


nomodule :: Bool -> Attribute
nomodule = BoolAttr " nomodule"
{-# INLINE nomodule #-}


nonce :: Builder -> Attribute
nonce = TextAttr " nonce=\""
{-# INLINE nonce #-}


novalidate :: Bool -> Attribute
novalidate = BoolAttr " novalidate"
{-# INLINE novalidate #-}


open :: Bool -> Attribute
open = BoolAttr " open"
{-# INLINE open #-}


optimum :: Builder -> Attribute
optimum = TextAttr " optimum=\""
{-# INLINE optimum #-}


pattern :: Builder -> Attribute
pattern = TextAttr " pattern=\""
{-# INLINE pattern #-}


ping :: Builder -> Attribute
ping = TextAttr " ping=\""
{-# INLINE ping #-}


placeholder :: Builder -> Attribute
placeholder = TextAttr " placeholder=\""
{-# INLINE placeholder #-}


playsinline :: Bool -> Attribute
playsinline = BoolAttr " playsinline"
{-# INLINE playsinline #-}


poster :: Builder -> Attribute
poster = TextAttr " poster=\""
{-# INLINE poster #-}


preload :: Builder -> Attribute
preload = TextAttr " preload=\""
{-# INLINE preload #-}


readonly :: Bool -> Attribute
readonly = BoolAttr " readonly"
{-# INLINE readonly #-}


referrerpolicy :: Builder -> Attribute
referrerpolicy = TextAttr " referrerpolicy=\""
{-# INLINE referrerpolicy #-}


rel :: Builder -> Attribute
rel = TextAttr " rel=\""
{-# INLINE rel #-}


required :: Bool -> Attribute
required = BoolAttr " required"
{-# INLINE required #-}


reversed :: Bool -> Attribute
reversed = BoolAttr " reversed"
{-# INLINE reversed #-}


rows :: Builder -> Attribute
rows = TextAttr " rows=\""
{-# INLINE rows #-}


rowspan :: Builder -> Attribute
rowspan = TextAttr " rowspan=\""
{-# INLINE rowspan #-}


sandbox :: Builder -> Attribute
sandbox = TextAttr " sandbox=\""
{-# INLINE sandbox #-}


scope :: Builder -> Attribute
scope = TextAttr " scope=\""
{-# INLINE scope #-}


selected :: Bool -> Attribute
selected = BoolAttr " selected"
{-# INLINE selected #-}


shape :: Builder -> Attribute
shape = TextAttr " shape=\""
{-# INLINE shape #-}


size :: Builder -> Attribute
size = TextAttr " size=\""
{-# INLINE size #-}


sizes :: Builder -> Attribute
sizes = TextAttr " sizes=\""
{-# INLINE sizes #-}


slot :: Builder -> Attribute
slot = TextAttr " slot=\""
{-# INLINE slot #-}


span :: Builder -> Attribute
span = TextAttr " span=\""
{-# INLINE span #-}


spellcheck :: Builder -> Attribute
spellcheck = TextAttr " spellcheck=\""
{-# INLINE spellcheck #-}


src :: Builder -> Attribute
src = TextAttr " src=\""
{-# INLINE src #-}


srcdoc :: Builder -> Attribute
srcdoc = TextAttr " srcdoc=\""
{-# INLINE srcdoc #-}


srclang :: Builder -> Attribute
srclang = TextAttr " srclang=\""
{-# INLINE srclang #-}


srcset :: Builder -> Attribute
srcset = TextAttr " srcset=\""
{-# INLINE srcset #-}


start :: Builder -> Attribute
start = TextAttr " start=\""
{-# INLINE start #-}


step :: Builder -> Attribute
step = TextAttr " step=\""
{-# INLINE step #-}


style :: [Builder] -> Attribute
style = TextAttr " style=\"" . fold
{-# INLINE style #-}


tabindex :: Builder -> Attribute
tabindex = TextAttr " tabindex=\""
{-# INLINE tabindex #-}


target :: Builder -> Attribute
target = TextAttr " target=\""
{-# INLINE target #-}


title :: Builder -> Attribute
title = TextAttr " title=\""
{-# INLINE title #-}


translate :: Builder -> Attribute
translate = TextAttr " translate=\""
{-# INLINE translate #-}


type_ :: Builder -> Attribute
type_ = TextAttr " type=\""
{-# INLINE type_ #-}


usemap :: Builder -> Attribute
usemap = TextAttr " usemap=\""
{-# INLINE usemap #-}


value :: Builder -> Attribute
value = TextAttr " value=\""
{-# INLINE value #-}


width :: Builder -> Attribute
width = TextAttr " width=\""
{-# INLINE width #-}


wrap :: Builder -> Attribute
wrap = TextAttr " wrap=\""
{-# INLINE wrap #-}


onauxclick :: Builder -> Attribute
onauxclick = TextAttr " onauxclick=\""
{-# INLINE onauxclick #-}


onafterprint :: Builder -> Attribute
onafterprint = TextAttr " onafterprint=\""
{-# INLINE onafterprint #-}


onbeforematch :: Builder -> Attribute
onbeforematch = TextAttr " onbeforematch=\""
{-# INLINE onbeforematch #-}


onbeforeprint :: Builder -> Attribute
onbeforeprint = TextAttr " onbeforeprint=\""
{-# INLINE onbeforeprint #-}


onbeforeunload :: Builder -> Attribute
onbeforeunload = TextAttr " onbeforeunload=\""
{-# INLINE onbeforeunload #-}


onblur :: Builder -> Attribute
onblur = TextAttr " onblur=\""
{-# INLINE onblur #-}


oncancel :: Builder -> Attribute
oncancel = TextAttr " oncancel=\""
{-# INLINE oncancel #-}


oncanplay :: Builder -> Attribute
oncanplay = TextAttr " oncanplay=\""
{-# INLINE oncanplay #-}


oncanplaythrough :: Builder -> Attribute
oncanplaythrough = TextAttr " oncanplaythrough=\""
{-# INLINE oncanplaythrough #-}


onchange :: Builder -> Attribute
onchange = TextAttr " onchange=\""
{-# INLINE onchange #-}


onclick :: Builder -> Attribute
onclick = TextAttr " onclick=\""
{-# INLINE onclick #-}


onclose :: Builder -> Attribute
onclose = TextAttr " onclose=\""
{-# INLINE onclose #-}


oncontextlost :: Builder -> Attribute
oncontextlost = TextAttr " oncontextlost=\""
{-# INLINE oncontextlost #-}


oncontextmenu :: Builder -> Attribute
oncontextmenu = TextAttr " oncontextmenu=\""
{-# INLINE oncontextmenu #-}


oncontextrestored :: Builder -> Attribute
oncontextrestored = TextAttr " oncontextrestored=\""
{-# INLINE oncontextrestored #-}


oncopy :: Builder -> Attribute
oncopy = TextAttr " oncopy=\""
{-# INLINE oncopy #-}


oncuechange :: Builder -> Attribute
oncuechange = TextAttr " oncuechange=\""
{-# INLINE oncuechange #-}


oncut :: Builder -> Attribute
oncut = TextAttr " oncut=\""
{-# INLINE oncut #-}


ondblclick :: Builder -> Attribute
ondblclick = TextAttr " ondblclick=\""
{-# INLINE ondblclick #-}


ondrag :: Builder -> Attribute
ondrag = TextAttr " ondrag=\""
{-# INLINE ondrag #-}


ondragend :: Builder -> Attribute
ondragend = TextAttr " ondragend=\""
{-# INLINE ondragend #-}


ondragenter :: Builder -> Attribute
ondragenter = TextAttr " ondragenter=\""
{-# INLINE ondragenter #-}


ondragleave :: Builder -> Attribute
ondragleave = TextAttr " ondragleave=\""
{-# INLINE ondragleave #-}


ondragover :: Builder -> Attribute
ondragover = TextAttr " ondragover=\""
{-# INLINE ondragover #-}


ondragstart :: Builder -> Attribute
ondragstart = TextAttr " ondragstart=\""
{-# INLINE ondragstart #-}


ondrop :: Builder -> Attribute
ondrop = TextAttr " ondrop=\""
{-# INLINE ondrop #-}


ondurationchange :: Builder -> Attribute
ondurationchange = TextAttr " ondurationchange=\""
{-# INLINE ondurationchange #-}


onemptied :: Builder -> Attribute
onemptied = TextAttr " onemptied=\""
{-# INLINE onemptied #-}


onended :: Builder -> Attribute
onended = TextAttr " onended=\""
{-# INLINE onended #-}


onerror :: Builder -> Attribute
onerror = TextAttr " onerror=\""
{-# INLINE onerror #-}


onfocus :: Builder -> Attribute
onfocus = TextAttr " onfocus=\""
{-# INLINE onfocus #-}


onformdata :: Builder -> Attribute
onformdata = TextAttr " onformdata=\""
{-# INLINE onformdata #-}


onhashchange :: Builder -> Attribute
onhashchange = TextAttr " onhashchange=\""
{-# INLINE onhashchange #-}


oninput :: Builder -> Attribute
oninput = TextAttr " oninput=\""
{-# INLINE oninput #-}


oninvalid :: Builder -> Attribute
oninvalid = TextAttr " oninvalid=\""
{-# INLINE oninvalid #-}


onkeydown :: Builder -> Attribute
onkeydown = TextAttr " onkeydown=\""
{-# INLINE onkeydown #-}


onkeypress :: Builder -> Attribute
onkeypress = TextAttr " onkeypress=\""
{-# INLINE onkeypress #-}


onkeyup :: Builder -> Attribute
onkeyup = TextAttr " onkeyup=\""
{-# INLINE onkeyup #-}


onlanguagechange :: Builder -> Attribute
onlanguagechange = TextAttr " onlanguagechange=\""
{-# INLINE onlanguagechange #-}


onload :: Builder -> Attribute
onload = TextAttr " onload=\""
{-# INLINE onload #-}


onloadeddata :: Builder -> Attribute
onloadeddata = TextAttr " onloadeddata=\""
{-# INLINE onloadeddata #-}


onloadedmetadata :: Builder -> Attribute
onloadedmetadata = TextAttr " onloadedmetadata=\""
{-# INLINE onloadedmetadata #-}


onloadstart :: Builder -> Attribute
onloadstart = TextAttr " onloadstart=\""
{-# INLINE onloadstart #-}


onmessage :: Builder -> Attribute
onmessage = TextAttr " onmessage=\""
{-# INLINE onmessage #-}


onmessageerror :: Builder -> Attribute
onmessageerror = TextAttr " onmessageerror=\""
{-# INLINE onmessageerror #-}


onmousedown :: Builder -> Attribute
onmousedown = TextAttr " onmousedown=\""
{-# INLINE onmousedown #-}


onmouseenter :: Builder -> Attribute
onmouseenter = TextAttr " onmouseenter=\""
{-# INLINE onmouseenter #-}


onmouseleave :: Builder -> Attribute
onmouseleave = TextAttr " onmouseleave=\""
{-# INLINE onmouseleave #-}


onmousemove :: Builder -> Attribute
onmousemove = TextAttr " onmousemove=\""
{-# INLINE onmousemove #-}


onmouseout :: Builder -> Attribute
onmouseout = TextAttr " onmouseout=\""
{-# INLINE onmouseout #-}


onmouseover :: Builder -> Attribute
onmouseover = TextAttr " onmouseover=\""
{-# INLINE onmouseover #-}


onmouseup :: Builder -> Attribute
onmouseup = TextAttr " onmouseup=\""
{-# INLINE onmouseup #-}


onoffline :: Builder -> Attribute
onoffline = TextAttr " onoffline=\""
{-# INLINE onoffline #-}


ononline :: Builder -> Attribute
ononline = TextAttr " ononline=\""
{-# INLINE ononline #-}


onpagehide :: Builder -> Attribute
onpagehide = TextAttr " onpagehide=\""
{-# INLINE onpagehide #-}


onpageshow :: Builder -> Attribute
onpageshow = TextAttr " onpageshow=\""
{-# INLINE onpageshow #-}


onpaste :: Builder -> Attribute
onpaste = TextAttr " onpaste=\""
{-# INLINE onpaste #-}


onpause :: Builder -> Attribute
onpause = TextAttr " onpause=\""
{-# INLINE onpause #-}


onplay :: Builder -> Attribute
onplay = TextAttr " onplay=\""
{-# INLINE onplay #-}


onplaying :: Builder -> Attribute
onplaying = TextAttr " onplaying=\""
{-# INLINE onplaying #-}


onpopstate :: Builder -> Attribute
onpopstate = TextAttr " onpopstate=\""
{-# INLINE onpopstate #-}


onprogress :: Builder -> Attribute
onprogress = TextAttr " onprogress=\""
{-# INLINE onprogress #-}


onratechange :: Builder -> Attribute
onratechange = TextAttr " onratechange=\""
{-# INLINE onratechange #-}


onreset :: Builder -> Attribute
onreset = TextAttr " onreset=\""
{-# INLINE onreset #-}


onresize :: Builder -> Attribute
onresize = TextAttr " onresize=\""
{-# INLINE onresize #-}


onrejectionhandled :: Builder -> Attribute
onrejectionhandled = TextAttr " onrejectionhandled=\""
{-# INLINE onrejectionhandled #-}


onscroll :: Builder -> Attribute
onscroll = TextAttr " onscroll=\""
{-# INLINE onscroll #-}


onsecuritypolicyviolation :: Builder -> Attribute
onsecuritypolicyviolation = TextAttr " onsecuritypolicyviolation=\""
{-# INLINE onsecuritypolicyviolation #-}


onseeked :: Builder -> Attribute
onseeked = TextAttr " onseeked=\""
{-# INLINE onseeked #-}


onseeking :: Builder -> Attribute
onseeking = TextAttr " onseeking=\""
{-# INLINE onseeking #-}


onselect :: Builder -> Attribute
onselect = TextAttr " onselect=\""
{-# INLINE onselect #-}


onslotchange :: Builder -> Attribute
onslotchange = TextAttr " onslotchange=\""
{-# INLINE onslotchange #-}


onstalled :: Builder -> Attribute
onstalled = TextAttr " onstalled=\""
{-# INLINE onstalled #-}


onstorage :: Builder -> Attribute
onstorage = TextAttr " onstorage=\""
{-# INLINE onstorage #-}


onsubmit :: Builder -> Attribute
onsubmit = TextAttr " onsubmit=\""
{-# INLINE onsubmit #-}


onsuspend :: Builder -> Attribute
onsuspend = TextAttr " onsuspend=\""
{-# INLINE onsuspend #-}


ontimeupdate :: Builder -> Attribute
ontimeupdate = TextAttr " ontimeupdate=\""
{-# INLINE ontimeupdate #-}


ontoggle :: Builder -> Attribute
ontoggle = TextAttr " ontoggle=\""
{-# INLINE ontoggle #-}


onunhandledrejection :: Builder -> Attribute
onunhandledrejection = TextAttr " onunhandledrejection=\""
{-# INLINE onunhandledrejection #-}


onunload :: Builder -> Attribute
onunload = TextAttr " onunload=\""
{-# INLINE onunload #-}


onvolumechange :: Builder -> Attribute
onvolumechange = TextAttr " onvolumechange=\""
{-# INLINE onvolumechange #-}


onwaiting :: Builder -> Attribute
onwaiting = TextAttr " onwaiting=\""
{-# INLINE onwaiting #-}


onwheel :: Builder -> Attribute
onwheel = TextAttr " onwheel=\""
{-# INLINE onwheel #-}
