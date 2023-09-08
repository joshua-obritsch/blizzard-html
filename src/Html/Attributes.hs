{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The "Html.Attributes" module provides a set of functions for generating HTML attributes.
--
-- __Example:__
--
-- @
-- Html.doctype []
--     [ Html.html []
--         [ Html.head []
--             [ Html.meta
--                 [ Attr.charset \"UTF-8\" ]
--             , Html.meta
--                 [ Attr.name \"viewport\"
--                 , Attr.content \"width=device-width, initial-scale=1.0\"
--                 ]
--             , Html.title []
--                 [ \"Commodore 64\" ]
--             ]
--         , Html.body []
--             [ Html.h1 []
--                 [ \"Commodore 64\" ]
--             , Html.img
--                 [ Attr.src \"commodore64.jpg\"
--                 , Attr.alt \"Commodore 64\"
--                 , Attr.width \"300\"
--                 , Attr.height \"200\"
--                 ]
--             , Html.h2 []
--                 [ \"Introduction\" ]
--             , Html.p []
--                 [ \"The \"
--                 , Html.strong []
--                     [ \"Commodore 64\" ]
--                 , \" is a legendary 8-bit home computer introduced in 1982.\"
--                 ]
--             , Html.h2 []
--                 [ \"Key Features\" ]
--             , Html.ul []
--                 [ Html.li []
--                     [ \"Released: \"
--                     , Html.span
--                         [ Attr.class_ \"highlight\" ]
--                         [ \"August 1982\" ]
--                     ]
--                 , Html.li []
--                     [ \"CPU: \"
--                     , Html.span
--                         [ Attr.class_ \"highlight\"
--                         , Attr.title \"MOS Technology 6510\"
--                         ]
--                         [ \"6510\" ]
--                     ]
--                 , Html.li []
--                     [ \"RAM: \"
--                     , Html.span
--                         [ Attr.class_ \"highlight\" ]
--                         [ \"64 KB\" ]
--                     ]
--                 , Html.li []
--                     [ \"Graphics: \"
--                     , Html.span
--                         [ Attr.class_ \"highlight\" ]
--                         [ \"VIC-II\" ]
--                     ]
--                 , Html.li []
--                     [ \"Sound: \"
--                     , Html.span
--                         [ Attr.class_ \"highlight\" ]
--                         [ \"SID 6581\" ]
--                     ]
--                 ]
--             ]
--         ]
-- @
--
-- __Result:__
--
-- @
-- \<!DOCTYPE html\>
-- \<html\>
--     \<head\>
--         \<meta charset=\"UTF-8\"\>
--         \<meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\"\>
--         \<title\>Commodore 64\<\/title\>
--     \<\/head\>
--     \<body\>
--         \<h1\>Commodore 64\<\/h1\>
--         \<img src=\"commodore64.jpg\" alt=\"Commodore 64\" width=\"300\" height=\"200\"\>
--         \<h2\>Introduction\<\/h2\>
--         \<p\>The \<strong\>Commodore 64\<\/strong\> is a legendary 8-bit computer introduced in 1982.\<\/p\>
--         \<h2\>Key Features\<\/h2\>
--         \<ul\>
--             \<li\>Released: \<span class=\"highlight\"\>August 1982\<\/span\>\<\/li\>
--             \<li\>CPU: \<span class=\"highlight\" title=\"MOS Technology 6510\"\>6510\<\/span\>\<\/li\>
--             \<li\>RAM: \<span class=\"highlight\"\>64 KB\<\/span\>\<\/li\>
--             \<li\>Graphics: \<span class=\"highlight\"\>VIC-II\<\/span\>\<\/li\>
--             \<li\>Sound: \<span class=\"highlight\"\>SID 6581\<\/span\>\<\/li\>
--         \<\/ul\>
--     \<\/body\>
-- \<\/html\>
-- @
--
-- /Note: All examples in this module assume the following imports:/
--
-- @
-- import qualified Html
-- import qualified Html.Attributes as Attr
-- @
--
-- /Note: All example results in this module are formatted neatly for readability but are condensed in practice./
module Html.Attributes
    ( -- * Attributes
      -- ** abbr
      abbr
      -- ** accept
    , accept
      -- ** acceptCharset
    , acceptCharset
      -- ** accesskey
    , accesskey
      -- ** action
    , action
      -- ** allow
    , allow
      -- ** allowfullscreen
    , allowfullscreen
      -- ** alt
    , alt
      -- ** as
    , as
      -- ** async
    , async
      -- ** autocapitalize
    , autocapitalize
      -- ** autocomplete
    , autocomplete
      -- ** autofocus
    , autofocus
      -- ** autoplay
    , autoplay
      -- ** blocking
    , blocking
      -- ** charset
    , charset
      -- ** checked
    , checked
      -- ** cite
    , cite
      -- ** class
    , class_
      -- ** color
    , color
      -- ** cols
    , cols
      -- ** colspan
    , colspan
      -- ** content
    , content
      -- ** contenteditable
    , contenteditable
      -- ** controls
    , controls
      -- ** coords
    , coords
      -- ** crossorigin
    , crossorigin
      -- ** data
    , data_
      -- ** datetime
    , datetime
      -- ** decoding
    , decoding
      -- ** default
    , default_
      -- ** defer
    , defer
      -- ** dir
    , dir
      -- ** disabled
    , disabled
      -- ** download
    , download
      -- ** draggable
    , draggable
      -- ** enctype
    , enctype
      -- ** enterkeyhint
    , enterkeyhint
      -- ** for
    , for
      -- ** form
    , form
      -- ** formaction
    , formaction
      -- ** formenctype
    , formenctype
      -- ** formmethod
    , formmethod
      -- ** formnovalidate
    , formnovalidate
      -- ** formtarget
    , formtarget
      -- ** headers
    , headers
      -- ** height
    , height
      -- ** hidden
    , hidden
      -- ** high
    , high
      -- ** href
    , href
      -- ** hreflang
    , hreflang
      -- ** httpEquiv
    , httpEquiv
      -- ** id
    , id
      -- ** imagesizes
    , imagesizes
      -- ** imagesrcset
    , imagesrcset
      -- ** inert
    , inert
      -- ** inputmode
    , inputmode
      -- ** integrity
    , integrity
      -- ** is
    , is
      -- ** ismap
    , ismap
      -- ** itemid
    , itemid
      -- ** itemprop
    , itemprop
      -- ** itemref
    , itemref
      -- ** itemscope
    , itemscope
      -- ** itemtype
    , itemtype
      -- ** kind
    , kind
      -- ** label
    , label
      -- ** lang
    , lang
      -- ** list
    , list
      -- ** loading
    , loading
      -- ** loop
    , loop
      -- ** low
    , low
      -- ** max
    , max
      -- ** maxlength
    , maxlength
      -- ** media
    , media
      -- ** method
    , method
      -- ** min
    , min
      -- ** minlength
    , minlength
      -- ** multiple
    , multiple
      -- ** muted
    , muted
      -- ** name
    , name
      -- ** nomodule
    , nomodule
      -- ** nonce
    , nonce
      -- ** novalidate
    , novalidate
      -- ** open
    , open
      -- ** optimum
    , optimum
      -- ** pattern
    , pattern
      -- ** ping
    , ping
      -- ** placeholder
    , placeholder
      -- ** playsinline
    , playsinline
      -- ** poster
    , poster
      -- ** preload
    , preload
      -- ** readonly
    , readonly
      -- ** referrerpolicy
    , referrerpolicy
      -- ** rel
    , rel
      -- ** required
    , required
      -- ** reversed
    , reversed
      -- ** rows
    , rows
      -- ** rowspan
    , rowspan
      -- ** sandbox
    , sandbox
      -- ** scope
    , scope
      -- ** selected
    , selected
      -- ** shape
    , shape
      -- ** size
    , size
      -- ** sizes
    , sizes
      -- ** slot
    , slot
      -- ** span
    , span
      -- ** spellcheck
    , spellcheck
      -- ** src
    , src
      -- ** srcdoc
    , srcdoc
      -- ** srclang
    , srclang
      -- ** srcset
    , srcset
      -- ** start
    , start
      -- ** step
    , step
      -- ** style
    , style
      -- ** tabindex
    , tabindex
      -- ** target
    , target
      -- ** title
    , title
      -- ** translate
    , translate
      -- ** type
    , type_
      -- ** usemap
    , usemap
      -- ** value
    , value
      -- ** width
    , width
      -- ** wrap
    , wrap
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
