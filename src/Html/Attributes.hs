{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Module    : Html.Attributes
-- Copyright   : (c) Joshua Obritsch, 2021
-- License     : MIT
-- Maintainer  : joshua@obritsch.com
-- Stability   : Experimental
--
-- The "Html.Attributes" module provides a set of functions for generating HTML attributes.
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
      -- ** alpha
    , alpha
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
      -- ** autocorrect
    , autocorrect
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
      -- ** closedby
    , closedby
      -- ** color
    , color
      -- ** colorspace
    , colorspace
      -- ** cols
    , cols
      -- ** colspan
    , colspan
      -- ** command
    , command
      -- ** commandfor
    , commandfor
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
      -- ** dirname
    , dirname
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
      -- ** fetchpriority
    , fetchpriority
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
      -- ** popover
    , popover
      -- ** popovertarget
    , popovertarget
      -- ** popovertargetaction
    , popovertargetaction
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
      -- ** shadowrootclonable
    , shadowrootclonable
      -- ** shadowrootdelegatesfocus
    , shadowrootdelegatesfocus
      -- ** shadowrootmode
    , shadowrootmode
      -- ** shadowrootserializable
    , shadowrootserializable
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
      -- ** writingsuggestions
    , writingsuggestions
    ) where


import Prelude (Bool)

import Data.Text.Lazy.Builder (Builder)
import Html                   (Attribute(..))


-- ATTRIBUTES


-- | Generates an HTML @abbr@ attribute with the given value.
abbr :: Builder -> Attribute ctx
abbr = TextAttribute " abbr=\""
{-# INLINE abbr #-}


-- | Generates an HTML @accept@ attribute with the given value.
accept :: Builder -> Attribute ctx
accept = TextAttribute " accept=\""
{-# INLINE accept #-}


-- | Generates an HTML @accept-charset@ attribute with the given value.
acceptCharset :: Builder -> Attribute ctx
acceptCharset = TextAttribute " accept-charset=\""
{-# INLINE acceptCharset #-}


-- | Generates an HTML @accesskey@ attribute with the given value.
accesskey :: Builder -> Attribute ctx
accesskey = TextAttribute " accesskey=\""
{-# INLINE accesskey #-}


-- | Generates an HTML @action@ attribute with the given value.
action :: Builder -> Attribute ctx
action = TextAttribute " action=\""
{-# INLINE action #-}


-- | Generates an HTML @allow@ attribute with the given value.
allow :: Builder -> Attribute ctx
allow = TextAttribute " allow=\""
{-# INLINE allow #-}


-- | Generates an HTML @allowfullscreen@ attribute with the given value.
allowfullscreen :: Bool -> Attribute ctx
allowfullscreen = BoolAttribute " allowfullscreen"
{-# INLINE allowfullscreen #-}


-- | Generates an HTML @alpha@ attribute with the given value.
alpha :: Bool -> Attribute ctx
alpha = BoolAttribute " alpha"
{-# INLINE alpha #-}


-- | Generates an HTML @alt@ attribute with the given value.
alt :: Builder -> Attribute ctx
alt = TextAttribute " alt=\""
{-# INLINE alt #-}


-- | Generates an HTML @as@ attribute with the given value.
as :: Builder -> Attribute ctx
as = TextAttribute " as=\""
{-# INLINE as #-}


-- | Generates an HTML @async@ attribute with the given value.
async :: Bool -> Attribute ctx
async = BoolAttribute " async"
{-# INLINE async #-}


-- | Generates an HTML @autocapitalize@ attribute with the given value.
autocapitalize :: Builder -> Attribute ctx
autocapitalize = TextAttribute " autocapitalize=\""
{-# INLINE autocapitalize #-}


-- | Generates an HTML @autocomplete@ attribute with the given value.
autocomplete :: Builder -> Attribute ctx
autocomplete = TextAttribute " autocomplete=\""
{-# INLINE autocomplete #-}


-- | Generates an HTML @autocorrect@ attribute with the given value.
autocorrect :: Builder -> Attribute ctx
autocorrect = TextAttribute " autocorrect=\""
{-# INLINE autocorrect #-}


-- | Generates an HTML @autofocus@ attribute with the given value.
autofocus :: Bool -> Attribute ctx
autofocus = BoolAttribute " autofocus"
{-# INLINE autofocus #-}


-- | Generates an HTML @autoplay@ attribute with the given value.
autoplay :: Bool -> Attribute ctx
autoplay = BoolAttribute " autoplay"
{-# INLINE autoplay #-}


-- | Generates an HTML @blocking@ attribute with the given value.
blocking :: Builder -> Attribute ctx
blocking = TextAttribute " blocking=\""
{-# INLINE blocking #-}


-- | Generates an HTML @charset@ attribute with the given value.
charset :: Builder -> Attribute ctx
charset = TextAttribute " charset=\""
{-# INLINE charset #-}


-- | Generates an HTML @checked@ attribute with the given value.
checked :: Bool -> Attribute ctx
checked = BoolAttribute " checked"
{-# INLINE checked #-}


-- | Generates an HTML @cite@ attribute with the given value.
cite :: Builder -> Attribute ctx
cite = TextAttribute " cite=\""
{-# INLINE cite #-}


-- | Generates an HTML @class@ attribute with the given value.
class_ :: Builder -> Attribute ctx
class_ = TextAttribute " class=\""
{-# INLINE class_ #-}


-- | Generates an HTML @closedby@ attribute with the given value.
closedby :: Builder -> Attribute ctx
closedby = TextAttribute " closedby=\""
{-# INLINE closedby #-}


-- | Generates an HTML @color@ attribute with the given value.
color :: Builder -> Attribute ctx
color = TextAttribute " color=\""
{-# INLINE color #-}


-- | Generates an HTML @colorspace@ attribute with the given value.
colorspace :: Builder -> Attribute ctx
colorspace = TextAttribute " colorspace=\""
{-# INLINE colorspace #-}


-- | Generates an HTML @cols@ attribute with the given value.
cols :: Builder -> Attribute ctx
cols = TextAttribute " cols=\""
{-# INLINE cols #-}


-- | Generates an HTML @colspan@ attribute with the given value.
colspan :: Builder -> Attribute ctx
colspan = TextAttribute " colspan=\""
{-# INLINE colspan #-}


-- | Generates an HTML @command@ attribute with the given value.
command :: Builder -> Attribute ctx
command = TextAttribute " command=\""
{-# INLINE command #-}


-- | Generates an HTML @commandfor@ attribute with the given value.
commandfor :: Builder -> Attribute ctx
commandfor = TextAttribute " commandfor=\""
{-# INLINE commandfor #-}


-- | Generates an HTML @content@ attribute with the given value.
content :: Builder -> Attribute ctx
content = TextAttribute " content=\""
{-# INLINE content #-}


-- | Generates an HTML @contenteditable@ attribute with the given value.
contenteditable :: Builder -> Attribute ctx
contenteditable = TextAttribute " contenteditable=\""
{-# INLINE contenteditable #-}


-- | Generates an HTML @controls@ attribute with the given value.
controls :: Bool -> Attribute ctx
controls = BoolAttribute " controls"
{-# INLINE controls #-}


-- | Generates an HTML @coords@ attribute with the given value.
coords :: Builder -> Attribute ctx
coords = TextAttribute " coords=\""
{-# INLINE coords #-}


-- | Generates an HTML @crossorigin@ attribute with the given value.
crossorigin :: Builder -> Attribute ctx
crossorigin = TextAttribute " crossorigin=\""
{-# INLINE crossorigin #-}


-- | Generates an HTML @data@ attribute with the given value.
data_ :: Builder -> Attribute ctx
data_ = TextAttribute " data=\""
{-# INLINE data_ #-}


-- | Generates an HTML @datetime@ attribute with the given value.
datetime :: Builder -> Attribute ctx
datetime = TextAttribute " datetime=\""
{-# INLINE datetime #-}


-- | Generates an HTML @decoding@ attribute with the given value.
decoding :: Builder -> Attribute ctx
decoding = TextAttribute " decoding=\""
{-# INLINE decoding #-}


-- | Generates an HTML @default@ attribute with the given value.
default_ :: Bool -> Attribute ctx
default_ = BoolAttribute " default"
{-# INLINE default_ #-}


-- | Generates an HTML @defer@ attribute with the given value.
defer :: Bool -> Attribute ctx
defer = BoolAttribute " defer"
{-# INLINE defer #-}


-- | Generates an HTML @dir@ attribute with the given value.
dir :: Builder -> Attribute ctx
dir = TextAttribute " dir=\""
{-# INLINE dir #-}


-- | Generates an HTML @dirname@ attribute with the given value.
dirname :: Builder -> Attribute ctx
dirname = TextAttribute " dirname=\""
{-# INLINE dirname #-}


-- | Generates an HTML @disabled@ attribute with the given value.
disabled :: Bool -> Attribute ctx
disabled = BoolAttribute " disabled"
{-# INLINE disabled #-}


-- | Generates an HTML @download@ attribute with the given value.
download :: Builder -> Attribute ctx
download = TextAttribute " download=\""
{-# INLINE download #-}


-- | Generates an HTML @draggable@ attribute with the given value.
draggable :: Builder -> Attribute ctx
draggable = TextAttribute " draggable=\""
{-# INLINE draggable #-}


-- | Generates an HTML @enctype@ attribute with the given value.
enctype :: Builder -> Attribute ctx
enctype = TextAttribute " enctype=\""
{-# INLINE enctype #-}


-- | Generates an HTML @enterkeyhint@ attribute with the given value.
enterkeyhint :: Builder -> Attribute ctx
enterkeyhint = TextAttribute " enterkeyhint=\""
{-# INLINE enterkeyhint #-}


-- | Generates an HTML @fetchpriority@ attribute with the given value.
fetchpriority :: Builder -> Attribute ctx
fetchpriority = TextAttribute " fetchpriority=\""
{-# INLINE fetchpriority #-}


-- | Generates an HTML @for@ attribute with the given value.
for :: Builder -> Attribute ctx
for = TextAttribute " for=\""
{-# INLINE for #-}


-- | Generates an HTML @form@ attribute with the given value.
form :: Builder -> Attribute ctx
form = TextAttribute " form=\""
{-# INLINE form #-}


-- | Generates an HTML @formaction@ attribute with the given value.
formaction :: Builder -> Attribute ctx
formaction = TextAttribute " formaction=\""
{-# INLINE formaction #-}


-- | Generates an HTML @formenctype@ attribute with the given value.
formenctype :: Builder -> Attribute ctx
formenctype = TextAttribute " formenctype=\""
{-# INLINE formenctype #-}


-- | Generates an HTML @formmethod@ attribute with the given value.
formmethod :: Builder -> Attribute ctx
formmethod = TextAttribute " formmethod=\""
{-# INLINE formmethod #-}


-- | Generates an HTML @formnovalidate@ attribute with the given value.
formnovalidate :: Bool -> Attribute ctx
formnovalidate = BoolAttribute " formnovalidate"
{-# INLINE formnovalidate #-}


-- | Generates an HTML @formtarget@ attribute with the given value.
formtarget :: Builder -> Attribute ctx
formtarget = TextAttribute " formtarget=\""
{-# INLINE formtarget #-}


-- | Generates an HTML @headers@ attribute with the given value.
headers :: Builder -> Attribute ctx
headers = TextAttribute " headers=\""
{-# INLINE headers #-}


-- | Generates an HTML @height@ attribute with the given value.
height :: Builder -> Attribute ctx
height = TextAttribute " height=\""
{-# INLINE height #-}


-- | Generates an HTML @hidden@ attribute with the given value.
hidden :: Bool -> Attribute ctx
hidden = BoolAttribute " hidden"
{-# INLINE hidden #-}


-- | Generates an HTML @high@ attribute with the given value.
high :: Builder -> Attribute ctx
high = TextAttribute " high=\""
{-# INLINE high #-}


-- | Generates an HTML @href@ attribute with the given value.
href :: Builder -> Attribute ctx
href = TextAttribute " href=\""
{-# INLINE href #-}


-- | Generates an HTML @hreflang@ attribute with the given value.
hreflang :: Builder -> Attribute ctx
hreflang = TextAttribute " hreflang=\""
{-# INLINE hreflang #-}


-- | Generates an HTML @http-equiv@ attribute with the given value.
httpEquiv :: Builder -> Attribute ctx
httpEquiv = TextAttribute " http-equiv=\""
{-# INLINE httpEquiv #-}


-- | Generates an HTML @id@ attribute with the given value.
id :: Builder -> Attribute ctx
id = TextAttribute " id=\""
{-# INLINE id #-}


-- | Generates an HTML @imagesizes@ attribute with the given value.
imagesizes :: Builder -> Attribute ctx
imagesizes = TextAttribute " imagesizes=\""
{-# INLINE imagesizes #-}


-- | Generates an HTML @imagesrcset@ attribute with the given value.
imagesrcset :: Builder -> Attribute ctx
imagesrcset = TextAttribute " imagesrcset=\""
{-# INLINE imagesrcset #-}


-- | Generates an HTML @inert@ attribute with the given value.
inert :: Bool -> Attribute ctx
inert = BoolAttribute " inert"
{-# INLINE inert #-}


-- | Generates an HTML @inputmode@ attribute with the given value.
inputmode :: Builder -> Attribute ctx
inputmode = TextAttribute " inputmode=\""
{-# INLINE inputmode #-}


-- | Generates an HTML @integrity@ attribute with the given value.
integrity :: Builder -> Attribute ctx
integrity = TextAttribute " integrity=\""
{-# INLINE integrity #-}


-- | Generates an HTML @is@ attribute with the given value.
is :: Builder -> Attribute ctx
is = TextAttribute " is=\""
{-# INLINE is #-}


-- | Generates an HTML @ismap@ attribute with the given value.
ismap :: Bool -> Attribute ctx
ismap = BoolAttribute " ismap"
{-# INLINE ismap #-}


-- | Generates an HTML @itemid@ attribute with the given value.
itemid :: Builder -> Attribute ctx
itemid = TextAttribute " itemid=\""
{-# INLINE itemid #-}


-- | Generates an HTML @itemprop@ attribute with the given value.
itemprop :: Builder -> Attribute ctx
itemprop = TextAttribute " itemprop=\""
{-# INLINE itemprop #-}


-- | Generates an HTML @itemref@ attribute with the given value.
itemref :: Builder -> Attribute ctx
itemref = TextAttribute " itemref=\""
{-# INLINE itemref #-}


-- | Generates an HTML @itemscope@ attribute with the given value.
itemscope :: Bool -> Attribute ctx
itemscope = BoolAttribute " itemscope"
{-# INLINE itemscope #-}


-- | Generates an HTML @itemtype@ attribute with the given value.
itemtype :: Builder -> Attribute ctx
itemtype = TextAttribute " itemtype=\""
{-# INLINE itemtype #-}


-- | Generates an HTML @kind@ attribute with the given value.
kind :: Builder -> Attribute ctx
kind = TextAttribute " kind=\""
{-# INLINE kind #-}


-- | Generates an HTML @label@ attribute with the given value.
label :: Builder -> Attribute ctx
label = TextAttribute " label=\""
{-# INLINE label #-}


-- | Generates an HTML @lang@ attribute with the given value.
lang :: Builder -> Attribute ctx
lang = TextAttribute " lang=\""
{-# INLINE lang #-}


-- | Generates an HTML @list@ attribute with the given value.
list :: Builder -> Attribute ctx
list = TextAttribute " list=\""
{-# INLINE list #-}


-- | Generates an HTML @loading@ attribute with the given value.
loading :: Builder -> Attribute ctx
loading = TextAttribute " loading=\""
{-# INLINE loading #-}


-- | Generates an HTML @loop@ attribute with the given value.
loop :: Bool -> Attribute ctx
loop = BoolAttribute " loop"
{-# INLINE loop #-}


-- | Generates an HTML @low@ attribute with the given value.
low :: Builder -> Attribute ctx
low = TextAttribute " low=\""
{-# INLINE low #-}


-- | Generates an HTML @max@ attribute with the given value.
max :: Builder -> Attribute ctx
max = TextAttribute " max=\""
{-# INLINE max #-}


-- | Generates an HTML @maxlength@ attribute with the given value.
maxlength :: Builder -> Attribute ctx
maxlength = TextAttribute " maxlength=\""
{-# INLINE maxlength #-}


-- | Generates an HTML @media@ attribute with the given value.
media :: Builder -> Attribute ctx
media = TextAttribute " media=\""
{-# INLINE media #-}


-- | Generates an HTML @method@ attribute with the given value.
method :: Builder -> Attribute ctx
method = TextAttribute " method=\""
{-# INLINE method #-}


-- | Generates an HTML @min@ attribute with the given value.
min :: Builder -> Attribute ctx
min = TextAttribute " min=\""
{-# INLINE min #-}


-- | Generates an HTML @minlength@ attribute with the given value.
minlength :: Builder -> Attribute ctx
minlength = TextAttribute " minlength=\""
{-# INLINE minlength #-}


-- | Generates an HTML @multiple@ attribute with the given value.
multiple :: Bool -> Attribute ctx
multiple = BoolAttribute " multiple"
{-# INLINE multiple #-}


-- | Generates an HTML @muted@ attribute with the given value.
muted :: Bool -> Attribute ctx
muted = BoolAttribute " muted"
{-# INLINE muted #-}


-- | Generates an HTML @name@ attribute with the given value.
name :: Builder -> Attribute ctx
name = TextAttribute " name=\""
{-# INLINE name #-}


-- | Generates an HTML @nomodule@ attribute with the given value.
nomodule :: Bool -> Attribute ctx
nomodule = BoolAttribute " nomodule"
{-# INLINE nomodule #-}


-- | Generates an HTML @nonce@ attribute with the given value.
nonce :: Builder -> Attribute ctx
nonce = TextAttribute " nonce=\""
{-# INLINE nonce #-}


-- | Generates an HTML @novalidate@ attribute with the given value.
novalidate :: Bool -> Attribute ctx
novalidate = BoolAttribute " novalidate"
{-# INLINE novalidate #-}


-- | Generates an HTML @open@ attribute with the given value.
open :: Bool -> Attribute ctx
open = BoolAttribute " open"
{-# INLINE open #-}


-- | Generates an HTML @optimum@ attribute with the given value.
optimum :: Builder -> Attribute ctx
optimum = TextAttribute " optimum=\""
{-# INLINE optimum #-}


-- | Generates an HTML @pattern@ attribute with the given value.
pattern :: Builder -> Attribute ctx
pattern = TextAttribute " pattern=\""
{-# INLINE pattern #-}


-- | Generates an HTML @ping@ attribute with the given value.
ping :: Builder -> Attribute ctx
ping = TextAttribute " ping=\""
{-# INLINE ping #-}


-- | Generates an HTML @placeholder@ attribute with the given value.
placeholder :: Builder -> Attribute ctx
placeholder = TextAttribute " placeholder=\""
{-# INLINE placeholder #-}


-- | Generates an HTML @playsinline@ attribute with the given value.
playsinline :: Bool -> Attribute ctx
playsinline = BoolAttribute " playsinline"
{-# INLINE playsinline #-}


-- | Generates an HTML @popover@ attribute with the given value.
popover :: Builder -> Attribute ctx
popover = TextAttribute " popover=\""
{-# INLINE popover #-}


-- | Generates an HTML @popovertarget@ attribute with the given value.
popovertarget :: Builder -> Attribute ctx
popovertarget = TextAttribute " popovertarget=\""
{-# INLINE popovertarget #-}


-- | Generates an HTML @popovertargetaction@ attribute with the given value.
popovertargetaction :: Builder -> Attribute ctx
popovertargetaction = TextAttribute " popovertargetaction=\""
{-# INLINE popovertargetaction #-}


-- | Generates an HTML @poster@ attribute with the given value.
poster :: Builder -> Attribute ctx
poster = TextAttribute " poster=\""
{-# INLINE poster #-}


-- | Generates an HTML @preload@ attribute with the given value.
preload :: Builder -> Attribute ctx
preload = TextAttribute " preload=\""
{-# INLINE preload #-}


-- | Generates an HTML @readonly@ attribute with the given value.
readonly :: Bool -> Attribute ctx
readonly = BoolAttribute " readonly"
{-# INLINE readonly #-}


-- | Generates an HTML @referrerpolicy@ attribute with the given value.
referrerpolicy :: Builder -> Attribute ctx
referrerpolicy = TextAttribute " referrerpolicy=\""
{-# INLINE referrerpolicy #-}


-- | Generates an HTML @rel@ attribute with the given value.
rel :: Builder -> Attribute ctx
rel = TextAttribute " rel=\""
{-# INLINE rel #-}


-- | Generates an HTML @required@ attribute with the given value.
required :: Bool -> Attribute ctx
required = BoolAttribute " required"
{-# INLINE required #-}


-- | Generates an HTML @reversed@ attribute with the given value.
reversed :: Bool -> Attribute ctx
reversed = BoolAttribute " reversed"
{-# INLINE reversed #-}


-- | Generates an HTML @rows@ attribute with the given value.
rows :: Builder -> Attribute ctx
rows = TextAttribute " rows=\""
{-# INLINE rows #-}


-- | Generates an HTML @rowspan@ attribute with the given value.
rowspan :: Builder -> Attribute ctx
rowspan = TextAttribute " rowspan=\""
{-# INLINE rowspan #-}


-- | Generates an HTML @sandbox@ attribute with the given value.
sandbox :: Builder -> Attribute ctx
sandbox = TextAttribute " sandbox=\""
{-# INLINE sandbox #-}


-- | Generates an HTML @scope@ attribute with the given value.
scope :: Builder -> Attribute ctx
scope = TextAttribute " scope=\""
{-# INLINE scope #-}


-- | Generates an HTML @selected@ attribute with the given value.
selected :: Bool -> Attribute ctx
selected = BoolAttribute " selected"
{-# INLINE selected #-}


-- | Generates an HTML @shadowrootclonable@ attribute with the given value.
shadowrootclonable :: Bool -> Attribute ctx
shadowrootclonable = BoolAttribute " shadowrootclonable"
{-# INLINE shadowrootclonable #-}


-- | Generates an HTML @shadowrootdelegatesfocus@ attribute with the given value.
shadowrootdelegatesfocus :: Bool -> Attribute ctx
shadowrootdelegatesfocus = BoolAttribute " shadowrootdelegatesfocus"
{-# INLINE shadowrootdelegatesfocus #-}


-- | Generates an HTML @shadowrootmode@ attribute with the given value.
shadowrootmode :: Builder -> Attribute ctx
shadowrootmode = TextAttribute " shadowrootmode=\""
{-# INLINE shadowrootmode #-}


-- | Generates an HTML @shadowrootserializable@ attribute with the given value.
shadowrootserializable :: Bool -> Attribute ctx
shadowrootserializable = BoolAttribute " shadowrootserializable"
{-# INLINE shadowrootserializable #-}


-- | Generates an HTML @shape@ attribute with the given value.
shape :: Builder -> Attribute ctx
shape = TextAttribute " shape=\""
{-# INLINE shape #-}


-- | Generates an HTML @size@ attribute with the given value.
size :: Builder -> Attribute ctx
size = TextAttribute " size=\""
{-# INLINE size #-}


-- | Generates an HTML @sizes@ attribute with the given value.
sizes :: Builder -> Attribute ctx
sizes = TextAttribute " sizes=\""
{-# INLINE sizes #-}


-- | Generates an HTML @slot@ attribute with the given value.
slot :: Builder -> Attribute ctx
slot = TextAttribute " slot=\""
{-# INLINE slot #-}


-- | Generates an HTML @span@ attribute with the given value.
span :: Builder -> Attribute ctx
span = TextAttribute " span=\""
{-# INLINE span #-}


-- | Generates an HTML @spellcheck@ attribute with the given value.
spellcheck :: Builder -> Attribute ctx
spellcheck = TextAttribute " spellcheck=\""
{-# INLINE spellcheck #-}


-- | Generates an HTML @src@ attribute with the given value.
src :: Builder -> Attribute ctx
src = TextAttribute " src=\""
{-# INLINE src #-}


-- | Generates an HTML @srcdoc@ attribute with the given value.
srcdoc :: Builder -> Attribute ctx
srcdoc = TextAttribute " srcdoc=\""
{-# INLINE srcdoc #-}


-- | Generates an HTML @srclang@ attribute with the given value.
srclang :: Builder -> Attribute ctx
srclang = TextAttribute " srclang=\""
{-# INLINE srclang #-}


-- | Generates an HTML @srcset@ attribute with the given value.
srcset :: Builder -> Attribute ctx
srcset = TextAttribute " srcset=\""
{-# INLINE srcset #-}


-- | Generates an HTML @start@ attribute with the given value.
start :: Builder -> Attribute ctx
start = TextAttribute " start=\""
{-# INLINE start #-}


-- | Generates an HTML @step@ attribute with the given value.
step :: Builder -> Attribute ctx
step = TextAttribute " step=\""
{-# INLINE step #-}


-- | Generates an HTML @style@ attribute with the given value.
style :: Builder -> Attribute ctx
style = TextAttribute " style=\""
{-# INLINE style #-}


-- | Generates an HTML @tabindex@ attribute with the given value.
tabindex :: Builder -> Attribute ctx
tabindex = TextAttribute " tabindex=\""
{-# INLINE tabindex #-}


-- | Generates an HTML @target@ attribute with the given value.
target :: Builder -> Attribute ctx
target = TextAttribute " target=\""
{-# INLINE target #-}


-- | Generates an HTML @title@ attribute with the given value.
title :: Builder -> Attribute ctx
title = TextAttribute " title=\""
{-# INLINE title #-}


-- | Generates an HTML @translate@ attribute with the given value.
translate :: Builder -> Attribute ctx
translate = TextAttribute " translate=\""
{-# INLINE translate #-}


-- | Generates an HTML @type@ attribute with the given value.
type_ :: Builder -> Attribute ctx
type_ = TextAttribute " type=\""
{-# INLINE type_ #-}


-- | Generates an HTML @usemap@ attribute with the given value.
usemap :: Builder -> Attribute ctx
usemap = TextAttribute " usemap=\""
{-# INLINE usemap #-}


-- | Generates an HTML @value@ attribute with the given value.
value :: Builder -> Attribute ctx
value = TextAttribute " value=\""
{-# INLINE value #-}


-- | Generates an HTML @width@ attribute with the given value.
width :: Builder -> Attribute ctx
width = TextAttribute " width=\""
{-# INLINE width #-}


-- | Generates an HTML @wrap@ attribute with the given value.
wrap :: Builder -> Attribute ctx
wrap = TextAttribute " wrap=\""
{-# INLINE wrap #-}


-- | Generates an HTML @writingsuggestions@ attribute with the given value.
writingsuggestions :: Builder -> Attribute ctx
writingsuggestions = TextAttribute " writingsuggestions=\""
{-# INLINE writingsuggestions #-}
