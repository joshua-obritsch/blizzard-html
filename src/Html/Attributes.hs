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


import Data.Bool              (Bool(..))
import Data.Text.Lazy.Builder (Builder)
import Html                   (Attribute(..))


-- ATTRIBUTES


-- | Generates an HTML @abbr@ attribute with the given value.
abbr :: Builder -> Attribute
abbr = TextAttribute " abbr=\""
{-# INLINE abbr #-}


-- | Generates an HTML @accept@ attribute with the given value.
accept :: Builder -> Attribute
accept = TextAttribute " accept=\""
{-# INLINE accept #-}


-- | Generates an HTML @accept-charset@ attribute with the given value.
acceptCharset :: Builder -> Attribute
acceptCharset = TextAttribute " accept-charset=\""
{-# INLINE acceptCharset #-}


-- | Generates an HTML @accesskey@ attribute with the given value.
accesskey :: Builder -> Attribute
accesskey = TextAttribute " accesskey=\""
{-# INLINE accesskey #-}


-- | Generates an HTML @action@ attribute with the given value.
action :: Builder -> Attribute
action = TextAttribute " action=\""
{-# INLINE action #-}


-- | Generates an HTML @allow@ attribute with the given value.
allow :: Builder -> Attribute
allow = TextAttribute " allow=\""
{-# INLINE allow #-}


-- | Generates an HTML @allowfullscreen@ attribute with the given value.
allowfullscreen :: Bool -> Attribute
allowfullscreen = BoolAttribute " allowfullscreen"
{-# INLINE allowfullscreen #-}


-- | Generates an HTML @alt@ attribute with the given value.
alt :: Builder -> Attribute
alt = TextAttribute " alt=\""
{-# INLINE alt #-}


-- | Generates an HTML @as@ attribute with the given value.
as :: Builder -> Attribute
as = TextAttribute " as=\""
{-# INLINE as #-}


-- | Generates an HTML @async@ attribute with the given value.
async :: Bool -> Attribute
async = BoolAttribute " async"
{-# INLINE async #-}


-- | Generates an HTML @autocapitalize@ attribute with the given value.
autocapitalize :: Builder -> Attribute
autocapitalize = TextAttribute " autocapitalize=\""
{-# INLINE autocapitalize #-}


-- | Generates an HTML @autocomplete@ attribute with the given value.
autocomplete :: Builder -> Attribute
autocomplete = TextAttribute " autocomplete=\""
{-# INLINE autocomplete #-}


-- | Generates an HTML @autofocus@ attribute with the given value.
autofocus :: Bool -> Attribute
autofocus = BoolAttribute " autofocus"
{-# INLINE autofocus #-}


-- | Generates an HTML @autoplay@ attribute with the given value.
autoplay :: Bool -> Attribute
autoplay = BoolAttribute " autoplay"
{-# INLINE autoplay #-}


-- | Generates an HTML @blocking@ attribute with the given value.
blocking :: Builder -> Attribute
blocking = TextAttribute " blocking=\""
{-# INLINE blocking #-}


-- | Generates an HTML @charset@ attribute with the given value.
charset :: Builder -> Attribute
charset = TextAttribute " charset=\""
{-# INLINE charset #-}


-- | Generates an HTML @checked@ attribute with the given value.
checked :: Bool -> Attribute
checked = BoolAttribute " checked"
{-# INLINE checked #-}


-- | Generates an HTML @cite@ attribute with the given value.
cite :: Builder -> Attribute
cite = TextAttribute " cite=\""
{-# INLINE cite #-}


-- | Generates an HTML @class@ attribute with the given value.
class_ :: Builder -> Attribute
class_ = TextAttribute " class=\""
{-# INLINE class_ #-}


-- | Generates an HTML @color@ attribute with the given value.
color :: Builder -> Attribute
color = TextAttribute " color=\""
{-# INLINE color #-}


-- | Generates an HTML @cols@ attribute with the given value.
cols :: Builder -> Attribute
cols = TextAttribute " cols=\""
{-# INLINE cols #-}


-- | Generates an HTML @colspan@ attribute with the given value.
colspan :: Builder -> Attribute
colspan = TextAttribute " colspan=\""
{-# INLINE colspan #-}


-- | Generates an HTML @content@ attribute with the given value.
content :: Builder -> Attribute
content = TextAttribute " content=\""
{-# INLINE content #-}


-- | Generates an HTML @contenteditable@ attribute with the given value.
contenteditable :: Builder -> Attribute
contenteditable = TextAttribute " contenteditable=\""
{-# INLINE contenteditable #-}


-- | Generates an HTML @controls@ attribute with the given value.
controls :: Bool -> Attribute
controls = BoolAttribute " controls"
{-# INLINE controls #-}


-- | Generates an HTML @coords@ attribute with the given value.
coords :: Builder -> Attribute
coords = TextAttribute " coords=\""
{-# INLINE coords #-}


-- | Generates an HTML @crossorigin@ attribute with the given value.
crossorigin :: Builder -> Attribute
crossorigin = TextAttribute " crossorigin=\""
{-# INLINE crossorigin #-}


-- | Generates an HTML @data@ attribute with the given value.
data_ :: Builder -> Attribute
data_ = TextAttribute " data=\""
{-# INLINE data_ #-}


-- | Generates an HTML @datetime@ attribute with the given value.
datetime :: Builder -> Attribute
datetime = TextAttribute " datetime=\""
{-# INLINE datetime #-}


-- | Generates an HTML @decoding@ attribute with the given value.
decoding :: Builder -> Attribute
decoding = TextAttribute " decoding=\""
{-# INLINE decoding #-}


-- | Generates an HTML @default@ attribute with the given value.
default_ :: Bool -> Attribute
default_ = BoolAttribute " default"
{-# INLINE default_ #-}


-- | Generates an HTML @defer@ attribute with the given value.
defer :: Bool -> Attribute
defer = BoolAttribute " defer"
{-# INLINE defer #-}


-- | Generates an HTML @dir@ attribute with the given value.
dir :: Builder -> Attribute
dir = TextAttribute " dir=\""
{-# INLINE dir #-}


-- | Generates an HTML @disabled@ attribute with the given value.
disabled :: Bool -> Attribute
disabled = BoolAttribute " disabled"
{-# INLINE disabled #-}


-- | Generates an HTML @download@ attribute with the given value.
download :: Builder -> Attribute
download = TextAttribute " download=\""
{-# INLINE download #-}


-- | Generates an HTML @draggable@ attribute with the given value.
draggable :: Builder -> Attribute
draggable = TextAttribute " draggable=\""
{-# INLINE draggable #-}


-- | Generates an HTML @enctype@ attribute with the given value.
enctype :: Builder -> Attribute
enctype = TextAttribute " enctype=\""
{-# INLINE enctype #-}


-- | Generates an HTML @enterkeyhint@ attribute with the given value.
enterkeyhint :: Builder -> Attribute
enterkeyhint = TextAttribute " enterkeyhint=\""
{-# INLINE enterkeyhint #-}


-- | Generates an HTML @for@ attribute with the given value.
for :: Builder -> Attribute
for = TextAttribute " for=\""
{-# INLINE for #-}


-- | Generates an HTML @form@ attribute with the given value.
form :: Builder -> Attribute
form = TextAttribute " form=\""
{-# INLINE form #-}


-- | Generates an HTML @formaction@ attribute with the given value.
formaction :: Builder -> Attribute
formaction = TextAttribute " formaction=\""
{-# INLINE formaction #-}


-- | Generates an HTML @formenctype@ attribute with the given value.
formenctype :: Builder -> Attribute
formenctype = TextAttribute " formenctype=\""
{-# INLINE formenctype #-}


-- | Generates an HTML @formmethod@ attribute with the given value.
formmethod :: Builder -> Attribute
formmethod = TextAttribute " formmethod=\""
{-# INLINE formmethod #-}


-- | Generates an HTML @formnovalidate@ attribute with the given value.
formnovalidate :: Bool -> Attribute
formnovalidate = BoolAttribute " formnovalidate"
{-# INLINE formnovalidate #-}


-- | Generates an HTML @formtarget@ attribute with the given value.
formtarget :: Builder -> Attribute
formtarget = TextAttribute " formtarget=\""
{-# INLINE formtarget #-}


-- | Generates an HTML @headers@ attribute with the given value.
headers :: Builder -> Attribute
headers = TextAttribute " headers=\""
{-# INLINE headers #-}


-- | Generates an HTML @height@ attribute with the given value.
height :: Builder -> Attribute
height = TextAttribute " height=\""
{-# INLINE height #-}


-- | Generates an HTML @hidden@ attribute with the given value.
hidden :: Bool -> Attribute
hidden = BoolAttribute " hidden"
{-# INLINE hidden #-}


-- | Generates an HTML @high@ attribute with the given value.
high :: Builder -> Attribute
high = TextAttribute " high=\""
{-# INLINE high #-}


-- | Generates an HTML @href@ attribute with the given value.
href :: Builder -> Attribute
href = TextAttribute " href=\""
{-# INLINE href #-}


-- | Generates an HTML @hreflang@ attribute with the given value.
hreflang :: Builder -> Attribute
hreflang = TextAttribute " hreflang=\""
{-# INLINE hreflang #-}


-- | Generates an HTML @http-equiv@ attribute with the given value.
httpEquiv :: Builder -> Attribute
httpEquiv = TextAttribute " http-equiv=\""
{-# INLINE httpEquiv #-}


-- | Generates an HTML @id@ attribute with the given value.
id :: Builder -> Attribute
id = TextAttribute " id=\""
{-# INLINE id #-}


-- | Generates an HTML @imagesizes@ attribute with the given value.
imagesizes :: Builder -> Attribute
imagesizes = TextAttribute " imagesizes=\""
{-# INLINE imagesizes #-}


-- | Generates an HTML @imagesrcset@ attribute with the given value.
imagesrcset :: Builder -> Attribute
imagesrcset = TextAttribute " imagesrcset=\""
{-# INLINE imagesrcset #-}


-- | Generates an HTML @inert@ attribute with the given value.
inert :: Bool -> Attribute
inert = BoolAttribute " inert"
{-# INLINE inert #-}


-- | Generates an HTML @inputmode@ attribute with the given value.
inputmode :: Builder -> Attribute
inputmode = TextAttribute " inputmode=\""
{-# INLINE inputmode #-}


-- | Generates an HTML @integrity@ attribute with the given value.
integrity :: Builder -> Attribute
integrity = TextAttribute " integrity=\""
{-# INLINE integrity #-}


-- | Generates an HTML @is@ attribute with the given value.
is :: Builder -> Attribute
is = TextAttribute " is=\""
{-# INLINE is #-}


-- | Generates an HTML @ismap@ attribute with the given value.
ismap :: Bool -> Attribute
ismap = BoolAttribute " ismap"
{-# INLINE ismap #-}


-- | Generates an HTML @itemid@ attribute with the given value.
itemid :: Builder -> Attribute
itemid = TextAttribute " itemid=\""
{-# INLINE itemid #-}


-- | Generates an HTML @itemprop@ attribute with the given value.
itemprop :: Builder -> Attribute
itemprop = TextAttribute " itemprop=\""
{-# INLINE itemprop #-}


-- | Generates an HTML @itemref@ attribute with the given value.
itemref :: Builder -> Attribute
itemref = TextAttribute " itemref=\""
{-# INLINE itemref #-}


-- | Generates an HTML @itemscope@ attribute with the given value.
itemscope :: Bool -> Attribute
itemscope = BoolAttribute " itemscope"
{-# INLINE itemscope #-}


-- | Generates an HTML @itemtype@ attribute with the given value.
itemtype :: Builder -> Attribute
itemtype = TextAttribute " itemtype=\""
{-# INLINE itemtype #-}


-- | Generates an HTML @kind@ attribute with the given value.
kind :: Builder -> Attribute
kind = TextAttribute " kind=\""
{-# INLINE kind #-}


-- | Generates an HTML @label@ attribute with the given value.
label :: Builder -> Attribute
label = TextAttribute " label=\""
{-# INLINE label #-}


-- | Generates an HTML @lang@ attribute with the given value.
lang :: Builder -> Attribute
lang = TextAttribute " lang=\""
{-# INLINE lang #-}


-- | Generates an HTML @list@ attribute with the given value.
list :: Builder -> Attribute
list = TextAttribute " list=\""
{-# INLINE list #-}


-- | Generates an HTML @loading@ attribute with the given value.
loading :: Builder -> Attribute
loading = TextAttribute " loading=\""
{-# INLINE loading #-}


-- | Generates an HTML @loop@ attribute with the given value.
loop :: Bool -> Attribute
loop = BoolAttribute " loop"
{-# INLINE loop #-}


-- | Generates an HTML @low@ attribute with the given value.
low :: Builder -> Attribute
low = TextAttribute " low=\""
{-# INLINE low #-}


-- | Generates an HTML @max@ attribute with the given value.
max :: Builder -> Attribute
max = TextAttribute " max=\""
{-# INLINE max #-}


-- | Generates an HTML @maxlength@ attribute with the given value.
maxlength :: Builder -> Attribute
maxlength = TextAttribute " maxlength=\""
{-# INLINE maxlength #-}


-- | Generates an HTML @media@ attribute with the given value.
media :: Builder -> Attribute
media = TextAttribute " media=\""
{-# INLINE media #-}


-- | Generates an HTML @method@ attribute with the given value.
method :: Builder -> Attribute
method = TextAttribute " method=\""
{-# INLINE method #-}


-- | Generates an HTML @min@ attribute with the given value.
min :: Builder -> Attribute
min = TextAttribute " min=\""
{-# INLINE min #-}


-- | Generates an HTML @minlength@ attribute with the given value.
minlength :: Builder -> Attribute
minlength = TextAttribute " minlength=\""
{-# INLINE minlength #-}


-- | Generates an HTML @multiple@ attribute with the given value.
multiple :: Bool -> Attribute
multiple = BoolAttribute " multiple"
{-# INLINE multiple #-}


-- | Generates an HTML @muted@ attribute with the given value.
muted :: Bool -> Attribute
muted = BoolAttribute " muted"
{-# INLINE muted #-}


-- | Generates an HTML @name@ attribute with the given value.
name :: Builder -> Attribute
name = TextAttribute " name=\""
{-# INLINE name #-}


-- | Generates an HTML @nomodule@ attribute with the given value.
nomodule :: Bool -> Attribute
nomodule = BoolAttribute " nomodule"
{-# INLINE nomodule #-}


-- | Generates an HTML @nonce@ attribute with the given value.
nonce :: Builder -> Attribute
nonce = TextAttribute " nonce=\""
{-# INLINE nonce #-}


-- | Generates an HTML @novalidate@ attribute with the given value.
novalidate :: Bool -> Attribute
novalidate = BoolAttribute " novalidate"
{-# INLINE novalidate #-}


-- | Generates an HTML @open@ attribute with the given value.
open :: Bool -> Attribute
open = BoolAttribute " open"
{-# INLINE open #-}


-- | Generates an HTML @optimum@ attribute with the given value.
optimum :: Builder -> Attribute
optimum = TextAttribute " optimum=\""
{-# INLINE optimum #-}


-- | Generates an HTML @pattern@ attribute with the given value.
pattern :: Builder -> Attribute
pattern = TextAttribute " pattern=\""
{-# INLINE pattern #-}


-- | Generates an HTML @ping@ attribute with the given value.
ping :: Builder -> Attribute
ping = TextAttribute " ping=\""
{-# INLINE ping #-}


-- | Generates an HTML @placeholder@ attribute with the given value.
placeholder :: Builder -> Attribute
placeholder = TextAttribute " placeholder=\""
{-# INLINE placeholder #-}


-- | Generates an HTML @playsinline@ attribute with the given value.
playsinline :: Bool -> Attribute
playsinline = BoolAttribute " playsinline"
{-# INLINE playsinline #-}


-- | Generates an HTML @poster@ attribute with the given value.
poster :: Builder -> Attribute
poster = TextAttribute " poster=\""
{-# INLINE poster #-}


-- | Generates an HTML @preload@ attribute with the given value.
preload :: Builder -> Attribute
preload = TextAttribute " preload=\""
{-# INLINE preload #-}


-- | Generates an HTML @readonly@ attribute with the given value.
readonly :: Bool -> Attribute
readonly = BoolAttribute " readonly"
{-# INLINE readonly #-}


-- | Generates an HTML @referrerpolicy@ attribute with the given value.
referrerpolicy :: Builder -> Attribute
referrerpolicy = TextAttribute " referrerpolicy=\""
{-# INLINE referrerpolicy #-}


-- | Generates an HTML @rel@ attribute with the given value.
rel :: Builder -> Attribute
rel = TextAttribute " rel=\""
{-# INLINE rel #-}


-- | Generates an HTML @required@ attribute with the given value.
required :: Bool -> Attribute
required = BoolAttribute " required"
{-# INLINE required #-}


-- | Generates an HTML @reversed@ attribute with the given value.
reversed :: Bool -> Attribute
reversed = BoolAttribute " reversed"
{-# INLINE reversed #-}


-- | Generates an HTML @rows@ attribute with the given value.
rows :: Builder -> Attribute
rows = TextAttribute " rows=\""
{-# INLINE rows #-}


-- | Generates an HTML @rowspan@ attribute with the given value.
rowspan :: Builder -> Attribute
rowspan = TextAttribute " rowspan=\""
{-# INLINE rowspan #-}


-- | Generates an HTML @sandbox@ attribute with the given value.
sandbox :: Builder -> Attribute
sandbox = TextAttribute " sandbox=\""
{-# INLINE sandbox #-}


-- | Generates an HTML @scope@ attribute with the given value.
scope :: Builder -> Attribute
scope = TextAttribute " scope=\""
{-# INLINE scope #-}


-- | Generates an HTML @selected@ attribute with the given value.
selected :: Bool -> Attribute
selected = BoolAttribute " selected"
{-# INLINE selected #-}


-- | Generates an HTML @shape@ attribute with the given value.
shape :: Builder -> Attribute
shape = TextAttribute " shape=\""
{-# INLINE shape #-}


-- | Generates an HTML @size@ attribute with the given value.
size :: Builder -> Attribute
size = TextAttribute " size=\""
{-# INLINE size #-}


-- | Generates an HTML @sizes@ attribute with the given value.
sizes :: Builder -> Attribute
sizes = TextAttribute " sizes=\""
{-# INLINE sizes #-}


-- | Generates an HTML @slot@ attribute with the given value.
slot :: Builder -> Attribute
slot = TextAttribute " slot=\""
{-# INLINE slot #-}


-- | Generates an HTML @span@ attribute with the given value.
span :: Builder -> Attribute
span = TextAttribute " span=\""
{-# INLINE span #-}


-- | Generates an HTML @spellcheck@ attribute with the given value.
spellcheck :: Builder -> Attribute
spellcheck = TextAttribute " spellcheck=\""
{-# INLINE spellcheck #-}


-- | Generates an HTML @src@ attribute with the given value.
src :: Builder -> Attribute
src = TextAttribute " src=\""
{-# INLINE src #-}


-- | Generates an HTML @srcdoc@ attribute with the given value.
srcdoc :: Builder -> Attribute
srcdoc = TextAttribute " srcdoc=\""
{-# INLINE srcdoc #-}


-- | Generates an HTML @srclang@ attribute with the given value.
srclang :: Builder -> Attribute
srclang = TextAttribute " srclang=\""
{-# INLINE srclang #-}


-- | Generates an HTML @srcset@ attribute with the given value.
srcset :: Builder -> Attribute
srcset = TextAttribute " srcset=\""
{-# INLINE srcset #-}


-- | Generates an HTML @start@ attribute with the given value.
start :: Builder -> Attribute
start = TextAttribute " start=\""
{-# INLINE start #-}


-- | Generates an HTML @step@ attribute with the given value.
step :: Builder -> Attribute
step = TextAttribute " step=\""
{-# INLINE step #-}


-- | Generates an HTML @style@ attribute with the given value.
style :: Builder -> Attribute
style = TextAttribute " style=\""
{-# INLINE style #-}


-- | Generates an HTML @tabindex@ attribute with the given value.
tabindex :: Builder -> Attribute
tabindex = TextAttribute " tabindex=\""
{-# INLINE tabindex #-}


-- | Generates an HTML @target@ attribute with the given value.
target :: Builder -> Attribute
target = TextAttribute " target=\""
{-# INLINE target #-}


-- | Generates an HTML @title@ attribute with the given value.
title :: Builder -> Attribute
title = TextAttribute " title=\""
{-# INLINE title #-}


-- | Generates an HTML @translate@ attribute with the given value.
translate :: Builder -> Attribute
translate = TextAttribute " translate=\""
{-# INLINE translate #-}


-- | Generates an HTML @type@ attribute with the given value.
type_ :: Builder -> Attribute
type_ = TextAttribute " type=\""
{-# INLINE type_ #-}


-- | Generates an HTML @usemap@ attribute with the given value.
usemap :: Builder -> Attribute
usemap = TextAttribute " usemap=\""
{-# INLINE usemap #-}


-- | Generates an HTML @value@ attribute with the given value.
value :: Builder -> Attribute
value = TextAttribute " value=\""
{-# INLINE value #-}


-- | Generates an HTML @width@ attribute with the given value.
width :: Builder -> Attribute
width = TextAttribute " width=\""
{-# INLINE width #-}


-- | Generates an HTML @wrap@ attribute with the given value.
wrap :: Builder -> Attribute
wrap = TextAttribute " wrap=\""
{-# INLINE wrap #-}
