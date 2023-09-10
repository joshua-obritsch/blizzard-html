{-# LANGUAGE OverloadedStrings #-}

-- | Module    : Html.Events
-- Copyright   : (c) Joshua Obritsch, 2021
-- License     : MIT
-- Maintainer  : joshua@obritsch.com
-- Stability   : Experimental
--
-- The "Html.Events" module provides a set of functions for generating HTML event handlers.
module Html.Events
    ( -- * Event Handlers
      -- ** onauxclick
      onauxclick
      -- ** onafterprint
    , onafterprint
      -- ** onbeforematch
    , onbeforematch
      -- ** onbeforeprint
    , onbeforeprint
      -- ** onbeforeunload
    , onbeforeunload
      -- ** onblur
    , onblur
      -- ** oncancel
    , oncancel
      -- ** oncanplay
    , oncanplay
      -- ** oncanplaythrough
    , oncanplaythrough
      -- ** onchange
    , onchange
      -- ** onclick
    , onclick
      -- ** onclose
    , onclose
      -- ** oncontextlost
    , oncontextlost
      -- ** oncontextmenu
    , oncontextmenu
      -- ** oncontextrestored
    , oncontextrestored
      -- ** oncopy
    , oncopy
      -- ** oncuechange
    , oncuechange
      -- ** oncut
    , oncut
      -- ** ondblclick
    , ondblclick
      -- ** ondrag
    , ondrag
      -- ** ondragend
    , ondragend
      -- ** ondragenter
    , ondragenter
      -- ** ondragleave
    , ondragleave
      -- ** ondragover
    , ondragover
      -- ** ondragstart
    , ondragstart
      -- ** ondrop
    , ondrop
      -- ** ondurationchange
    , ondurationchange
      -- ** onemptied
    , onemptied
      -- ** onended
    , onended
      -- ** onerror
    , onerror
      -- ** onfocus
    , onfocus
      -- ** onformdata
    , onformdata
      -- ** onhashchange
    , onhashchange
      -- ** oninput
    , oninput
      -- ** oninvalid
    , oninvalid
      -- ** onkeydown
    , onkeydown
      -- ** onkeypress
    , onkeypress
      -- ** onkeyup
    , onkeyup
      -- ** onlanguagechange
    , onlanguagechange
      -- ** onload
    , onload
      -- ** onloadeddata
    , onloadeddata
      -- ** onloadedmetadata
    , onloadedmetadata
      -- ** onloadstart
    , onloadstart
      -- ** onmessage
    , onmessage
      -- ** onmessageerror
    , onmessageerror
      -- ** onmousedown
    , onmousedown
      -- ** onmouseenter
    , onmouseenter
      -- ** onmouseleave
    , onmouseleave
      -- ** onmousemove
    , onmousemove
      -- ** onmouseout
    , onmouseout
      -- ** onmouseover
    , onmouseover
      -- ** onmouseup
    , onmouseup
      -- ** onoffline
    , onoffline
      -- ** ononline
    , ononline
      -- ** onpagehide
    , onpagehide
      -- ** onpageshow
    , onpageshow
      -- ** onpaste
    , onpaste
      -- ** onpause
    , onpause
      -- ** onplay
    , onplay
      -- ** onplaying
    , onplaying
      -- ** onpopstate
    , onpopstate
      -- ** onprogress
    , onprogress
      -- ** onratechange
    , onratechange
      -- ** onreset
    , onreset
      -- ** onresize
    , onresize
      -- ** onrejectionhandled
    , onrejectionhandled
      -- ** onscroll
    , onscroll
      -- ** onsecuritypolicyviolation
    , onsecuritypolicyviolation
      -- ** onseeked
    , onseeked
      -- ** onseeking
    , onseeking
      -- ** onselect
    , onselect
      -- ** onslotchange
    , onslotchange
      -- ** onstalled
    , onstalled
      -- ** onstorage
    , onstorage
      -- ** onsubmit
    , onsubmit
      -- ** onsuspend
    , onsuspend
      -- ** ontimeupdate
    , ontimeupdate
      -- ** ontoggle
    , ontoggle
      -- ** onunhandledrejection
    , onunhandledrejection
      -- ** onunload
    , onunload
      -- ** onvolumechange
    , onvolumechange
      -- ** onwaiting
    , onwaiting
      -- ** onwheel
    , onwheel
    ) where


import Data.Text.Lazy.Builder (Builder)
import Html (Attribute(..))


-- EVENT HANDLERS


-- | Generates an HTML @onauxclick@ event handler with the given value.
onauxclick :: Builder -> Attribute
onauxclick = TextAttribute " onauxclick=\""
{-# INLINE onauxclick #-}


-- | Generates an HTML @onafterprint@ event handler with the given value.
onafterprint :: Builder -> Attribute
onafterprint = TextAttribute " onafterprint=\""
{-# INLINE onafterprint #-}


-- | Generates an HTML @onbeforematch@ event handler with the given value.
onbeforematch :: Builder -> Attribute
onbeforematch = TextAttribute " onbeforematch=\""
{-# INLINE onbeforematch #-}


-- | Generates an HTML @onbeforeprint@ event handler with the given value.
onbeforeprint :: Builder -> Attribute
onbeforeprint = TextAttribute " onbeforeprint=\""
{-# INLINE onbeforeprint #-}


-- | Generates an HTML @onbeforeunload@ event handler with the given value.
onbeforeunload :: Builder -> Attribute
onbeforeunload = TextAttribute " onbeforeunload=\""
{-# INLINE onbeforeunload #-}


-- | Generates an HTML @onblur@ event handler with the given value.
onblur :: Builder -> Attribute
onblur = TextAttribute " onblur=\""
{-# INLINE onblur #-}


-- | Generates an HTML @oncancel@ event handler with the given value.
oncancel :: Builder -> Attribute
oncancel = TextAttribute " oncancel=\""
{-# INLINE oncancel #-}


-- | Generates an HTML @oncanplay@ event handler with the given value.
oncanplay :: Builder -> Attribute
oncanplay = TextAttribute " oncanplay=\""
{-# INLINE oncanplay #-}


-- | Generates an HTML @oncanplaythrough@ event handler with the given value.
oncanplaythrough :: Builder -> Attribute
oncanplaythrough = TextAttribute " oncanplaythrough=\""
{-# INLINE oncanplaythrough #-}


-- | Generates an HTML @onchange@ event handler with the given value.
onchange :: Builder -> Attribute
onchange = TextAttribute " onchange=\""
{-# INLINE onchange #-}


-- | Generates an HTML @onclick@ event handler with the given value.
onclick :: Builder -> Attribute
onclick = TextAttribute " onclick=\""
{-# INLINE onclick #-}


-- | Generates an HTML @onclose@ event handler with the given value.
onclose :: Builder -> Attribute
onclose = TextAttribute " onclose=\""
{-# INLINE onclose #-}


-- | Generates an HTML @oncontextlost@ event handler with the given value.
oncontextlost :: Builder -> Attribute
oncontextlost = TextAttribute " oncontextlost=\""
{-# INLINE oncontextlost #-}


-- | Generates an HTML @oncontextmenu@ event handler with the given value.
oncontextmenu :: Builder -> Attribute
oncontextmenu = TextAttribute " oncontextmenu=\""
{-# INLINE oncontextmenu #-}


-- | Generates an HTML @oncontextrestored@ event handler with the given value.
oncontextrestored :: Builder -> Attribute
oncontextrestored = TextAttribute " oncontextrestored=\""
{-# INLINE oncontextrestored #-}


-- | Generates an HTML @oncopy@ event handler with the given value.
oncopy :: Builder -> Attribute
oncopy = TextAttribute " oncopy=\""
{-# INLINE oncopy #-}


-- | Generates an HTML @oncuechange@ event handler with the given value.
oncuechange :: Builder -> Attribute
oncuechange = TextAttribute " oncuechange=\""
{-# INLINE oncuechange #-}


-- | Generates an HTML @oncut@ event handler with the given value.
oncut :: Builder -> Attribute
oncut = TextAttribute " oncut=\""
{-# INLINE oncut #-}


-- | Generates an HTML @ondblclick@ event handler with the given value.
ondblclick :: Builder -> Attribute
ondblclick = TextAttribute " ondblclick=\""
{-# INLINE ondblclick #-}


-- | Generates an HTML @ondrag@ event handler with the given value.
ondrag :: Builder -> Attribute
ondrag = TextAttribute " ondrag=\""
{-# INLINE ondrag #-}


-- | Generates an HTML @ondragend@ event handler with the given value.
ondragend :: Builder -> Attribute
ondragend = TextAttribute " ondragend=\""
{-# INLINE ondragend #-}


-- | Generates an HTML @ondragenter@ event handler with the given value.
ondragenter :: Builder -> Attribute
ondragenter = TextAttribute " ondragenter=\""
{-# INLINE ondragenter #-}


-- | Generates an HTML @ondragleave@ event handler with the given value.
ondragleave :: Builder -> Attribute
ondragleave = TextAttribute " ondragleave=\""
{-# INLINE ondragleave #-}


-- | Generates an HTML @ondragover@ event handler with the given value.
ondragover :: Builder -> Attribute
ondragover = TextAttribute " ondragover=\""
{-# INLINE ondragover #-}


-- | Generates an HTML @ondragstart@ event handler with the given value.
ondragstart :: Builder -> Attribute
ondragstart = TextAttribute " ondragstart=\""
{-# INLINE ondragstart #-}


-- | Generates an HTML @ondrop@ event handler with the given value.
ondrop :: Builder -> Attribute
ondrop = TextAttribute " ondrop=\""
{-# INLINE ondrop #-}


-- | Generates an HTML @ondurationchange@ event handler with the given value.
ondurationchange :: Builder -> Attribute
ondurationchange = TextAttribute " ondurationchange=\""
{-# INLINE ondurationchange #-}


-- | Generates an HTML @onemptied@ event handler with the given value.
onemptied :: Builder -> Attribute
onemptied = TextAttribute " onemptied=\""
{-# INLINE onemptied #-}


-- | Generates an HTML @onended@ event handler with the given value.
onended :: Builder -> Attribute
onended = TextAttribute " onended=\""
{-# INLINE onended #-}


-- | Generates an HTML @onerror@ event handler with the given value.
onerror :: Builder -> Attribute
onerror = TextAttribute " onerror=\""
{-# INLINE onerror #-}


-- | Generates an HTML @onfocus@ event handler with the given value.
onfocus :: Builder -> Attribute
onfocus = TextAttribute " onfocus=\""
{-# INLINE onfocus #-}


-- | Generates an HTML @onformdata@ event handler with the given value.
onformdata :: Builder -> Attribute
onformdata = TextAttribute " onformdata=\""
{-# INLINE onformdata #-}


-- | Generates an HTML @onhashchange@ event handler with the given value.
onhashchange :: Builder -> Attribute
onhashchange = TextAttribute " onhashchange=\""
{-# INLINE onhashchange #-}


-- | Generates an HTML @oninput@ event handler with the given value.
oninput :: Builder -> Attribute
oninput = TextAttribute " oninput=\""
{-# INLINE oninput #-}


-- | Generates an HTML @oninvalid@ event handler with the given value.
oninvalid :: Builder -> Attribute
oninvalid = TextAttribute " oninvalid=\""
{-# INLINE oninvalid #-}


-- | Generates an HTML @onkeydown@ event handler with the given value.
onkeydown :: Builder -> Attribute
onkeydown = TextAttribute " onkeydown=\""
{-# INLINE onkeydown #-}


-- | Generates an HTML @onkeypress@ event handler with the given value.
onkeypress :: Builder -> Attribute
onkeypress = TextAttribute " onkeypress=\""
{-# INLINE onkeypress #-}


-- | Generates an HTML @onkeyup@ event handler with the given value.
onkeyup :: Builder -> Attribute
onkeyup = TextAttribute " onkeyup=\""
{-# INLINE onkeyup #-}


-- | Generates an HTML @onlanguagechange@ event handler with the given value.
onlanguagechange :: Builder -> Attribute
onlanguagechange = TextAttribute " onlanguagechange=\""
{-# INLINE onlanguagechange #-}


-- | Generates an HTML @onload@ event handler with the given value.
onload :: Builder -> Attribute
onload = TextAttribute " onload=\""
{-# INLINE onload #-}


-- | Generates an HTML @onloadeddata@ event handler with the given value.
onloadeddata :: Builder -> Attribute
onloadeddata = TextAttribute " onloadeddata=\""
{-# INLINE onloadeddata #-}


-- | Generates an HTML @onloadedmetadata@ event handler with the given value.
onloadedmetadata :: Builder -> Attribute
onloadedmetadata = TextAttribute " onloadedmetadata=\""
{-# INLINE onloadedmetadata #-}


-- | Generates an HTML @onloadstart@ event handler with the given value.
onloadstart :: Builder -> Attribute
onloadstart = TextAttribute " onloadstart=\""
{-# INLINE onloadstart #-}


-- | Generates an HTML @onmessage@ event handler with the given value.
onmessage :: Builder -> Attribute
onmessage = TextAttribute " onmessage=\""
{-# INLINE onmessage #-}


-- | Generates an HTML @onmessageerror@ event handler with the given value.
onmessageerror :: Builder -> Attribute
onmessageerror = TextAttribute " onmessageerror=\""
{-# INLINE onmessageerror #-}


-- | Generates an HTML @onmousedown@ event handler with the given value.
onmousedown :: Builder -> Attribute
onmousedown = TextAttribute " onmousedown=\""
{-# INLINE onmousedown #-}


-- | Generates an HTML @onmouseenter@ event handler with the given value.
onmouseenter :: Builder -> Attribute
onmouseenter = TextAttribute " onmouseenter=\""
{-# INLINE onmouseenter #-}


-- | Generates an HTML @onmouseleave@ event handler with the given value.
onmouseleave :: Builder -> Attribute
onmouseleave = TextAttribute " onmouseleave=\""
{-# INLINE onmouseleave #-}


-- | Generates an HTML @onmousemove@ event handler with the given value.
onmousemove :: Builder -> Attribute
onmousemove = TextAttribute " onmousemove=\""
{-# INLINE onmousemove #-}


-- | Generates an HTML @onmouseout@ event handler with the given value.
onmouseout :: Builder -> Attribute
onmouseout = TextAttribute " onmouseout=\""
{-# INLINE onmouseout #-}


-- | Generates an HTML @onmouseover@ event handler with the given value.
onmouseover :: Builder -> Attribute
onmouseover = TextAttribute " onmouseover=\""
{-# INLINE onmouseover #-}


-- | Generates an HTML @onmouseup@ event handler with the given value.
onmouseup :: Builder -> Attribute
onmouseup = TextAttribute " onmouseup=\""
{-# INLINE onmouseup #-}


-- | Generates an HTML @onoffline@ event handler with the given value.
onoffline :: Builder -> Attribute
onoffline = TextAttribute " onoffline=\""
{-# INLINE onoffline #-}


-- | Generates an HTML @ononline@ event handler with the given value.
ononline :: Builder -> Attribute
ononline = TextAttribute " ononline=\""
{-# INLINE ononline #-}


-- | Generates an HTML @onpagehide@ event handler with the given value.
onpagehide :: Builder -> Attribute
onpagehide = TextAttribute " onpagehide=\""
{-# INLINE onpagehide #-}


-- | Generates an HTML @onpageshow@ event handler with the given value.
onpageshow :: Builder -> Attribute
onpageshow = TextAttribute " onpageshow=\""
{-# INLINE onpageshow #-}


-- | Generates an HTML @onpaste@ event handler with the given value.
onpaste :: Builder -> Attribute
onpaste = TextAttribute " onpaste=\""
{-# INLINE onpaste #-}


-- | Generates an HTML @onpause@ event handler with the given value.
onpause :: Builder -> Attribute
onpause = TextAttribute " onpause=\""
{-# INLINE onpause #-}


-- | Generates an HTML @onplay@ event handler with the given value.
onplay :: Builder -> Attribute
onplay = TextAttribute " onplay=\""
{-# INLINE onplay #-}


-- | Generates an HTML @onplaying@ event handler with the given value.
onplaying :: Builder -> Attribute
onplaying = TextAttribute " onplaying=\""
{-# INLINE onplaying #-}


-- | Generates an HTML @onpopstate@ event handler with the given value.
onpopstate :: Builder -> Attribute
onpopstate = TextAttribute " onpopstate=\""
{-# INLINE onpopstate #-}


-- | Generates an HTML @onprogress@ event handler with the given value.
onprogress :: Builder -> Attribute
onprogress = TextAttribute " onprogress=\""
{-# INLINE onprogress #-}


-- | Generates an HTML @onratechange@ event handler with the given value.
onratechange :: Builder -> Attribute
onratechange = TextAttribute " onratechange=\""
{-# INLINE onratechange #-}


-- | Generates an HTML @onreset@ event handler with the given value.
onreset :: Builder -> Attribute
onreset = TextAttribute " onreset=\""
{-# INLINE onreset #-}


-- | Generates an HTML @onresize@ event handler with the given value.
onresize :: Builder -> Attribute
onresize = TextAttribute " onresize=\""
{-# INLINE onresize #-}


-- | Generates an HTML @onrejectionhandled@ event handler with the given value.
onrejectionhandled :: Builder -> Attribute
onrejectionhandled = TextAttribute " onrejectionhandled=\""
{-# INLINE onrejectionhandled #-}


-- | Generates an HTML @onscroll@ event handler with the given value.
onscroll :: Builder -> Attribute
onscroll = TextAttribute " onscroll=\""
{-# INLINE onscroll #-}


-- | Generates an HTML @onsecuritypolicyviolation@ event handler with the given value.
onsecuritypolicyviolation :: Builder -> Attribute
onsecuritypolicyviolation = TextAttribute " onsecuritypolicyviolation=\""
{-# INLINE onsecuritypolicyviolation #-}


-- | Generates an HTML @onseeked@ event handler with the given value.
onseeked :: Builder -> Attribute
onseeked = TextAttribute " onseeked=\""
{-# INLINE onseeked #-}


-- | Generates an HTML @onseeking@ event handler with the given value.
onseeking :: Builder -> Attribute
onseeking = TextAttribute " onseeking=\""
{-# INLINE onseeking #-}


-- | Generates an HTML @onselect@ event handler with the given value.
onselect :: Builder -> Attribute
onselect = TextAttribute " onselect=\""
{-# INLINE onselect #-}


-- | Generates an HTML @onslotchange@ event handler with the given value.
onslotchange :: Builder -> Attribute
onslotchange = TextAttribute " onslotchange=\""
{-# INLINE onslotchange #-}


-- | Generates an HTML @onstalled@ event handler with the given value.
onstalled :: Builder -> Attribute
onstalled = TextAttribute " onstalled=\""
{-# INLINE onstalled #-}


-- | Generates an HTML @onstorage@ event handler with the given value.
onstorage :: Builder -> Attribute
onstorage = TextAttribute " onstorage=\""
{-# INLINE onstorage #-}


-- | Generates an HTML @onsubmit@ event handler with the given value.
onsubmit :: Builder -> Attribute
onsubmit = TextAttribute " onsubmit=\""
{-# INLINE onsubmit #-}


-- | Generates an HTML @onsuspend@ event handler with the given value.
onsuspend :: Builder -> Attribute
onsuspend = TextAttribute " onsuspend=\""
{-# INLINE onsuspend #-}


-- | Generates an HTML @ontimeupdate@ event handler with the given value.
ontimeupdate :: Builder -> Attribute
ontimeupdate = TextAttribute " ontimeupdate=\""
{-# INLINE ontimeupdate #-}


-- | Generates an HTML @ontoggle@ event handler with the given value.
ontoggle :: Builder -> Attribute
ontoggle = TextAttribute " ontoggle=\""
{-# INLINE ontoggle #-}


-- | Generates an HTML @onunhandledrejection@ event handler with the given value.
onunhandledrejection :: Builder -> Attribute
onunhandledrejection = TextAttribute " onunhandledrejection=\""
{-# INLINE onunhandledrejection #-}


-- | Generates an HTML @onunload@ event handler with the given value.
onunload :: Builder -> Attribute
onunload = TextAttribute " onunload=\""
{-# INLINE onunload #-}


-- | Generates an HTML @onvolumechange@ event handler with the given value.
onvolumechange :: Builder -> Attribute
onvolumechange = TextAttribute " onvolumechange=\""
{-# INLINE onvolumechange #-}


-- | Generates an HTML @onwaiting@ event handler with the given value.
onwaiting :: Builder -> Attribute
onwaiting = TextAttribute " onwaiting=\""
{-# INLINE onwaiting #-}


-- | Generates an HTML @onwheel@ event handler with the given value.
onwheel :: Builder -> Attribute
onwheel = TextAttribute " onwheel=\""
{-# INLINE onwheel #-}
