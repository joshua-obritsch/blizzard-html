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
      -- ** onafterprint
      onafterprint
      -- ** onauxclick
    , onauxclick
      -- ** onbeforeinput
    , onbeforeinput
      -- ** onbeforematch
    , onbeforematch
      -- ** onbeforeprint
    , onbeforeprint
      -- ** onbeforetoggle
    , onbeforetoggle
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
      -- ** oncommand
    , oncommand
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
      -- ** onpagereveal
    , onpagereveal
      -- ** onpageshow
    , onpageshow
      -- ** onpageswap
    , onpageswap
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
      -- ** onscrollend
    , onscrollend
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
import Html                   (Attribute(..))


-- EVENT HANDLERS


-- | Generates an HTML @onafterprint@ event handler with the given value.
onafterprint :: Builder -> Attribute ctx
onafterprint = TextAttribute " onafterprint=\""
{-# INLINE onafterprint #-}


-- | Generates an HTML @onauxclick@ event handler with the given value.
onauxclick :: Builder -> Attribute ctx
onauxclick = TextAttribute " onauxclick=\""
{-# INLINE onauxclick #-}


-- | Generates an HTML @onbeforeinput@ event handler with the given value.
onbeforeinput :: Builder -> Attribute ctx
onbeforeinput = TextAttribute " onbeforeinput=\""
{-# INLINE onbeforeinput #-}


-- | Generates an HTML @onbeforematch@ event handler with the given value.
onbeforematch :: Builder -> Attribute ctx
onbeforematch = TextAttribute " onbeforematch=\""
{-# INLINE onbeforematch #-}


-- | Generates an HTML @onbeforeprint@ event handler with the given value.
onbeforeprint :: Builder -> Attribute ctx
onbeforeprint = TextAttribute " onbeforeprint=\""
{-# INLINE onbeforeprint #-}


-- | Generates an HTML @onbeforetoggle@ event handler with the given value.
onbeforetoggle :: Builder -> Attribute ctx
onbeforetoggle = TextAttribute " onbeforetoggle=\""
{-# INLINE onbeforetoggle #-}


-- | Generates an HTML @onbeforeunload@ event handler with the given value.
onbeforeunload :: Builder -> Attribute ctx
onbeforeunload = TextAttribute " onbeforeunload=\""
{-# INLINE onbeforeunload #-}


-- | Generates an HTML @onblur@ event handler with the given value.
onblur :: Builder -> Attribute ctx
onblur = TextAttribute " onblur=\""
{-# INLINE onblur #-}


-- | Generates an HTML @oncancel@ event handler with the given value.
oncancel :: Builder -> Attribute ctx
oncancel = TextAttribute " oncancel=\""
{-# INLINE oncancel #-}


-- | Generates an HTML @oncanplay@ event handler with the given value.
oncanplay :: Builder -> Attribute ctx
oncanplay = TextAttribute " oncanplay=\""
{-# INLINE oncanplay #-}


-- | Generates an HTML @oncanplaythrough@ event handler with the given value.
oncanplaythrough :: Builder -> Attribute ctx
oncanplaythrough = TextAttribute " oncanplaythrough=\""
{-# INLINE oncanplaythrough #-}


-- | Generates an HTML @onchange@ event handler with the given value.
onchange :: Builder -> Attribute ctx
onchange = TextAttribute " onchange=\""
{-# INLINE onchange #-}


-- | Generates an HTML @onclick@ event handler with the given value.
onclick :: Builder -> Attribute ctx
onclick = TextAttribute " onclick=\""
{-# INLINE onclick #-}


-- | Generates an HTML @onclose@ event handler with the given value.
onclose :: Builder -> Attribute ctx
onclose = TextAttribute " onclose=\""
{-# INLINE onclose #-}


-- | Generates an HTML @oncommand@ event handler with the given value.
oncommand :: Builder -> Attribute ctx
oncommand = TextAttribute " oncommand=\""
{-# INLINE oncommand #-}


-- | Generates an HTML @oncontextlost@ event handler with the given value.
oncontextlost :: Builder -> Attribute ctx
oncontextlost = TextAttribute " oncontextlost=\""
{-# INLINE oncontextlost #-}


-- | Generates an HTML @oncontextmenu@ event handler with the given value.
oncontextmenu :: Builder -> Attribute ctx
oncontextmenu = TextAttribute " oncontextmenu=\""
{-# INLINE oncontextmenu #-}


-- | Generates an HTML @oncontextrestored@ event handler with the given value.
oncontextrestored :: Builder -> Attribute ctx
oncontextrestored = TextAttribute " oncontextrestored=\""
{-# INLINE oncontextrestored #-}


-- | Generates an HTML @oncopy@ event handler with the given value.
oncopy :: Builder -> Attribute ctx
oncopy = TextAttribute " oncopy=\""
{-# INLINE oncopy #-}


-- | Generates an HTML @oncuechange@ event handler with the given value.
oncuechange :: Builder -> Attribute ctx
oncuechange = TextAttribute " oncuechange=\""
{-# INLINE oncuechange #-}


-- | Generates an HTML @oncut@ event handler with the given value.
oncut :: Builder -> Attribute ctx
oncut = TextAttribute " oncut=\""
{-# INLINE oncut #-}


-- | Generates an HTML @ondblclick@ event handler with the given value.
ondblclick :: Builder -> Attribute ctx
ondblclick = TextAttribute " ondblclick=\""
{-# INLINE ondblclick #-}


-- | Generates an HTML @ondrag@ event handler with the given value.
ondrag :: Builder -> Attribute ctx
ondrag = TextAttribute " ondrag=\""
{-# INLINE ondrag #-}


-- | Generates an HTML @ondragend@ event handler with the given value.
ondragend :: Builder -> Attribute ctx
ondragend = TextAttribute " ondragend=\""
{-# INLINE ondragend #-}


-- | Generates an HTML @ondragenter@ event handler with the given value.
ondragenter :: Builder -> Attribute ctx
ondragenter = TextAttribute " ondragenter=\""
{-# INLINE ondragenter #-}


-- | Generates an HTML @ondragleave@ event handler with the given value.
ondragleave :: Builder -> Attribute ctx
ondragleave = TextAttribute " ondragleave=\""
{-# INLINE ondragleave #-}


-- | Generates an HTML @ondragover@ event handler with the given value.
ondragover :: Builder -> Attribute ctx
ondragover = TextAttribute " ondragover=\""
{-# INLINE ondragover #-}


-- | Generates an HTML @ondragstart@ event handler with the given value.
ondragstart :: Builder -> Attribute ctx
ondragstart = TextAttribute " ondragstart=\""
{-# INLINE ondragstart #-}


-- | Generates an HTML @ondrop@ event handler with the given value.
ondrop :: Builder -> Attribute ctx
ondrop = TextAttribute " ondrop=\""
{-# INLINE ondrop #-}


-- | Generates an HTML @ondurationchange@ event handler with the given value.
ondurationchange :: Builder -> Attribute ctx
ondurationchange = TextAttribute " ondurationchange=\""
{-# INLINE ondurationchange #-}


-- | Generates an HTML @onemptied@ event handler with the given value.
onemptied :: Builder -> Attribute ctx
onemptied = TextAttribute " onemptied=\""
{-# INLINE onemptied #-}


-- | Generates an HTML @onended@ event handler with the given value.
onended :: Builder -> Attribute ctx
onended = TextAttribute " onended=\""
{-# INLINE onended #-}


-- | Generates an HTML @onerror@ event handler with the given value.
onerror :: Builder -> Attribute ctx
onerror = TextAttribute " onerror=\""
{-# INLINE onerror #-}


-- | Generates an HTML @onfocus@ event handler with the given value.
onfocus :: Builder -> Attribute ctx
onfocus = TextAttribute " onfocus=\""
{-# INLINE onfocus #-}


-- | Generates an HTML @onformdata@ event handler with the given value.
onformdata :: Builder -> Attribute ctx
onformdata = TextAttribute " onformdata=\""
{-# INLINE onformdata #-}


-- | Generates an HTML @onhashchange@ event handler with the given value.
onhashchange :: Builder -> Attribute ctx
onhashchange = TextAttribute " onhashchange=\""
{-# INLINE onhashchange #-}


-- | Generates an HTML @oninput@ event handler with the given value.
oninput :: Builder -> Attribute ctx
oninput = TextAttribute " oninput=\""
{-# INLINE oninput #-}


-- | Generates an HTML @oninvalid@ event handler with the given value.
oninvalid :: Builder -> Attribute ctx
oninvalid = TextAttribute " oninvalid=\""
{-# INLINE oninvalid #-}


-- | Generates an HTML @onkeydown@ event handler with the given value.
onkeydown :: Builder -> Attribute ctx
onkeydown = TextAttribute " onkeydown=\""
{-# INLINE onkeydown #-}


-- | Generates an HTML @onkeypress@ event handler with the given value.
onkeypress :: Builder -> Attribute ctx
onkeypress = TextAttribute " onkeypress=\""
{-# INLINE onkeypress #-}


-- | Generates an HTML @onkeyup@ event handler with the given value.
onkeyup :: Builder -> Attribute ctx
onkeyup = TextAttribute " onkeyup=\""
{-# INLINE onkeyup #-}


-- | Generates an HTML @onlanguagechange@ event handler with the given value.
onlanguagechange :: Builder -> Attribute ctx
onlanguagechange = TextAttribute " onlanguagechange=\""
{-# INLINE onlanguagechange #-}


-- | Generates an HTML @onload@ event handler with the given value.
onload :: Builder -> Attribute ctx
onload = TextAttribute " onload=\""
{-# INLINE onload #-}


-- | Generates an HTML @onloadeddata@ event handler with the given value.
onloadeddata :: Builder -> Attribute ctx
onloadeddata = TextAttribute " onloadeddata=\""
{-# INLINE onloadeddata #-}


-- | Generates an HTML @onloadedmetadata@ event handler with the given value.
onloadedmetadata :: Builder -> Attribute ctx
onloadedmetadata = TextAttribute " onloadedmetadata=\""
{-# INLINE onloadedmetadata #-}


-- | Generates an HTML @onloadstart@ event handler with the given value.
onloadstart :: Builder -> Attribute ctx
onloadstart = TextAttribute " onloadstart=\""
{-# INLINE onloadstart #-}


-- | Generates an HTML @onmessage@ event handler with the given value.
onmessage :: Builder -> Attribute ctx
onmessage = TextAttribute " onmessage=\""
{-# INLINE onmessage #-}


-- | Generates an HTML @onmessageerror@ event handler with the given value.
onmessageerror :: Builder -> Attribute ctx
onmessageerror = TextAttribute " onmessageerror=\""
{-# INLINE onmessageerror #-}


-- | Generates an HTML @onmousedown@ event handler with the given value.
onmousedown :: Builder -> Attribute ctx
onmousedown = TextAttribute " onmousedown=\""
{-# INLINE onmousedown #-}


-- | Generates an HTML @onmouseenter@ event handler with the given value.
onmouseenter :: Builder -> Attribute ctx
onmouseenter = TextAttribute " onmouseenter=\""
{-# INLINE onmouseenter #-}


-- | Generates an HTML @onmouseleave@ event handler with the given value.
onmouseleave :: Builder -> Attribute ctx
onmouseleave = TextAttribute " onmouseleave=\""
{-# INLINE onmouseleave #-}


-- | Generates an HTML @onmousemove@ event handler with the given value.
onmousemove :: Builder -> Attribute ctx
onmousemove = TextAttribute " onmousemove=\""
{-# INLINE onmousemove #-}


-- | Generates an HTML @onmouseout@ event handler with the given value.
onmouseout :: Builder -> Attribute ctx
onmouseout = TextAttribute " onmouseout=\""
{-# INLINE onmouseout #-}


-- | Generates an HTML @onmouseover@ event handler with the given value.
onmouseover :: Builder -> Attribute ctx
onmouseover = TextAttribute " onmouseover=\""
{-# INLINE onmouseover #-}


-- | Generates an HTML @onmouseup@ event handler with the given value.
onmouseup :: Builder -> Attribute ctx
onmouseup = TextAttribute " onmouseup=\""
{-# INLINE onmouseup #-}


-- | Generates an HTML @onoffline@ event handler with the given value.
onoffline :: Builder -> Attribute ctx
onoffline = TextAttribute " onoffline=\""
{-# INLINE onoffline #-}


-- | Generates an HTML @ononline@ event handler with the given value.
ononline :: Builder -> Attribute ctx
ononline = TextAttribute " ononline=\""
{-# INLINE ononline #-}


-- | Generates an HTML @onpagehide@ event handler with the given value.
onpagehide :: Builder -> Attribute ctx
onpagehide = TextAttribute " onpagehide=\""
{-# INLINE onpagehide #-}


-- | Generates an HTML @onpagereveal@ event handler with the given value.
onpagereveal :: Builder -> Attribute ctx
onpagereveal = TextAttribute " onpagereveal=\""
{-# INLINE onpagereveal #-}


-- | Generates an HTML @onpageshow@ event handler with the given value.
onpageshow :: Builder -> Attribute ctx
onpageshow = TextAttribute " onpageshow=\""
{-# INLINE onpageshow #-}


-- | Generates an HTML @onpageswap@ event handler with the given value.
onpageswap :: Builder -> Attribute ctx
onpageswap = TextAttribute " onpageswap=\""
{-# INLINE onpageswap #-}


-- | Generates an HTML @onpaste@ event handler with the given value.
onpaste :: Builder -> Attribute ctx
onpaste = TextAttribute " onpaste=\""
{-# INLINE onpaste #-}


-- | Generates an HTML @onpause@ event handler with the given value.
onpause :: Builder -> Attribute ctx
onpause = TextAttribute " onpause=\""
{-# INLINE onpause #-}


-- | Generates an HTML @onplay@ event handler with the given value.
onplay :: Builder -> Attribute ctx
onplay = TextAttribute " onplay=\""
{-# INLINE onplay #-}


-- | Generates an HTML @onplaying@ event handler with the given value.
onplaying :: Builder -> Attribute ctx
onplaying = TextAttribute " onplaying=\""
{-# INLINE onplaying #-}


-- | Generates an HTML @onpopstate@ event handler with the given value.
onpopstate :: Builder -> Attribute ctx
onpopstate = TextAttribute " onpopstate=\""
{-# INLINE onpopstate #-}


-- | Generates an HTML @onprogress@ event handler with the given value.
onprogress :: Builder -> Attribute ctx
onprogress = TextAttribute " onprogress=\""
{-# INLINE onprogress #-}


-- | Generates an HTML @onratechange@ event handler with the given value.
onratechange :: Builder -> Attribute ctx
onratechange = TextAttribute " onratechange=\""
{-# INLINE onratechange #-}


-- | Generates an HTML @onreset@ event handler with the given value.
onreset :: Builder -> Attribute ctx
onreset = TextAttribute " onreset=\""
{-# INLINE onreset #-}


-- | Generates an HTML @onresize@ event handler with the given value.
onresize :: Builder -> Attribute ctx
onresize = TextAttribute " onresize=\""
{-# INLINE onresize #-}


-- | Generates an HTML @onrejectionhandled@ event handler with the given value.
onrejectionhandled :: Builder -> Attribute ctx
onrejectionhandled = TextAttribute " onrejectionhandled=\""
{-# INLINE onrejectionhandled #-}


-- | Generates an HTML @onscroll@ event handler with the given value.
onscroll :: Builder -> Attribute ctx
onscroll = TextAttribute " onscroll=\""
{-# INLINE onscroll #-}


-- | Generates an HTML @onscrollend@ event handler with the given value.
onscrollend :: Builder -> Attribute ctx
onscrollend = TextAttribute " onscrollend=\""
{-# INLINE onscrollend #-}


-- | Generates an HTML @onsecuritypolicyviolation@ event handler with the given value.
onsecuritypolicyviolation :: Builder -> Attribute ctx
onsecuritypolicyviolation = TextAttribute " onsecuritypolicyviolation=\""
{-# INLINE onsecuritypolicyviolation #-}


-- | Generates an HTML @onseeked@ event handler with the given value.
onseeked :: Builder -> Attribute ctx
onseeked = TextAttribute " onseeked=\""
{-# INLINE onseeked #-}


-- | Generates an HTML @onseeking@ event handler with the given value.
onseeking :: Builder -> Attribute ctx
onseeking = TextAttribute " onseeking=\""
{-# INLINE onseeking #-}


-- | Generates an HTML @onselect@ event handler with the given value.
onselect :: Builder -> Attribute ctx
onselect = TextAttribute " onselect=\""
{-# INLINE onselect #-}


-- | Generates an HTML @onslotchange@ event handler with the given value.
onslotchange :: Builder -> Attribute ctx
onslotchange = TextAttribute " onslotchange=\""
{-# INLINE onslotchange #-}


-- | Generates an HTML @onstalled@ event handler with the given value.
onstalled :: Builder -> Attribute ctx
onstalled = TextAttribute " onstalled=\""
{-# INLINE onstalled #-}


-- | Generates an HTML @onstorage@ event handler with the given value.
onstorage :: Builder -> Attribute ctx
onstorage = TextAttribute " onstorage=\""
{-# INLINE onstorage #-}


-- | Generates an HTML @onsubmit@ event handler with the given value.
onsubmit :: Builder -> Attribute ctx
onsubmit = TextAttribute " onsubmit=\""
{-# INLINE onsubmit #-}


-- | Generates an HTML @onsuspend@ event handler with the given value.
onsuspend :: Builder -> Attribute ctx
onsuspend = TextAttribute " onsuspend=\""
{-# INLINE onsuspend #-}


-- | Generates an HTML @ontimeupdate@ event handler with the given value.
ontimeupdate :: Builder -> Attribute ctx
ontimeupdate = TextAttribute " ontimeupdate=\""
{-# INLINE ontimeupdate #-}


-- | Generates an HTML @ontoggle@ event handler with the given value.
ontoggle :: Builder -> Attribute ctx
ontoggle = TextAttribute " ontoggle=\""
{-# INLINE ontoggle #-}


-- | Generates an HTML @onunhandledrejection@ event handler with the given value.
onunhandledrejection :: Builder -> Attribute ctx
onunhandledrejection = TextAttribute " onunhandledrejection=\""
{-# INLINE onunhandledrejection #-}


-- | Generates an HTML @onunload@ event handler with the given value.
onunload :: Builder -> Attribute ctx
onunload = TextAttribute " onunload=\""
{-# INLINE onunload #-}


-- | Generates an HTML @onvolumechange@ event handler with the given value.
onvolumechange :: Builder -> Attribute ctx
onvolumechange = TextAttribute " onvolumechange=\""
{-# INLINE onvolumechange #-}


-- | Generates an HTML @onwaiting@ event handler with the given value.
onwaiting :: Builder -> Attribute ctx
onwaiting = TextAttribute " onwaiting=\""
{-# INLINE onwaiting #-}


-- | Generates an HTML @onwheel@ event handler with the given value.
onwheel :: Builder -> Attribute ctx
onwheel = TextAttribute " onwheel=\""
{-# INLINE onwheel #-}
