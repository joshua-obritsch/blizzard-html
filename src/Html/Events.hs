{-# LANGUAGE OverloadedStrings #-}

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
