{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Module    : Html.Math
-- Copyright   : (c) Joshua Obritsch, 2025
-- License     : MIT
-- Maintainer  : joshua@obritsch.com
-- Stability   : Experimental
--
-- The "Html.Math" module provides a set of functions for generating MathML elements and attributes.
module Html.Math
    ( -- * Elements
      -- ** \<annotation\>
      annotation
      -- ** \<annotation-xml\>
    , annotationXml
      -- ** \<maction\>
    , maction
      -- ** \<math\>
    , math
      -- ** \<merror\>
    , merror
      -- ** \<mfrac\>
    , mfrac
      -- ** \<mi\>
    , mi
      -- ** \<mmultiscripts\>
    , mmultiscripts
      -- ** \<mn\>
    , mn
      -- ** \<mo\>
    , mo
      -- ** \<mover\>
    , mover
      -- ** \<mpadded\>
    , mpadded
      -- ** \<mphantom\>
    , mphantom
      -- ** \<mprescripts\>
    , mprescripts
      -- ** \<mroot\>
    , mroot
      -- ** \<mrow\>
    , mrow
      -- ** \<ms\>
    , ms
      -- ** \<mspace\>
    , mspace
      -- ** \<msqrt\>
    , msqrt
      -- ** \<mstyle\>
    , mstyle
      -- ** \<msub\>
    , msub
      -- ** \<msubsup\>
    , msubsup
      -- ** \<msup\>
    , msup
      -- ** \<mtable\>
    , mtable
      -- ** \<mtd\>
    , mtd
      -- ** \<mtext\>
    , mtext
      -- ** \<mtr\>
    , mtr
      -- ** \<munder\>
    , munder
      -- ** \<munderover\>
    , munderover
      -- ** \<semantics\>
    , semantics

      -- * Attributes
      -- ** accent
    , accent
      -- ** accentunder
    , accentunder
      -- ** actiontype
    , actiontype
      -- ** alttext
    , alttext
      -- ** columnspan
    , columnspan
      -- ** depth
    , depth
      -- ** display
    , display
      -- ** displaystyle
    , displaystyle
      -- ** encoding
    , encoding
      -- ** fence
    , fence
      -- ** largeop
    , largeop
      -- ** linethickness
    , linethickness
      -- ** lspace
    , lspace
      -- ** mathbackground
    , mathbackground
      -- ** mathcolor
    , mathcolor
      -- ** mathsize
    , mathsize
      -- ** maxsize
    , maxsize
      -- ** minsize
    , minsize
      -- ** movablelimits
    , movablelimits
      -- ** rowspan
    , rowspan
      -- ** rspace
    , rspace
      -- ** scriptlevel
    , scriptlevel
      -- ** selection
    , selection
      -- ** separator
    , separator
      -- ** stretchy
    , stretchy
      -- ** symmetric
    , symmetric
      -- ** voffset
    , voffset
    ) where


import Data.Text.Lazy.Builder (Builder)
import Html                   (Attribute(..), Html(..))


-- ELEMENTS


-- | Generates an HTML @\<annotation\>@ element with the given attributes and contents.
annotation :: [Attribute ctx] -> [Html ctx] -> Html ctx
annotation = ParentNode "<annotation" "</annotation>"
{-# INLINE annotation #-}


-- | Generates an HTML @\<annotation-xml\>@ element with the given attributes and contents.
annotationXml :: [Attribute ctx] -> [Html ctx] -> Html ctx
annotationXml = ParentNode "<annotation-xml" "</annotation-xml>"
{-# INLINE annotationXml #-}


-- | Generates an HTML @\<maction\>@ element with the given attributes and contents.
maction :: [Attribute ctx] -> [Html ctx] -> Html ctx
maction = ParentNode "<maction" "</maction>"
{-# INLINE maction #-}


-- | Generates an HTML @\<math\>@ element with the given attributes and contents.
math :: [Attribute ctx] -> [Html ctx] -> Html ctx
math = ParentNode "<math" "</math>"
{-# INLINE math #-}


-- | Generates an HTML @\<merror\>@ element with the given attributes and contents.
merror :: [Attribute ctx] -> [Html ctx] -> Html ctx
merror = ParentNode "<merror" "</merror>"
{-# INLINE merror #-}


-- | Generates an HTML @\<mfrac\>@ element with the given attributes and contents.
mfrac :: [Attribute ctx] -> [Html ctx] -> Html ctx
mfrac = ParentNode "<mfrac" "</mfrac>"
{-# INLINE mfrac #-}


-- | Generates an HTML @\<mi\>@ element with the given attributes and contents.
mi :: [Attribute ctx] -> [Html ctx] -> Html ctx
mi = ParentNode "<mi" "</mi>"
{-# INLINE mi #-}


-- | Generates an HTML @\<mmultiscripts\>@ element with the given attributes and contents.
mmultiscripts :: [Attribute ctx] -> [Html ctx] -> Html ctx
mmultiscripts = ParentNode "<mmultiscripts" "</mmultiscripts>"
{-# INLINE mmultiscripts #-}


-- | Generates an HTML @\<mn\>@ element with the given attributes and contents.
mn :: [Attribute ctx] -> [Html ctx] -> Html ctx
mn = ParentNode "<mn" "</mn>"
{-# INLINE mn #-}


-- | Generates an HTML @\<mo\>@ element with the given attributes and contents.
mo :: [Attribute ctx] -> [Html ctx] -> Html ctx
mo = ParentNode "<mo" "</mo>"
{-# INLINE mo #-}


-- | Generates an HTML @\<mover\>@ element with the given attributes and contents.
mover :: [Attribute ctx] -> [Html ctx] -> Html ctx
mover = ParentNode "<mover" "</mover>"
{-# INLINE mover #-}


-- | Generates an HTML @\<mpadded\>@ element with the given attributes and contents.
mpadded :: [Attribute ctx] -> [Html ctx] -> Html ctx
mpadded = ParentNode "<mpadded" "</mpadded>"
{-# INLINE mpadded #-}


-- | Generates an HTML @\<mphantom\>@ element with the given attributes and contents.
mphantom :: [Attribute ctx] -> [Html ctx] -> Html ctx
mphantom = ParentNode "<mphantom" "</mphantom>"
{-# INLINE mphantom #-}


-- | Generates an HTML @\<mprescripts\>@ element with the given attributes.
mprescripts :: [Attribute ctx] -> Html ctx
mprescripts = LeafNode "<mprescripts"
{-# INLINE mprescripts #-}


-- | Generates an HTML @\<mroot\>@ element with the given attributes and contents.
mroot :: [Attribute ctx] -> [Html ctx] -> Html ctx
mroot = ParentNode "<mroot" "</mroot>"
{-# INLINE mroot #-}


-- | Generates an HTML @\<mrow\>@ element with the given attributes and contents.
mrow :: [Attribute ctx] -> [Html ctx] -> Html ctx
mrow = ParentNode "<mrow" "</mrow>"
{-# INLINE mrow #-}


-- | Generates an HTML @\<ms\>@ element with the given attributes and contents.
ms :: [Attribute ctx] -> [Html ctx] -> Html ctx
ms = ParentNode "<ms" "</ms>"
{-# INLINE ms #-}


-- | Generates an HTML @\<mspace\>@ element with the given attributes.
mspace :: [Attribute ctx] -> Html ctx
mspace = LeafNode "<mspace"
{-# INLINE mspace #-}


-- | Generates an HTML @\<msqrt\>@ element with the given attributes and contents.
msqrt :: [Attribute ctx] -> [Html ctx] -> Html ctx
msqrt = ParentNode "<msqrt" "</msqrt>"
{-# INLINE msqrt #-}


-- | Generates an HTML @\<mstyle\>@ element with the given attributes and contents.
mstyle :: [Attribute ctx] -> [Html ctx] -> Html ctx
mstyle = ParentNode "<mstyle" "</mstyle>"
{-# INLINE mstyle #-}


-- | Generates an HTML @\<msub\>@ element with the given attributes and contents.
msub :: [Attribute ctx] -> [Html ctx] -> Html ctx
msub = ParentNode "<msub" "</msub>"
{-# INLINE msub #-}


-- | Generates an HTML @\<msubsup\>@ element with the given attributes and contents.
msubsup :: [Attribute ctx] -> [Html ctx] -> Html ctx
msubsup = ParentNode "<msubsup" "</msubsup>"
{-# INLINE msubsup #-}


-- | Generates an HTML @\<msup\>@ element with the given attributes and contents.
msup :: [Attribute ctx] -> [Html ctx] -> Html ctx
msup = ParentNode "<msup" "</msup>"
{-# INLINE msup #-}


-- | Generates an HTML @\<mtable\>@ element with the given attributes and contents.
mtable :: [Attribute ctx] -> [Html ctx] -> Html ctx
mtable = ParentNode "<mtable" "</mtable>"
{-# INLINE mtable #-}


-- | Generates an HTML @\<mtd\>@ element with the given attributes and contents.
mtd :: [Attribute ctx] -> [Html ctx] -> Html ctx
mtd = ParentNode "<mtd" "</mtd>"
{-# INLINE mtd #-}


-- | Generates an HTML @\<mtext\>@ element with the given attributes and contents.
mtext :: [Attribute ctx] -> [Html ctx] -> Html ctx
mtext = ParentNode "<mtext" "</mtext>"
{-# INLINE mtext #-}


-- | Generates an HTML @\<mtr\>@ element with the given attributes and contents.
mtr :: [Attribute ctx] -> [Html ctx] -> Html ctx
mtr = ParentNode "<mtr" "</mtr>"
{-# INLINE mtr #-}


-- | Generates an HTML @\<munder\>@ element with the given attributes and contents.
munder :: [Attribute ctx] -> [Html ctx] -> Html ctx
munder = ParentNode "<munder" "</munder>"
{-# INLINE munder #-}


-- | Generates an HTML @\<munderover\>@ element with the given attributes and contents.
munderover :: [Attribute ctx] -> [Html ctx] -> Html ctx
munderover = ParentNode "<munderover" "</munderover>"
{-# INLINE munderover #-}


-- | Generates an HTML @\<semantics\>@ element with the given attributes and contents.
semantics :: [Attribute ctx] -> [Html ctx] -> Html ctx
semantics = ParentNode "<semantics" "</semantics>"
{-# INLINE semantics #-}


-- ATTRIBUTES


-- | Generates an HTML @accent@ attribute with the given value.
accent :: Builder -> Attribute ctx
accent = TextAttribute " accent=\""
{-# INLINE accent #-}


-- | Generates an HTML @accentunder@ attribute with the given value.
accentunder :: Builder -> Attribute ctx
accentunder = TextAttribute " accentunder=\""
{-# INLINE accentunder #-}


-- | Generates an HTML @actiontype@ attribute with the given value.
actiontype :: Builder -> Attribute ctx
actiontype = TextAttribute " actiontype=\""
{-# INLINE actiontype #-}


-- | Generates an HTML @alttext@ attribute with the given value.
alttext :: Builder -> Attribute ctx
alttext = TextAttribute " alttext=\""
{-# INLINE alttext #-}


-- | Generates an HTML @columnspan@ attribute with the given value.
columnspan :: Builder -> Attribute ctx
columnspan = TextAttribute " columnspan=\""
{-# INLINE columnspan #-}


-- | Generates an HTML @depth@ attribute with the given value.
depth :: Builder -> Attribute ctx
depth = TextAttribute " depth=\""
{-# INLINE depth #-}


-- | Generates an HTML @display@ attribute with the given value.
display :: Builder -> Attribute ctx
display = TextAttribute " display=\""
{-# INLINE display #-}


-- | Generates an HTML @displaystyle@ attribute with the given value.
displaystyle :: Builder -> Attribute ctx
displaystyle = TextAttribute " displaystyle=\""
{-# INLINE displaystyle #-}


-- | Generates an HTML @encoding@ attribute with the given value.
encoding :: Builder -> Attribute ctx
encoding = TextAttribute " encoding=\""
{-# INLINE encoding #-}


-- | Generates an HTML @fence@ attribute with the given value.
fence :: Builder -> Attribute ctx
fence = TextAttribute " fence=\""
{-# INLINE fence #-}


-- | Generates an HTML @largeop@ attribute with the given value.
largeop :: Builder -> Attribute ctx
largeop = TextAttribute " largeop=\""
{-# INLINE largeop #-}


-- | Generates an HTML @linethickness@ attribute with the given value.
linethickness :: Builder -> Attribute ctx
linethickness = TextAttribute " linethickness=\""
{-# INLINE linethickness #-}


-- | Generates an HTML @lspace@ attribute with the given value.
lspace :: Builder -> Attribute ctx
lspace = TextAttribute " lspace=\""
{-# INLINE lspace #-}


-- | Generates an HTML @mathbackground@ attribute with the given value.
mathbackground :: Builder -> Attribute ctx
mathbackground = TextAttribute " mathbackground=\""
{-# INLINE mathbackground #-}


-- | Generates an HTML @mathcolor@ attribute with the given value.
mathcolor :: Builder -> Attribute ctx
mathcolor = TextAttribute " mathcolor=\""
{-# INLINE mathcolor #-}


-- | Generates an HTML @mathsize@ attribute with the given value.
mathsize :: Builder -> Attribute ctx
mathsize = TextAttribute " mathsize=\""
{-# INLINE mathsize #-}


-- | Generates an HTML @maxsize@ attribute with the given value.
maxsize :: Builder -> Attribute ctx
maxsize = TextAttribute " maxsize=\""
{-# INLINE maxsize #-}


-- | Generates an HTML @minsize@ attribute with the given value.
minsize :: Builder -> Attribute ctx
minsize = TextAttribute " minsize=\""
{-# INLINE minsize #-}


-- | Generates an HTML @movablelimits@ attribute with the given value.
movablelimits :: Builder -> Attribute ctx
movablelimits = TextAttribute " movablelimits=\""
{-# INLINE movablelimits #-}


-- | Generates an HTML @rowspan@ attribute with the given value.
rowspan :: Builder -> Attribute ctx
rowspan = TextAttribute " rowspan=\""
{-# INLINE rowspan #-}


-- | Generates an HTML @rspace@ attribute with the given value.
rspace :: Builder -> Attribute ctx
rspace = TextAttribute " rspace=\""
{-# INLINE rspace #-}


-- | Generates an HTML @scriptlevel@ attribute with the given value.
scriptlevel :: Builder -> Attribute ctx
scriptlevel = TextAttribute " scriptlevel=\""
{-# INLINE scriptlevel #-}


-- | Generates an HTML @selection@ attribute with the given value.
selection :: Builder -> Attribute ctx
selection = TextAttribute " selection=\""
{-# INLINE selection #-}


-- | Generates an HTML @separator@ attribute with the given value.
separator :: Builder -> Attribute ctx
separator = TextAttribute " separator=\""
{-# INLINE separator #-}


-- | Generates an HTML @stretchy@ attribute with the given value.
stretchy :: Builder -> Attribute ctx
stretchy = TextAttribute " stretchy=\""
{-# INLINE stretchy #-}


-- | Generates an HTML @symmetric@ attribute with the given value.
symmetric :: Builder -> Attribute ctx
symmetric = TextAttribute " symmetric=\""
{-# INLINE symmetric #-}


-- | Generates an HTML @voffset@ attribute with the given value.
voffset :: Builder -> Attribute ctx
voffset = TextAttribute " voffset=\""
{-# INLINE voffset #-}
