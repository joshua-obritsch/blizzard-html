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
import Html                   (Html(..))
import Html.Attributes        (Attribute(..))


-- ELEMENTS


-- | Generates an HTML @\<annotation\>@ element with the given attributes and contents.
annotation :: [Attribute] -> [Html] -> Html
annotation = ParentNode "<annotation" "</annotation>"
{-# INLINE annotation #-}


-- | Generates an HTML @\<annotation-xml\>@ element with the given attributes and contents.
annotationXml :: [Attribute] -> [Html] -> Html
annotationXml = ParentNode "<annotation-xml" "</annotation-xml>"
{-# INLINE annotationXml #-}


-- | Generates an HTML @\<maction\>@ element with the given attributes and contents.
maction :: [Attribute] -> [Html] -> Html
maction = ParentNode "<maction" "</maction>"
{-# INLINE maction #-}


-- | Generates an HTML @\<math\>@ element with the given attributes and contents.
math :: [Attribute] -> [Html] -> Html
math = ParentNode "<math" "</math>"
{-# INLINE math #-}


-- | Generates an HTML @\<merror\>@ element with the given attributes and contents.
merror :: [Attribute] -> [Html] -> Html
merror = ParentNode "<merror" "</merror>"
{-# INLINE merror #-}


-- | Generates an HTML @\<mfrac\>@ element with the given attributes and contents.
mfrac :: [Attribute] -> [Html] -> Html
mfrac = ParentNode "<mfrac" "</mfrac>"
{-# INLINE mfrac #-}


-- | Generates an HTML @\<mi\>@ element with the given attributes and contents.
mi :: [Attribute] -> [Html] -> Html
mi = ParentNode "<mi" "</mi>"
{-# INLINE mi #-}


-- | Generates an HTML @\<mmultiscripts\>@ element with the given attributes and contents.
mmultiscripts :: [Attribute] -> [Html] -> Html
mmultiscripts = ParentNode "<mmultiscripts" "</mmultiscripts>"
{-# INLINE mmultiscripts #-}


-- | Generates an HTML @\<mn\>@ element with the given attributes and contents.
mn :: [Attribute] -> [Html] -> Html
mn = ParentNode "<mn" "</mn>"
{-# INLINE mn #-}


-- | Generates an HTML @\<mo\>@ element with the given attributes and contents.
mo :: [Attribute] -> [Html] -> Html
mo = ParentNode "<mo" "</mo>"
{-# INLINE mo #-}


-- | Generates an HTML @\<mover\>@ element with the given attributes and contents.
mover :: [Attribute] -> [Html] -> Html
mover = ParentNode "<mover" "</mover>"
{-# INLINE mover #-}


-- | Generates an HTML @\<mpadded\>@ element with the given attributes and contents.
mpadded :: [Attribute] -> [Html] -> Html
mpadded = ParentNode "<mpadded" "</mpadded>"
{-# INLINE mpadded #-}


-- | Generates an HTML @\<mphantom\>@ element with the given attributes and contents.
mphantom :: [Attribute] -> [Html] -> Html
mphantom = ParentNode "<mphantom" "</mphantom>"
{-# INLINE mphantom #-}


-- | Generates an HTML @\<mprescripts\>@ element with the given attributes.
mprescripts :: [Attribute] -> Html
mprescripts = LeafNode "<mprescripts"
{-# INLINE mprescripts #-}


-- | Generates an HTML @\<mroot\>@ element with the given attributes and contents.
mroot :: [Attribute] -> [Html] -> Html
mroot = ParentNode "<mroot" "</mroot>"
{-# INLINE mroot #-}


-- | Generates an HTML @\<mrow\>@ element with the given attributes and contents.
mrow :: [Attribute] -> [Html] -> Html
mrow = ParentNode "<mrow" "</mrow>"
{-# INLINE mrow #-}


-- | Generates an HTML @\<ms\>@ element with the given attributes and contents.
ms :: [Attribute] -> [Html] -> Html
ms = ParentNode "<ms" "</ms>"
{-# INLINE ms #-}


-- | Generates an HTML @\<mspace\>@ element with the given attributes.
mspace :: [Attribute] -> Html
mspace = LeafNode "<mspace"
{-# INLINE mspace #-}


-- | Generates an HTML @\<msqrt\>@ element with the given attributes and contents.
msqrt :: [Attribute] -> [Html] -> Html
msqrt = ParentNode "<msqrt" "</msqrt>"
{-# INLINE msqrt #-}


-- | Generates an HTML @\<mstyle\>@ element with the given attributes and contents.
mstyle :: [Attribute] -> [Html] -> Html
mstyle = ParentNode "<mstyle" "</mstyle>"
{-# INLINE mstyle #-}


-- | Generates an HTML @\<msub\>@ element with the given attributes and contents.
msub :: [Attribute] -> [Html] -> Html
msub = ParentNode "<msub" "</msub>"
{-# INLINE msub #-}


-- | Generates an HTML @\<msubsup\>@ element with the given attributes and contents.
msubsup :: [Attribute] -> [Html] -> Html
msubsup = ParentNode "<msubsup" "</msubsup>"
{-# INLINE msubsup #-}


-- | Generates an HTML @\<msup\>@ element with the given attributes and contents.
msup :: [Attribute] -> [Html] -> Html
msup = ParentNode "<msup" "</msup>"
{-# INLINE msup #-}


-- | Generates an HTML @\<mtable\>@ element with the given attributes and contents.
mtable :: [Attribute] -> [Html] -> Html
mtable = ParentNode "<mtable" "</mtable>"
{-# INLINE mtable #-}


-- | Generates an HTML @\<mtd\>@ element with the given attributes and contents.
mtd :: [Attribute] -> [Html] -> Html
mtd = ParentNode "<mtd" "</mtd>"
{-# INLINE mtd #-}


-- | Generates an HTML @\<mtext\>@ element with the given attributes and contents.
mtext :: [Attribute] -> [Html] -> Html
mtext = ParentNode "<mtext" "</mtext>"
{-# INLINE mtext #-}


-- | Generates an HTML @\<mtr\>@ element with the given attributes and contents.
mtr :: [Attribute] -> [Html] -> Html
mtr = ParentNode "<mtr" "</mtr>"
{-# INLINE mtr #-}


-- | Generates an HTML @\<munder\>@ element with the given attributes and contents.
munder :: [Attribute] -> [Html] -> Html
munder = ParentNode "<munder" "</munder>"
{-# INLINE munder #-}


-- | Generates an HTML @\<munderover\>@ element with the given attributes and contents.
munderover :: [Attribute] -> [Html] -> Html
munderover = ParentNode "<munderover" "</munderover>"
{-# INLINE munderover #-}


-- | Generates an HTML @\<semantics\>@ element with the given attributes and contents.
semantics :: [Attribute] -> [Html] -> Html
semantics = ParentNode "<semantics" "</semantics>"
{-# INLINE semantics #-}


-- ATTRIBUTES


-- | Generates an HTML @accent@ attribute with the given value.
accent :: Builder -> Attribute
accent = TextAttribute " accent=\""
{-# INLINE accent #-}


-- | Generates an HTML @accentunder@ attribute with the given value.
accentunder :: Builder -> Attribute
accentunder = TextAttribute " accentunder=\""
{-# INLINE accentunder #-}


-- | Generates an HTML @actiontype@ attribute with the given value.
actiontype :: Builder -> Attribute
actiontype = TextAttribute " actiontype=\""
{-# INLINE actiontype #-}


-- | Generates an HTML @alttext@ attribute with the given value.
alttext :: Builder -> Attribute
alttext = TextAttribute " alttext=\""
{-# INLINE alttext #-}


-- | Generates an HTML @columnspan@ attribute with the given value.
columnspan :: Builder -> Attribute
columnspan = TextAttribute " columnspan=\""
{-# INLINE columnspan #-}


-- | Generates an HTML @depth@ attribute with the given value.
depth :: Builder -> Attribute
depth = TextAttribute " depth=\""
{-# INLINE depth #-}


-- | Generates an HTML @display@ attribute with the given value.
display :: Builder -> Attribute
display = TextAttribute " display=\""
{-# INLINE display #-}


-- | Generates an HTML @displaystyle@ attribute with the given value.
displaystyle :: Builder -> Attribute
displaystyle = TextAttribute " displaystyle=\""
{-# INLINE displaystyle #-}


-- | Generates an HTML @encoding@ attribute with the given value.
encoding :: Builder -> Attribute
encoding = TextAttribute " encoding=\""
{-# INLINE encoding #-}


-- | Generates an HTML @fence@ attribute with the given value.
fence :: Builder -> Attribute
fence = TextAttribute " fence=\""
{-# INLINE fence #-}


-- | Generates an HTML @largeop@ attribute with the given value.
largeop :: Builder -> Attribute
largeop = TextAttribute " largeop=\""
{-# INLINE largeop #-}


-- | Generates an HTML @linethickness@ attribute with the given value.
linethickness :: Builder -> Attribute
linethickness = TextAttribute " linethickness=\""
{-# INLINE linethickness #-}


-- | Generates an HTML @lspace@ attribute with the given value.
lspace :: Builder -> Attribute
lspace = TextAttribute " lspace=\""
{-# INLINE lspace #-}


-- | Generates an HTML @mathbackground@ attribute with the given value.
mathbackground :: Builder -> Attribute
mathbackground = TextAttribute " mathbackground=\""
{-# INLINE mathbackground #-}


-- | Generates an HTML @mathcolor@ attribute with the given value.
mathcolor :: Builder -> Attribute
mathcolor = TextAttribute " mathcolor=\""
{-# INLINE mathcolor #-}


-- | Generates an HTML @mathsize@ attribute with the given value.
mathsize :: Builder -> Attribute
mathsize = TextAttribute " mathsize=\""
{-# INLINE mathsize #-}


-- | Generates an HTML @maxsize@ attribute with the given value.
maxsize :: Builder -> Attribute
maxsize = TextAttribute " maxsize=\""
{-# INLINE maxsize #-}


-- | Generates an HTML @minsize@ attribute with the given value.
minsize :: Builder -> Attribute
minsize = TextAttribute " minsize=\""
{-# INLINE minsize #-}


-- | Generates an HTML @movablelimits@ attribute with the given value.
movablelimits :: Builder -> Attribute
movablelimits = TextAttribute " movablelimits=\""
{-# INLINE movablelimits #-}


-- | Generates an HTML @rowspan@ attribute with the given value.
rowspan :: Builder -> Attribute
rowspan = TextAttribute " rowspan=\""
{-# INLINE rowspan #-}


-- | Generates an HTML @rspace@ attribute with the given value.
rspace :: Builder -> Attribute
rspace = TextAttribute " rspace=\""
{-# INLINE rspace #-}


-- | Generates an HTML @scriptlevel@ attribute with the given value.
scriptlevel :: Builder -> Attribute
scriptlevel = TextAttribute " scriptlevel=\""
{-# INLINE scriptlevel #-}


-- | Generates an HTML @selection@ attribute with the given value.
selection :: Builder -> Attribute
selection = TextAttribute " selection=\""
{-# INLINE selection #-}


-- | Generates an HTML @separator@ attribute with the given value.
separator :: Builder -> Attribute
separator = TextAttribute " separator=\""
{-# INLINE separator #-}


-- | Generates an HTML @stretchy@ attribute with the given value.
stretchy :: Builder -> Attribute
stretchy = TextAttribute " stretchy=\""
{-# INLINE stretchy #-}


-- | Generates an HTML @symmetric@ attribute with the given value.
symmetric :: Builder -> Attribute
symmetric = TextAttribute " symmetric=\""
{-# INLINE symmetric #-}


-- | Generates an HTML @voffset@ attribute with the given value.
voffset :: Builder -> Attribute
voffset = TextAttribute " voffset=\""
{-# INLINE voffset #-}
