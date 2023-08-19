{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Css.Colors
    ( hex
    , pct
    , rgb
    , rgba
    , hsl
    , hsla
    , aliceblue
    , antiquewhite
    , aqua
    , aquamarine
    , azure
    , beige
    , bisque
    , black
    , blanchedalmond
    , blue
    , blueviolet
    , brown
    , burlywood
    , cadetblue
    , chartreuse
    , chocolate
    , coral
    , cornflowerblue
    , cornsilk
    , crimson
    , cyan
    , darkblue
    , darkcyan
    , darkgoldenrod
    , darkgray
    , darkgreen
    , darkgrey
    , darkkhaki
    , darkmagenta
    , darkolivegreen
    , darkorange
    , darkorchid
    , darkred
    , darksalmon
    , darkseagreen
    , darkslateblue
    , darkslategray
    , darkslategrey
    , darkturquoise
    , darkviolet
    , deeppink
    , deepskyblue
    , dimgray
    , dimgrey
    , dodgerblue
    , firebrick
    , floralwhite
    , forestgreen
    , fuchsia
    , gainsboro
    , ghostwhite
    , gold
    , goldenrod
    , gray
    , green
    , greenyellow
    , grey
    , honeydew
    , hotpink
    , indianred
    , indigo
    , ivory
    , khaki
    , lavender
    , lavenderblush
    , lawngreen
    , lemonchiffon
    , lightblue
    , lightcoral
    , lightcyan
    , lightgoldenrodyellow
    , lightgray
    , lightgreen
    , lightgrey
    , lightpink
    , lightsalmon
    , lightseagreen
    , lightskyblue
    , lightslategray
    , lightslategrey
    , lightsteelblue
    , lightyellow
    , lime
    , limegreen
    , linen
    , magenta
    , maroon
    , mediumaquamarine
    , mediumblue
    , mediumorchid
    , mediumpurple
    , mediumseagreen
    , mediumslateblue
    , mediumspringgreen
    , mediumturquoise
    , mediumvioletred
    , midnightblue
    , mintcream
    , mistyrose
    , moccasin
    , navajowhite
    , navy
    , oldlace
    , olive
    , olivedrab
    , orange
    , orangered
    , orchid
    , palegoldenrod
    , palegreen
    , paleturquoise
    , palevioletred
    , papayawhip
    , peachpuff
    , peru
    , pink
    , plum
    , powderblue
    , purple
    , red
    , rosybrown
    , royalblue
    , saddlebrown
    , salmon
    , sandybrown
    , seagreen
    , seashell
    , sienna
    , silver
    , skyblue
    , slateblue
    , slategray
    , slategrey
    , snow
    , springgreen
    , steelblue
    , tan
    , teal
    , thistle
    , tomato
    , turquoise
    , violet
    , wheat
    , white
    , whitesmoke
    , yellow
    , yellowgreen
    , currentcolor
    ) where


import Prelude (Int, mconcat)

import Data.Text.Lazy.Builder (Builder)
import Internal (Buildable(..))


hex :: Int -> Builder
hex a = mconcat [ "#", build a ]


pct :: Int -> Builder
pct a = mconcat [ build a, "%" ]


rgb :: (Buildable a, Buildable b, Buildable c) => a -> b -> c -> Builder
rgb r g b = mconcat [ "rgb(", build r, ",", build g, ",", build b, ")" ]


rgba :: (Buildable a, Buildable b, Buildable c, Buildable d) => a -> b -> c -> d -> Builder
rgba r g b a = mconcat [ "rgba(", build r, ",", build g, ",", build b, ",", build a, ")" ]


hsl :: (Buildable a, Buildable b, Buildable c) => a -> b -> c -> Builder
hsl h s l = mconcat [ "hsl(", build h, ",", build s, ",", build l, ")" ]


hsla :: (Buildable a, Buildable b, Buildable c, Buildable d) => a -> b -> c -> d -> Builder
hsla h s l a = mconcat [ "hsla(", build h, ",", build s, ",", build l, ",", build a, ")" ]


aliceblue :: Builder
aliceblue = "aliceblue"


antiquewhite :: Builder
antiquewhite = "antiquewhite"


aqua :: Builder
aqua = "aqua"


aquamarine :: Builder
aquamarine = "aquamarine"


azure :: Builder
azure = "azure"


beige :: Builder
beige = "beige"


bisque :: Builder
bisque = "bisque"


black :: Builder
black = "black"


blanchedalmond :: Builder
blanchedalmond = "blanchedalmond"


blue :: Builder
blue = "blue"


blueviolet :: Builder
blueviolet = "blueviolet"


brown :: Builder
brown = "brown"


burlywood :: Builder
burlywood = "burlywood"


cadetblue :: Builder
cadetblue = "cadetblue"


chartreuse :: Builder
chartreuse = "chartreuse"


chocolate :: Builder
chocolate = "chocolate"


coral :: Builder
coral = "coral"


cornflowerblue :: Builder
cornflowerblue = "cornflowerblue"


cornsilk :: Builder
cornsilk = "cornsilk"


crimson :: Builder
crimson = "crimson"


cyan :: Builder
cyan = "cyan"


darkblue :: Builder
darkblue = "darkblue"


darkcyan :: Builder
darkcyan = "darkcyan"


darkgoldenrod :: Builder
darkgoldenrod = "darkgoldenrod"


darkgray :: Builder
darkgray = "darkgray"


darkgreen :: Builder
darkgreen = "darkgreen"


darkgrey :: Builder
darkgrey = "darkgrey"


darkkhaki :: Builder
darkkhaki = "darkkhaki"


darkmagenta :: Builder
darkmagenta = "darkmagenta"


darkolivegreen :: Builder
darkolivegreen = "darkolivegreen"


darkorange :: Builder
darkorange = "darkorange"


darkorchid :: Builder
darkorchid = "darkorchid"


darkred :: Builder
darkred = "darkred"


darksalmon :: Builder
darksalmon = "darksalmon"


darkseagreen :: Builder
darkseagreen = "darkseagreen"


darkslateblue :: Builder
darkslateblue = "darkslateblue"


darkslategray :: Builder
darkslategray = "darkslategray"


darkslategrey :: Builder
darkslategrey = "darkslategrey"


darkturquoise :: Builder
darkturquoise = "darkturquoise"


darkviolet :: Builder
darkviolet = "darkviolet"


deeppink :: Builder
deeppink = "deeppink"


deepskyblue :: Builder
deepskyblue = "deepskyblue"


dimgray :: Builder
dimgray = "dimgray"


dimgrey :: Builder
dimgrey = "dimgrey"


dodgerblue :: Builder
dodgerblue = "dodgerblue"


firebrick :: Builder
firebrick = "firebrick"


floralwhite :: Builder
floralwhite = "floralwhite"


forestgreen :: Builder
forestgreen = "forestgreen"


fuchsia :: Builder
fuchsia = "fuchsia"


gainsboro :: Builder
gainsboro = "gainsboro"


ghostwhite :: Builder
ghostwhite = "ghostwhite"


gold :: Builder
gold = "gold"


goldenrod :: Builder
goldenrod = "goldenrod"


gray :: Builder
gray = "gray"


green :: Builder
green = "green"


greenyellow :: Builder
greenyellow = "greenyellow"


grey :: Builder
grey = "grey"


honeydew :: Builder
honeydew = "honeydew"


hotpink :: Builder
hotpink = "hotpink"


indianred :: Builder
indianred = "indianred"


indigo :: Builder
indigo = "indigo"


ivory :: Builder
ivory = "ivory"


khaki :: Builder
khaki = "khaki"


lavender :: Builder
lavender = "lavender"


lavenderblush :: Builder
lavenderblush = "lavenderblush"


lawngreen :: Builder
lawngreen = "lawngreen"


lemonchiffon :: Builder
lemonchiffon = "lemonchiffon"


lightblue :: Builder
lightblue = "lightblue"


lightcoral :: Builder
lightcoral = "lightcoral"


lightcyan :: Builder
lightcyan = "lightcyan"


lightgoldenrodyellow :: Builder
lightgoldenrodyellow = "lightgoldenrodyellow"


lightgray :: Builder
lightgray = "lightgray"


lightgreen :: Builder
lightgreen = "lightgreen"


lightgrey :: Builder
lightgrey = "lightgrey"


lightpink :: Builder
lightpink = "lightpink"


lightsalmon :: Builder
lightsalmon = "lightsalmon"


lightseagreen :: Builder
lightseagreen = "lightseagreen"


lightskyblue :: Builder
lightskyblue = "lightskyblue"


lightslategray :: Builder
lightslategray = "lightslategray"


lightslategrey :: Builder
lightslategrey = "lightslategrey"


lightsteelblue :: Builder
lightsteelblue = "lightsteelblue"


lightyellow :: Builder
lightyellow = "lightyellow"


lime :: Builder
lime = "lime"


limegreen :: Builder
limegreen = "limegreen"


linen :: Builder
linen = "linen"


magenta :: Builder
magenta = "magenta"


maroon :: Builder
maroon = "maroon"


mediumaquamarine :: Builder
mediumaquamarine = "mediumaquamarine"


mediumblue :: Builder
mediumblue = "mediumblue"


mediumorchid :: Builder
mediumorchid = "mediumorchid"


mediumpurple :: Builder
mediumpurple = "mediumpurple"


mediumseagreen :: Builder
mediumseagreen = "mediumseagreen"


mediumslateblue :: Builder
mediumslateblue = "mediumslateblue"


mediumspringgreen :: Builder
mediumspringgreen = "mediumspringgreen"


mediumturquoise :: Builder
mediumturquoise = "mediumturquoise"


mediumvioletred :: Builder
mediumvioletred = "mediumvioletred"


midnightblue :: Builder
midnightblue = "midnightblue"


mintcream :: Builder
mintcream = "mintcream"


mistyrose :: Builder
mistyrose = "mistyrose"


moccasin :: Builder
moccasin = "moccasin"


navajowhite :: Builder
navajowhite = "navajowhite"


navy :: Builder
navy = "navy"


oldlace :: Builder
oldlace = "oldlace"


olive :: Builder
olive = "olive"


olivedrab :: Builder
olivedrab = "olivedrab"


orange :: Builder
orange = "orange"


orangered :: Builder
orangered = "orangered"


orchid :: Builder
orchid = "orchid"


palegoldenrod :: Builder
palegoldenrod = "palegoldenrod"


palegreen :: Builder
palegreen = "palegreen"


paleturquoise :: Builder
paleturquoise = "paleturquoise"


palevioletred :: Builder
palevioletred = "palevioletred"


papayawhip :: Builder
papayawhip = "papayawhip"


peachpuff :: Builder
peachpuff = "peachpuff"


peru :: Builder
peru = "peru"


pink :: Builder
pink = "pink"


plum :: Builder
plum = "plum"


powderblue :: Builder
powderblue = "powderblue"


purple :: Builder
purple = "purple"


red :: Builder
red = "red"


rosybrown :: Builder
rosybrown = "rosybrown"


royalblue :: Builder
royalblue = "royalblue"


saddlebrown :: Builder
saddlebrown = "saddlebrown"


salmon :: Builder
salmon = "salmon"


sandybrown :: Builder
sandybrown = "sandybrown"


seagreen :: Builder
seagreen = "seagreen"


seashell :: Builder
seashell = "seashell"


sienna :: Builder
sienna = "sienna"


silver :: Builder
silver = "silver"


skyblue :: Builder
skyblue = "skyblue"


slateblue :: Builder
slateblue = "slateblue"


slategray :: Builder
slategray = "slategray"


slategrey :: Builder
slategrey = "slategrey"


snow :: Builder
snow = "snow"


springgreen :: Builder
springgreen = "springgreen"


steelblue :: Builder
steelblue = "steelblue"


tan :: Builder
tan = "tan"


teal :: Builder
teal = "teal"


thistle :: Builder
thistle = "thistle"


tomato :: Builder
tomato = "tomato"


turquoise :: Builder
turquoise = "turquoise"


violet :: Builder
violet = "violet"


wheat :: Builder
wheat = "wheat"


white :: Builder
white = "white"


whitesmoke :: Builder
whitesmoke = "whitesmoke"


yellow :: Builder
yellow = "yellow"


yellowgreen :: Builder
yellowgreen = "yellowgreen"


currentcolor :: Builder
currentcolor = "currentcolor"
