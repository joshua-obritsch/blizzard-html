module Blizzard.Css.FontFace
    ( FontFaceFormat(..)
    , FontFaceSrc(..)
    , fontFaceSrc
    ) where


import Blizzard.Internal (Attribute(..))
import Clay.FontFace
    ( FontFaceFormat(..)
    , FontFaceSrc(..)
    )

import qualified Clay.FontFace as F


fontFaceSrc :: [FontFaceSrc] -> Attribute
fontFaceSrc a = AttrCss $ F.fontFaceSrc a
