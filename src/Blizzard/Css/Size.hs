{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Blizzard.Css.Size
    ( Size
    , Length, Percentage, Calculation
    , zero, int, double
    , cm, in_, mm, pc, pt, px
    , em, ex, fr, rem, vh, vmax, vmin, vw
    , pct
    , available, fitContent, maxContent, minContent
    , plus, minus
    , Angle
    , Deg, Rad, Grad, Turn
    , deg, rad, grad, turn
    ) where


import Prelude hiding (rem)

import Data.Text (Text)

import Blizzard.Css.Common
    ( Auto(..)
    , Inherit(..)
    , None(..)
    , Normal(..)
    , Other(..)
    )
import Blizzard.Css.Property (Val, Value(..), fromDouble, value)


data Size a
    =             RawSize  Text
    | forall b c. AddSize (Size b) (Size c)
    | forall b c. SubSize (Size b) (Size c)
    |             MulSize  Double  (Size a)
    |             DivSize  Double  (Size a)
    |             Other    Value


deriving instance Show (Size a)


data Length


data Percentage


data Calculation


sizeToText :: Size a -> Text
sizeToText (RawSize a  ) = a
sizeToText (AddSize a b) = mconcat ["(", sizeToText a, " + ", sizeToText b, ")"]
sizeToText (SubSize a b) = mconcat ["(", sizeToText a, " - ", sizeToText b, ")"]
sizeToText (MulSize a b) = mconcat ["(", fromDouble a, " * ", sizeToText b, ")"]
sizeToText (DivSize a b) = mconcat ["(", sizeToText b, " / ", fromDouble a, ")"]
sizeToText (Other   a  ) = unvalue a


instance Val (Size a) where
    value (RawSize a) = value a
    value (Other   a) = a
    value          a  = Value $ "calc" <> sizeToText a


instance Auto    (Size a) where auto    = Other "auto"
instance Inherit (Size a) where inherit = Other "inherit"
instance None    (Size a) where none    = Other "none"
instance Normal  (Size a) where normal  = Other "normal"
instance Other   (Size a) where other   = Other


zero :: Size a
zero = RawSize "0"


int :: Int -> Size a
int = RawSize . unvalue . value


double :: Double -> Size a
double = RawSize . unvalue . value


cm, in_, mm, pc, pt, px :: Double -> Size Length

cm  = RawSize . (<> "cm") . fromDouble
in_ = RawSize . (<> "in") . fromDouble
mm  = RawSize . (<> "mm") . fromDouble
pc  = RawSize . (<> "pc") . fromDouble
pt  = RawSize . (<> "pt") . fromDouble
px  = RawSize . (<> "px") . fromDouble


em, ex, fr, rem, vh, vmax, vmin, vw :: Double -> Size Length

em   = RawSize . (<> "em"  ) . fromDouble
ex   = RawSize . (<> "ex"  ) . fromDouble
fr   = RawSize . (<> "fr"  ) . fromDouble
rem  = RawSize . (<> "rem" ) . fromDouble
vh   = RawSize . (<> "vh"  ) . fromDouble
vmax = RawSize . (<> "vmax") . fromDouble
vmin = RawSize . (<> "vmin") . fromDouble
vw   = RawSize . (<> "vw"  ) . fromDouble


pct :: Double -> Size Percentage
pct = RawSize . (<> "%") . fromDouble


available :: Size Length
available = RawSize "available"


fitContent :: Size Length
fitContent = RawSize "fit-content"


maxContent :: Size Length
maxContent = RawSize "max-content"


minContent :: Size Length
minContent = RawSize "min-content"


instance Num (Size Length) where
    fromInteger = px . fromInteger
    (+)         = error "plus not implemented for Size"
    (*)         = error "times not implemented for Size"
    abs         = error "abs not implemented for Size"
    signum      = error "signum not implemented for Size"
    negate      = error "negate not implemented for Size"


instance Fractional (Size Length) where
    fromRational = px . fromRational
    recip        = error "recip not implemented for Size"


instance Num (Size Percentage) where
    fromInteger = pct . fromInteger
    (+)         = error "plus not implemented for Size"
    (*)         = error "times not implemented for Size"
    abs         = error "abs not implemented for Size"
    signum      = error "signum not implemented for Size"
    negate      = error "negate not implemented for Size"


instance Fractional (Size Percentage) where
    fromRational = pct . fromRational
    recip        = error "recip not implemented for Size"


type family SizeCalculation sa sb where
    SizeCalculation Percentage Percentage = Percentage
    SizeCalculation Length     Length     = Length
    SizeCalculation a          b          = Calculation


plus :: Size a -> Size b -> Size (SizeCalculation a b)
plus = AddSize


minus :: Size a -> Size b -> Size (SizeCalculation a b)
minus = SubSize


newtype Angle a = Angle Value
    deriving (Auto, Inherit, Other, Val)


data Deg
data Grad
data Rad
data Turn


deg :: Double -> Angle Deg
deg = Angle . (<> "deg") . value


grad :: Double -> Angle Grad
grad = Angle . (<> "grad") . value


rad :: Double -> Angle Rad
rad = Angle . (<> "rad") . value


turn :: Double -> Angle Turn
turn = Angle . (<> "turn") . value


instance Num (Angle Deg) where
    fromInteger = deg . fromInteger
    (+)         = error "plus not implemented for Angle"
    (*)         = error "times not implemented for Angle"
    abs         = error "abs not implemented for Angle"
    signum      = error "signum not implemented for Angle"
    negate      = error "negate not implemented for Angle"


instance Fractional (Angle Deg) where
    fromRational = deg . fromRational
    recip        = error "recip not implemented for Angle"


instance Num (Angle Grad) where
    fromInteger = grad . fromInteger
    (+)         = error "plus not implemented for Angle"
    (*)         = error "times not implemented for Angle"
    abs         = error "abs not implemented for Angle"
    signum      = error "signum not implemented for Angle"
    negate      = error "negate not implemented for Angle"


instance Fractional (Angle Grad) where
    fromRational = grad . fromRational
    recip        = error "recip not implemented for Angle"


instance Num (Angle Rad) where
    fromInteger = rad . fromInteger
    (+)         = error "plus not implemented for Angle"
    (*)         = error "times not implemented for Angle"
    abs         = error "abs not implemented for Angle"
    signum      = error "signum not implemented for Angle"
    negate      = error "negate not implemented for Angle"


instance Fractional (Angle Rad) where
    fromRational = rad . fromRational
    recip        = error "recip not implemented for Angle"


instance Num (Angle Turn) where
    fromInteger = turn . fromInteger
    (+)         = error "plus not implemented for Angle"
    (*)         = error "times not implemented for Angle"
    abs         = error "abs not implemented for Angle"
    signum      = error "signum not implemented for Angle"
    negate      = error "negate not implemented for Angle"


instance Fractional (Angle Turn) where
    fromRational = turn . fromRational
    recip        = error "recip not implemented for Angle"
