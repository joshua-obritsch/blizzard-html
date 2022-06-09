{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module Blizzard.Css.Size
    ( Size
    , Length
    , Percentage
    ) where


import Data.Text (Text)

import Blizzard.Css.Common
    ( Auto(..)
    , Inherit(..)
    , None(..)
    , Normal(..)
    , Other(..)
    )
import Blizzard.Css.Property (Val, Value(..), value)


data Length


data Percentage


data Combination


data Size a
    =             RawSize  Text
    | forall b c. AddSize (Size b) (Size c)
    | forall b c. SubSize (Size b) (Size c)
    |             MulSize  Double  (Size a)
    |             DivSize  Double  (Size a)
    |             Other    Value


deriving instance Show (Size a)


sizeToText :: Size a -> Text
sizeToText (RawSize a  ) = a
sizeToText (AddSize a b) = mconcat ["(", sizeToText    a, " + ", sizeToText    b, ")"]
sizeToText (SubSize a b) = mconcat ["(", sizeToText    a, " - ", sizeToText    b, ")"]
sizeToText (MulSize a b) = mconcat ["(", cssDoubleText a, " * ", sizeToText    b, ")"]
sizeToText (DivSize a b) = mconcat ["(", sizeToText    b, " / ", cssDoubleText a, ")"]
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


cm, inches, mm, px, pt, pc :: Double -> Size Length
