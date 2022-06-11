{-# LANGUAGE OverloadedStrings #-}

module Blizzard.Css.Common
    ( All(..)
    , Auto(..)
    , Baseline(..)
    , Center(..)
    , Hidden(..)
    , Inherit(..)
    , Initial(..)
    , None(..)
    , Normal(..)
    , Other(..)
    , Unset(..)
    , Visible(..)
    , call
    ) where


import Data.String (IsString)

import Blizzard.Css.Property (Value)


class All      a where all      :: a
class Auto     a where auto     :: a
class Baseline a where baseline :: a
class Center   a where center   :: a
class Hidden   a where hidden   :: a
class Inherit  a where inherit  :: a
class Initial  a where initial  :: a
class None     a where none     :: a
class Normal   a where normal   :: a
class Other    a where other    :: Value -> a
class Unset    a where unset    :: a
class Visible  a where visible  :: a


instance All      Value where all      = "all"
instance Auto     Value where auto     = "auto"
instance Baseline Value where baseline = "baseline"
instance Center   Value where center   = "center"
instance Hidden   Value where hidden   = "hidden"
instance Inherit  Value where inherit  = "inherit"
instance Initial  Value where initial  = "initial"
instance None     Value where none     = "none"
instance Normal   Value where normal   = "normal"
instance Other    Value where other    = id
instance Unset    Value where unset    = "unset"
instance Visible  Value where visible  = "visible"


call :: (IsString a, Monoid a) => a -> a -> a
call a b = a <> "(" <> b <> ")"
